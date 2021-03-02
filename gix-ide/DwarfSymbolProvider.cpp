#include "DwarfSymbolProvider.h"

#include <QDir>
#include <stdio.h>
#include <fcntl.h>

#if defined(_WIN32)
#include <io.h>
#else
#include <fcntl.h>
#include <unistd.h>

#ifndef O_BINARY
#define O_BINARY  0
#define O_TEXT    0
#endif

#endif

#include "PathUtils.h"
#include <SymbolBufferReader.h>

static bool get_module_base_offset(Dwarf_Debug dbg, uint64_t *offset);
static bool list_funcs_in_file(Dwarf_Debug dbg, QMap<QString, void *> &funcinfo);
static bool locate_cobol_modules(module_dwarf_private_data *md, QMap<QString, void *> &funcinfo);
static Dwarf_Die find_top_level_die(const QString &target_die_name, Dwarf_Debug dbg);
static int get_form_values(Dwarf_Debug dbg, Dwarf_Attribute attrib, Dwarf_Half *theform, Dwarf_Half *directform, Dwarf_Error *err);

extern void ListDLLFunctions(QString sADllName, QList<QString> &slListOfDllFunctions);

QMap<QString, struct module_dwarf_private_data *> DwarfSymbolProvider::module_data;

struct module_dwarf_private_data
{
	QString module_name;
	Dwarf_Debug dbg;
	int fd;
	uint64_t base_offset;
};

struct lineinfo_entry
{
	QString filename;
	int lineno = 0;
	void *lineaddr = 0;
};

static bool get_cu_line_info(Dwarf_Debug dbg, QList<lineinfo_entry> &linenums);

void *DwarfSymbolProvider::loadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *_err)
{
	QString cp = QDir::cleanPath(mod_path);
	if (!cp.startsWith(gd->getModuleDirectory()))
		return nullptr;

	Dwarf_Debug dbg = 0;
	int res = DW_DLV_ERROR;
	Dwarf_Error error = 0;
	Dwarf_Handler errhand = 0;
	Dwarf_Ptr errarg = 0;

	int fd = open(mod_path.toLocal8Bit().constData(), O_RDONLY | O_BINARY);

	if (fd < 0) {
		return nullptr;
	}

	res = dwarf_init(fd, DW_DLC_READ, errhand, errarg, &dbg, &error);
	if (res != DW_DLV_OK) {
		fprintf(stderr, "ERROR: %s\n", dwarf_errmsg(error));
		return nullptr;
	}

	uint64_t offset;
	if (!get_module_base_offset(dbg, &offset)) {
		if (dwarf_finish(dbg, &error) != DW_DLV_OK) {
			fprintf(stderr, "Failed DWARF finalization\n");
		}
		close(fd);
		return nullptr;
	}


	module_dwarf_private_data *md = new module_dwarf_private_data();
	md->module_name = mod_path;
	md->dbg = dbg;
	md->fd = fd;
	md->base_offset = offset;

	module_data[mod_path] = md;

	return (void *)md;
}


bool DwarfSymbolProvider::initialize(GixDebugger *gd, void *hproc, void *userdata)
{
	return true;
}

bool DwarfSymbolProvider::isGnuCOBOLModule(GixDebugger *gd, void *hproc, void *hmod, void *userdata, int *err)
{
	return true;
}

uint64_t hex2int64(const char *hex)
{
	uint64_t val = 0;
	while (*hex) {
		// get current character then increment
		uint8_t byte = *hex++;
		// transform hex character to the 4bit equivalent number, using the ascii table indexes
		if (byte >= '0' && byte <= '9') byte = byte - '0';
		else if (byte >= 'a' && byte <= 'f') byte = byte - 'a' + 10;
		else if (byte >= 'A' && byte <= 'F') byte = byte - 'A' + 10;
		// shift 4 to make space for new digit, and add the 4 bits of the new digit 
		val = (val << 4) | (byte & 0xF);
	}
	return val;
}

SharedModuleInfo *DwarfSymbolProvider::extractModuleDebugInfo(GixDebugger *gd, void *hproc, void *hmod, void *hsym, const QString &mod_path, void *userdata, int *err)
{
	module_dwarf_private_data *md = nullptr;
	if (!module_data.contains(mod_path))
		return nullptr;

	md = module_data[mod_path];

	SharedModuleInfo *shared_module = new SharedModuleInfo();
	shared_module->owner = gd;

	shared_module->dll_path = mod_path;
	shared_module->dll_base = hmod;
	shared_module->dll_symbols = hsym;

	shared_module->base_of_code = (uint64_t)userdata;


	QList<lineinfo_entry> linenum_info;

	if (get_cu_line_info(md->dbg, linenum_info)) {

		for (lineinfo_entry qp : linenum_info) {

			if (!is_acceptable_source(gd, qp.filename))
				continue;

			if (shared_module->owner->existsBreakpoint(qp.filename, qp.lineno))
				continue;

			if (!shared_module->source_files.contains(qp.filename))
				shared_module->source_files.push_back(qp.filename);

			uint64_t dwarf_addr = (uint64_t)qp.lineaddr;

			uint64_t addr = (uint64_t)shared_module->dll_base + (dwarf_addr - md->base_offset) + shared_module->base_of_code;	// 0x1000

			SourceLineInfo *li = new SourceLineInfo();
			li->module = shared_module;
			li->source_file = qp.filename;

			li->line = qp.lineno;
			li->addr = (void *)addr;

			QString k = li->source_file + ":" + QString::number(li->line);
			shared_module->owner->source_lines[k] = li;
			shared_module->owner->source_lines_by_addr[li->addr] = li;

			//fprintf(stderr, "%s : 0x%s\n", k.c_str(), DbgUtils::to_hex((uint64_t)li->addr).c_str());

			UserBreakpoint *bkp = new UserBreakpoint();
			bkp->owner = shared_module;
			bkp->automatic = true;
			bkp->address = li->addr;
			bkp->key = k;
			bkp->line = li->line;
			bkp->source_file = li->source_file;
			shared_module->owner->installHardwareBreakpoint(bkp);
			shared_module->owner->breakpoints[k] = bkp;

		}
	}

	QMap<QString, void *> funcinfo;

	bool b = locate_cobol_modules(md, funcinfo);
	if (b) {
		for (auto m : funcinfo.keys()) {
			uint64_t dwarf_addr = (uint64_t)funcinfo[m];

			CobolModuleInfo *cmi = new CobolModuleInfo();
			cmi->name = m;
			cmi->owner = shared_module;
			cmi->entry_point = (void *)(((uint64_t)shared_module->dll_base) + (dwarf_addr - md->base_offset) + shared_module->base_of_code);	// 0x1000

			cmi->entry_breakpoint = new UserBreakpoint();
			cmi->entry_breakpoint->address = cmi->entry_point;
			cmi->entry_breakpoint->automatic = true;
			cmi->entry_breakpoint->key = QString(m) + ":0";
			cmi->entry_breakpoint->line = 0;
			cmi->entry_breakpoint->orig_instr = 0x00;
			cmi->entry_breakpoint->owner = shared_module;
			cmi->entry_breakpoint->source_file = "";

			shared_module->cbl_modules[m] = cmi;

			initCobolModuleLocalInfo(gd, hproc, cmi);
		}
	}

	return shared_module;
}

void *DwarfSymbolProvider::getSymbolAddress(GixDebugger *gd, void *hproc, void *hmod, const QString &sym_name, void *userdata, int *err)
{
	return 0;
}

void *get_die_addr(Dwarf_Die die, Dwarf_Debug dbg)
{
	Dwarf_Error err = 0;;
	Dwarf_Attribute *attrs;
	Dwarf_Signed attrcount, i;

	if (!die)
		return 0;

	if (dwarf_attrlist(die, &attrs, &attrcount, &err) != DW_DLV_OK)
		return NULL;

	for (i = 0; i < attrcount; ++i) {
		Dwarf_Half attrcode;
		if (dwarf_whatattr(attrs[i], &attrcode, &err) != DW_DLV_OK)
			break;

		if (attrcode != DW_AT_location)
			continue;

		Dwarf_Half theform = 0;
		Dwarf_Half directform = 0;
		Dwarf_Block *tempb = 0;

		if (get_form_values(dbg, attrs[i], &theform, &directform, &err))
			break;

		if (theform == DW_FORM_exprloc) {
			Dwarf_Unsigned blen;
			Dwarf_Ptr bdata;
			if (dwarf_formexprloc(attrs[i], &blen, &bdata, &err))
				break;

			uint8_t op = *((uint8_t *)bdata);
			if (op == DW_OP_addr) {
				uint8_t *data_addr = ((uint8_t *)bdata) + 1;
				uint64_t addr = *((uint64_t *)data_addr);

				return (void *)addr;

			}
		}

	}
	return 0;
}


int DwarfSymbolProvider::readSymbolValueAsInt(GixDebugger *gd, CobolModuleInfo *cmi, Dwarf_Debug dbg, void *hproc, const QString &s)
{
	QString dllpath = cmi->owner->dll_path;
	if (!module_data.contains(dllpath))
		return -1;

	module_dwarf_private_data *md = module_data[dllpath];
	SharedModuleInfo *shared_module = cmi->owner;

	Dwarf_Die die = find_top_level_die(s, dbg);
	if (!die)
		return -1;

	uint64_t dwarf_addr = (uint64_t)get_die_addr(die, dbg);
	if (!dwarf_addr)
		return -1;

	uint64_t addr = (uint64_t)shared_module->dll_base + (dwarf_addr - md->base_offset) + shared_module->base_of_code;	// 0x1000

	int res = 0;
	if (!gd->readProcessMemory((void *)addr, &res, sizeof(int)))
		return -1;

	return res;
}

uint8_t *DwarfSymbolProvider::readSymbolValueInBuffer(GixDebugger *gd, CobolModuleInfo *cmi, Dwarf_Debug dbg, void *hproc, const QString &s, int bfrlen)
{
	QString dllpath = cmi->owner->dll_path;
	if (!module_data.contains(dllpath))
		return 0;

	module_dwarf_private_data *md = module_data[dllpath];
	SharedModuleInfo *shared_module = cmi->owner;

	Dwarf_Die die = find_top_level_die(s, dbg);
	if (!die)
		return 0;

	uint64_t dwarf_addr = (uint64_t)get_die_addr(die, dbg);
	if (!dwarf_addr)
		return 0;

	uint64_t addr = (uint64_t)shared_module->dll_base + (dwarf_addr - md->base_offset) + shared_module->base_of_code;	// 0x1000

	uint8_t *bfr = (uint8_t *)calloc(1, bfrlen);
	if (!gd->readProcessMemory((void *)addr, bfr, bfrlen))
		return 0;

	return bfr;
}


bool DwarfSymbolProvider::initCobolModuleLocalInfo(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi)
{
	QString dllpath = cmi->owner->dll_path;
	if (!module_data.contains(dllpath))
		return false;

	module_dwarf_private_data *md = module_data[dllpath];
	Dwarf_Debug dbg = md->dbg;

	int __GIX_SYM_MOD_MC = readSymbolValueAsInt(gd, cmi, dbg, hproc, "__GIX_SYM_" + cmi->name + "_MC");
	int __GIX_SYM_MOD_MS = readSymbolValueAsInt(gd, cmi, dbg, hproc, "__GIX_SYM_" + cmi->name + "_MS");
	int __GIX_SYM_MOD_EC = readSymbolValueAsInt(gd, cmi, dbg, hproc, "__GIX_SYM_" + cmi->name + "_EC");
	int __GIX_SYM_MOD_ES = readSymbolValueAsInt(gd, cmi, dbg, hproc, "__GIX_SYM_" + cmi->name + "_ES");

	uint8_t *__GIX_SYM_MOD_E = readSymbolValueInBuffer(gd, cmi, dbg, hproc, "__GIX_SYM_" + cmi->name + "_E", __GIX_SYM_MOD_ES);
	uint8_t *__GIX_SYM_MOD_M = readSymbolValueInBuffer(gd, cmi, dbg, hproc, "__GIX_SYM_" + cmi->name + "_M", __GIX_SYM_MOD_MS);

	SymbolBufferReader sr(__GIX_SYM_MOD_E, __GIX_SYM_MOD_ES);
	for (int i = 0; i < __GIX_SYM_MOD_EC; i++) {
		VariableResolverData *rd = new VariableResolverData();
		rd->var_name = sr.readString();
		rd->var_path = sr.readString();
		rd->base_var_name = sr.readString();
		rd->local_addr = sr.readInt();
		rd->storage_len = sr.readInt();

		int display_size = sr.readInt();
		int is_signed = sr.readInt();
		int decimals = sr.readInt();
		QString format = sr.readString();
		int storage_type = sr.readInt();
		QString storage = sr.readString();
		int occurs = sr.readInt();
		QString redefines = sr.readString();

		cmi->locals[rd->var_name] = rd;
	}

	SymbolBufferReader sm(__GIX_SYM_MOD_M, __GIX_SYM_MOD_MS);
	for (int i = 0; i < __GIX_SYM_MOD_MC; i++) {
		QString sym_name = sm.readString();
		QString var_name = sm.readString();
		int storage_size = sm.readInt();

		if (!cmi->locals.contains(var_name))
			continue;

		VariableResolverData *rd = cmi->locals[var_name];
		rd->local_sym_name = sym_name;
	}

	free(__GIX_SYM_MOD_E);
	free(__GIX_SYM_MOD_M);

	cmi->initialized = true;

	return true;
}

bool DwarfSymbolProvider::deinit(GixDebugger *gd, void *hproc)
{
	return true;
}

QString DwarfSymbolProvider::dumpStackFrame(GixDebugger *gd, void *hproc, void *hthread)
{
	return QString();
}

bool DwarfSymbolProvider::is_acceptable_source(GixDebugger *gd, const QString &filename)
{
	for (QString sm : gd->src_file_types) {
		if (sm.startsWith("*"))
			sm = sm.mid(1);

		if (filename.endsWith(sm))
			return true;
	}
	return false;
}

bool get_module_base_offset(Dwarf_Debug dbg, uint64_t *offset)
{
	Dwarf_Unsigned cu_header_length, abbrev_offset, next_cu_header;
	Dwarf_Half version_stamp, address_size;
	Dwarf_Error err = 0;;
	Dwarf_Die no_die = 0, cu_die;
	Dwarf_Attribute *attrs;
	Dwarf_Addr lowpc = 0, highpc = 0;
	Dwarf_Signed attrcount, i;

	/* Find compilation unit header */
	if (dwarf_next_cu_header(
		dbg,
		&cu_header_length,
		&version_stamp,
		&abbrev_offset,
		&address_size,
		&next_cu_header,
		&err) == DW_DLV_ERROR)
		return false;

	/* Expect the CU to have a single sibling - a DIE */
	if (dwarf_siblingof(dbg, no_die, &cu_die, &err) == DW_DLV_ERROR)
		return false;

	/* Grab the DIEs attributes for display */
	if (dwarf_attrlist(cu_die, &attrs, &attrcount, &err) != DW_DLV_OK)
		return false;

	for (i = 0; i < attrcount; ++i) {
		Dwarf_Half attrcode;
		if (dwarf_whatattr(attrs[i], &attrcode, &err) != DW_DLV_OK)
			return false;

		/* We only take some of the attributes for display here.
		** More can be picked with appropriate tag constants.
		*/
		if (attrcode == DW_AT_low_pc)
			dwarf_formaddr(attrs[i], &lowpc, 0);
		else if (attrcode == DW_AT_high_pc) {
			Dwarf_Half form = 0;
			Dwarf_Unsigned offset = 0;
			dwarf_whatform(attrs[i], &form, &err);

			switch (form) {
				case DW_FORM_addr:
					dwarf_formaddr(attrs[i], &highpc, &err);
					//printf("DW_FORM_addr: 0x%08llx\n", &highpc);
					break;
				case DW_FORM_data8:
					dwarf_formudata(attrs[i], &offset, &err);
					//printf("DW_FORM_data8: 0x%08llx\n", offset);
					break;
				default:
					break;
			}

		}
	}

	//printf("low pc  : 0x%08llx\n", lowpc);
	//printf("high pc : 0x%08llx\n", highpc);

	*offset = lowpc;
	return true;
}


static bool get_cu_line_info(Dwarf_Debug dbg, QList<lineinfo_entry> &linenums)
{
	Dwarf_Unsigned lineversion = 0;
	Dwarf_Signed linecount = 0;
	Dwarf_Line *linebuf = NULL;
	Dwarf_Signed linecount_actuals = 0;
	Dwarf_Line *linebuf_actuals = NULL;
	Dwarf_Small  table_count = 0;
	int lres = 0;
	int line_errs = 0;
	Dwarf_Line_Context line_context = 0;
	const char *sec_name = 0;
	Dwarf_Off cudie_local_offset = 0;
	int lires = 0;
	int atres = 0;
	int ares = 0;
	Dwarf_Unsigned lineno = 0;
	Dwarf_Unsigned cu_header_length, abbrev_offset, next_cu_header;
	Dwarf_Half version_stamp, address_size;
	Dwarf_Error err = 0;;
	Dwarf_Die no_die = 0, cu_die;
	Dwarf_Attribute *attrs;
	Dwarf_Addr lowpc = 0, highpc = 0;
	Dwarf_Signed attrcount, i;

	/* Find compilation unit header */
	if (dwarf_next_cu_header(
		dbg,
		&cu_header_length,
		&version_stamp,
		&abbrev_offset,
		&address_size,
		&next_cu_header,
		&err) == DW_DLV_ERROR)
		return false;

	/* Expect the CU to have a single sibling - a DIE */
	if (dwarf_siblingof(dbg, no_die, &cu_die, &err) == DW_DLV_ERROR)
		return false;

	lres = dwarf_srclines_b(cu_die, &lineversion,
		&table_count, &line_context,
		&err);

	if (table_count > 0) {

	}

	if (lres == DW_DLV_OK) {
		lres = dwarf_srclines_two_level_from_linecontext(line_context,
			&linebuf, &linecount,
			&linebuf_actuals, &linecount_actuals,
			&err);
	}

	if (linecount > 0) {

		Dwarf_Signed i = 0;
		Dwarf_Addr pc = 0;
		Dwarf_Error lt_err = 0;

		for (i = 0; i < linecount; i++) {
			Dwarf_Line line = linebuf[i];
			char *filename = 0;
			int nsres = 0;

			pc = 0;
			ares = dwarf_lineaddr(line, &pc, &lt_err);
			lires = dwarf_lineno(line, &lineno, &lt_err);
			dwarf_linesrc(line, &filename, &err);

			QString f = QDir::cleanPath(filename);
			lineinfo_entry t = { f, (int)lineno, (void *)pc };
			linenums.push_back(t);
		}
	}
	return true;
}

/* List a function if it's in the given DIE.
*/
bool list_func_in_die(Dwarf_Debug dgb, Dwarf_Die the_die, QMap<QString, void *> &funcinfo)
{
	char *die_name = 0;
	const char *tag_name = 0;
	Dwarf_Error err;
	Dwarf_Half tag;
	Dwarf_Attribute *attrs;
	Dwarf_Addr lowpc = 0, highpc = 0;
	Dwarf_Signed attrcount, i;
	int rc = dwarf_diename(the_die, &die_name, &err);

	if (rc == DW_DLV_ERROR)
		return false;

	else if (rc == DW_DLV_NO_ENTRY)
		return false;

	if (dwarf_tag(the_die, &tag, &err) != DW_DLV_OK)
		return false;

	/* Only interested in subprogram DIEs here */
	if (tag != DW_TAG_subprogram)
		return false;

	if (dwarf_get_TAG_name(tag, &tag_name) != DW_DLV_OK)
		return false;

	//printf("DW_TAG_subprogram: '%s'\n", die_name);

	/* Grab the DIEs attributes for display */
	if (dwarf_attrlist(the_die, &attrs, &attrcount, &err) != DW_DLV_OK)
		return false;

	for (i = 0; i < attrcount; ++i) {
		Dwarf_Half attrcode;
		if (dwarf_whatattr(attrs[i], &attrcode, &err) != DW_DLV_OK)
			return false;

		/* We only take some of the attributes for display here.
		** More can be picked with appropriate tag constants.
		*/
		if (attrcode == DW_AT_low_pc)
			dwarf_formaddr(attrs[i], &lowpc, 0);
		else if (attrcode == DW_AT_high_pc) {
			Dwarf_Half form = 0;
			Dwarf_Unsigned offset = 0;
			dwarf_whatform(attrs[i], &form, &err);

			switch (form) {
				case DW_FORM_addr:
					dwarf_formaddr(attrs[i], &highpc, &err);
					//printf("DW_FORM_addr: 0x%08llx\n", &highpc);
					break;
				case DW_FORM_data8:
					dwarf_formudata(attrs[i], &offset, &err);
					//printf("DW_FORM_data8: 0x%08llx\n", offset);
					break;
				default:
					break;
			}
		}
	}

	if (lowpc) {
		funcinfo[die_name] = (void *)lowpc;
		return true;
	}

	//printf("low pc  : 0x%08llx\n", lowpc);
	//printf("high pc : 0x%08llx\n", highpc);

	return false;
}


/* List all the functions from the file represented by the given descriptor.
*/
bool list_funcs_in_file(Dwarf_Debug dbg, QMap<QString, void *> &funcinfo)
{
	Dwarf_Unsigned cu_header_length, abbrev_offset, next_cu_header;
	Dwarf_Half version_stamp, address_size;
	Dwarf_Error err = 0;;
	Dwarf_Die no_die = 0, cu_die, child_die;

	while (dwarf_next_cu_header(
		dbg,
		&cu_header_length,
		&version_stamp,
		&abbrev_offset,
		&address_size,
		&next_cu_header,
		&err) != DW_DLV_NO_ENTRY);


	while (true) {
		/* Find compilation unit header */
		if (dwarf_next_cu_header(
			dbg,
			&cu_header_length,
			&version_stamp,
			&abbrev_offset,
			&address_size,
			&next_cu_header,
			&err) != DW_DLV_OK)
			break;

		/* Expect the CU to have a single sibling - a DIE */
		if (dwarf_siblingof(dbg, no_die, &cu_die, &err) == DW_DLV_ERROR)
			return false;

		list_func_in_die(dbg, cu_die, funcinfo);

		/* Expect the CU DIE to have children */
		if (dwarf_child(cu_die, &child_die, &err) == DW_DLV_ERROR)
			return false;

		/* Now go over all children DIEs */
		while (1) {
			int rc;
			list_func_in_die(dbg, child_die, funcinfo);

			rc = dwarf_siblingof(dbg, child_die, &child_die, &err);

			if (rc == DW_DLV_ERROR)
				return false;
			else if (rc == DW_DLV_NO_ENTRY)
				break; /* done */
		}
	}
	return true;
}

bool checkCobolModuleArgs(QString fname, void *faddr)
{
	// TODO: here we should actually check if parameters are consistent withthose of a GNUCobol module
	return true;
}

bool locate_cobol_modules(module_dwarf_private_data *md, QMap<QString, void *> &funcinfo)
{
	QMap<QString, void *> tmp_funcinfo;

	if (!list_funcs_in_file(md->dbg, tmp_funcinfo))
		return false;

	funcinfo.clear();

	for (QString fname : tmp_funcinfo.keys()) {
		if (tmp_funcinfo.contains(fname + "_") && checkCobolModuleArgs(fname, tmp_funcinfo[fname])) {
			funcinfo.insert(fname, tmp_funcinfo[fname]);
		}
	}

	return true;
}

Dwarf_Die get_die_name(Dwarf_Debug dbg, Dwarf_Die the_die, const QString &name)
{
	char *die_name = 0;
	const char *tag_name = 0;
	Dwarf_Error err;
	Dwarf_Half tag;
	Dwarf_Attribute *attrs;
	Dwarf_Addr lowpc = 0, highpc = 0;
	Dwarf_Signed attrcount, i;

	int rc = dwarf_diename(the_die, &die_name, &err);

	if (rc == DW_DLV_ERROR)
		return NULL;

	else if (rc == DW_DLV_NO_ENTRY)
		return NULL;

	if (dwarf_tag(the_die, &tag, &err) != DW_DLV_OK)
		return NULL;

	/* Only interested in subprogram DIEs here */
	if (tag != DW_TAG_subprogram)
		return NULL;


	if (dwarf_get_TAG_name(tag, &tag_name) != DW_DLV_OK)
		return NULL;

	//printf("DW_TAG_subprogram: '%s'\n", die_name);


}

Dwarf_Die find_top_level_die(const QString &target_die_name, Dwarf_Debug dbg)
{
	Dwarf_Unsigned cu_header_length, abbrev_offset, next_cu_header;
	Dwarf_Half version_stamp, address_size;
	Dwarf_Error err = 0;;
	Dwarf_Die no_die = 0, cu_die, child_die;
	char *die_name = 0;

	while (dwarf_next_cu_header(
		dbg,
		&cu_header_length,
		&version_stamp,
		&abbrev_offset,
		&address_size,
		&next_cu_header,
		&err) != DW_DLV_NO_ENTRY);


	while (true) {
		/* Find compilation unit header */
		if (dwarf_next_cu_header(
			dbg,
			&cu_header_length,
			&version_stamp,
			&abbrev_offset,
			&address_size,
			&next_cu_header,
			&err) != DW_DLV_OK)
			break;

		/* Expect the CU to have a single sibling - a DIE */
		if (dwarf_siblingof(dbg, no_die, &cu_die, &err) == DW_DLV_ERROR)
			return NULL;

		//list_func_in_die(dbg, cu_die, funcinfo);

		int rc = dwarf_diename(cu_die, &die_name, &err);

		/* Expect the CU DIE to have children */
		if (dwarf_child(cu_die, &child_die, &err) == DW_DLV_ERROR)
			return NULL;

		/* Now go over all children DIEs */
		while (1) {
			rc = dwarf_diename(child_die, &die_name, &err);
			if (die_name == target_die_name)
				return child_die;

			rc = dwarf_siblingof(dbg, child_die, &child_die, &err);

			if (rc == DW_DLV_ERROR)
				return NULL;
			else if (rc == DW_DLV_NO_ENTRY)
				break; /* done */
		}
	}
	return 0;
}

static int
get_form_values(Dwarf_Debug dbg,
	Dwarf_Attribute attrib,
	Dwarf_Half *theform, Dwarf_Half *directform,
	Dwarf_Error *err)
{
	int res = 0;

	res = dwarf_whatform(attrib, theform, err);
	if (res != DW_DLV_OK) {
		return res;
	}
	res = dwarf_whatform_direct(attrib, directform, err);
	return res;
}


void *DwarfSymbolProvider::get_local_var_addr(CobolModuleInfo *cmi, const QString &sym_name)
{
	Dwarf_Die c;
	Dwarf_Attribute *attrs;
	Dwarf_Error err;
	char *die_name = 0;
	Dwarf_Signed attrcount, i;
	Dwarf_Addr addr;
	int rc;

	QString dllpath = cmi->owner->dll_path;
	if (!module_data.contains(dllpath))
		return NULL;

	module_dwarf_private_data *md = module_data[dllpath];
	Dwarf_Debug dbg = md->dbg;
	Dwarf_Die the_die = find_top_level_die(cmi->name + "_", dbg);
	if (!the_die)
		return NULL;

	if (dwarf_child(the_die, &c, &err) == DW_DLV_ERROR)
		return NULL;

	do {
		if (dwarf_diename(c, &die_name, &err))
			break;

		if (sym_name == die_name) {

			if (dwarf_attrlist(c, &attrs, &attrcount, &err) != DW_DLV_OK)
				return NULL;

			for (i = 0; i < attrcount; ++i) {
				Dwarf_Half attrcode;
				if (dwarf_whatattr(attrs[i], &attrcode, &err) != DW_DLV_OK)
					break;

				if (attrcode != DW_AT_location)
					continue;

				Dwarf_Half theform = 0;
				Dwarf_Half directform = 0;
				Dwarf_Block *tempb = 0;

				if (get_form_values(dbg, attrs[i], &theform, &directform, &err))
					break;

				if (theform == DW_FORM_exprloc) {
					Dwarf_Unsigned blen;
					Dwarf_Ptr bdata;
					if (dwarf_formexprloc(attrs[i], &blen, &bdata, &err))
						break;

					uint8_t op = *((uint8_t *)bdata);
					if (op == DW_OP_addr) {
						uint8_t *data_addr = ((uint8_t *)bdata) + 1;
						uint64_t addr = *((uint64_t *)data_addr);

						//printf(":: %s : 0x%08llx\n", die_name, addr);

						return (void *)addr;
					}
				}

				//printf("%s: N/A\n", die_name);
			}
		}

		if (dwarf_siblingof(dbg, c, &c, &err))
			break;


	} while (1);

	return 0;
}

void *DwarfSymbolProvider::resolveLocalVariableAddress(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi, uint64_t frame_ptr, VariableResolverData *rootvar, VariableResolverData *vvar)
{
	uint64_t dll_base = (uint64_t)cmi->owner->dll_base;

	if (!module_data.contains(cmi->owner->dll_path))
		return nullptr;

	module_dwarf_private_data *md = module_data[cmi->owner->dll_path];

	uint64_t root_addr = (uint64_t)get_local_var_addr(cmi, rootvar->local_sym_name);

	uint64_t offset = (root_addr - md->base_offset) + cmi->owner->base_of_code;	// 0x1000
	offset += vvar->local_addr;

	return (void *)(dll_base + offset);
}
