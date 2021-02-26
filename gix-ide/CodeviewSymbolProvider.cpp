#include "CodeviewSymbolProvider.h"
#include "SymbolBufferReader.h"
#include <utils.h>
#include <QDir>
//#include <libcob.h>

static BOOL __stdcall DLL_EnumSourceFilesProc(PSOURCEFILE pSourceFile, PVOID UserContext);
static BOOL __stdcall DLL_EnumLinesProc(PSRCCODEINFO LineInfo, PVOID UserContext);
static BOOL __stdcall DLL_SymEnumerateLocalSymbolsCallback(PSYMBOL_INFO pSymInfo, ULONG SymbolSize, PVOID UserContext);


extern void ListDLLFunctions(QString sADllName, QList<QString> &slListOfDllFunctions);

bool CodeviewSymbolProvider::deinit(GixDebugger *gd, void *hproc)
{
	bool b = true;
	for (auto sh : symbol_handles) {
		if (sh)
			b &= SymUnloadModule64((HANDLE)hproc, sh);
	}
	return b;
}

BOOL __stdcall _ProcessMemoryReader(HANDLE  hProcess,
	DWORD qwBaseAddress,
	PVOID   lpBuffer,
	DWORD   nSize,
	LPDWORD lpNumberOfBytesRead)
{

	SIZE_T st;
	BOOL   bRet = ReadProcessMemory(hProcess, (LPVOID)qwBaseAddress, lpBuffer, nSize, &st);
	*lpNumberOfBytesRead = (DWORD)st;
	//printf("ReadMemory: hProcess: %p, baseAddr: %p, buffer: %p, size: %d, read: %d, result: %d\n", hProcess, (LPVOID) qwBaseAddress, lpBuffer, nSize, (DWORD) st, (DWORD) bRet);
	return bRet;

}


QString CodeviewSymbolProvider::dumpStackFrame(GixDebugger *gd, void *hproc, void *hthread)
{
	QString res;

	STACKFRAME64 stack = { 0 };
	DWORD64 dwDisplacement64 = 0;
	DWORD dwDisplacement = 0;

	CONTEXT context;
	context.ContextFlags = CONTEXT_FULL;
	GetThreadContext(hthread, &context);

	// Must be like this
	stack.AddrPC.Offset = context.Rip; // EIP - Instruction Pointer
	stack.AddrPC.Mode = AddrModeFlat;
	stack.AddrFrame.Offset = context.Rbp; // EBP
	stack.AddrFrame.Mode = AddrModeFlat;
	stack.AddrStack.Offset = context.Rsp; // ESP - Stack Pointer
	stack.AddrStack.Mode = AddrModeFlat;

	BOOL bSuccess;
	do {
		bSuccess = StackWalk64(IMAGE_FILE_MACHINE_AMD64, hproc, hthread, &stack,
			&context, (PREAD_PROCESS_MEMORY_ROUTINE64)&_ProcessMemoryReader, SymFunctionTableAccess64,
			SymGetModuleBase64, 0);

		if (!bSuccess)
			break;

		// Symbol retrieval code goes here.

		// 1
		IMAGEHLP_MODULE64 module = { 0 };
		module.SizeOfStruct = sizeof(module);
		SymGetModuleInfo64(hproc, (DWORD64)stack.AddrPC.Offset, &module);

		// 2
		IMAGEHLP_SYMBOL64 *pSymbol;
		DWORD dwDisplacement;
		pSymbol = (IMAGEHLP_SYMBOL64 *)new BYTE[sizeof(IMAGEHLP_SYMBOL64) + MAX_SYM_NAME];

		memset(pSymbol, 0, sizeof(IMAGEHLP_SYMBOL64) + MAX_SYM_NAME);
		pSymbol->SizeOfStruct = sizeof(IMAGEHLP_SYMBOL64); // Required
		pSymbol->MaxNameLength = MAX_SYM_NAME;             // Required

		SymGetSymFromAddr64(hproc, stack.AddrPC.Offset,
			&dwDisplacement64, pSymbol); // Retruns true on success

		// 3
		IMAGEHLP_LINE64 line;
		line.SizeOfStruct = sizeof(line);

		bSuccess = SymGetLineFromAddr64(hproc,
			stack.AddrPC.Offset,
			&dwDisplacement, &line);

		if (bSuccess) {
			res += QString("%1:%2\n").arg(line.FileName).arg(line.LineNumber);
		}


	} while (stack.AddrReturn.Offset != 0);

	return res;
}

void *CodeviewSymbolProvider::resolveLocalVariableAddress(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi, uint64_t frame_ptr, VariableResolverData *vrootvar, VariableResolverData *vvar)
{
	int err = 0;
	IMAGEHLP_STACK_FRAME isf;
	ZeroMemory(&isf, sizeof(IMAGEHLP_STACK_FRAME));

	DWORD64 mod_address = getSymbolAddress(gd, hproc, NULL, cmi->name + "_", NULL, &err);
	isf.InstructionOffset = mod_address;
	SymSetContext(hproc, &isf, NULL);

	DWORD64 root_addr = getSymbolAddress(gd, hproc, NULL, vrootvar->local_sym_name, NULL, 0);
	if (!root_addr)
		return 0;

	unsigned long long addr = ((frame_ptr + root_addr + vvar->local_addr) - 1);
	return (void *)addr;
}

bool CodeviewSymbolProvider::initialize(GixDebugger *gd, void *hproc, void *userdata)
{
	QString wd = QDir::toNativeSeparators(gd->getWorkingDirectory());
#if _DEBUG	
	SymSetOptions(SYMOPT_DEBUG);
#endif
	return SymInitialize(hproc, wd.toLocal8Bit().constData(), false);
}

static BOOL __stdcall DLL_GetSymbolFromDLL(PSYMBOL_INFO pSymInfo, ULONG SymbolSize, PVOID UserContext)
{
	SYMBOL_INFO *pSymbol = (SYMBOL_INFO *)UserContext;
	memcpy(pSymbol, pSymInfo, sizeof(SYMBOL_INFO) + MAX_SYM_NAME);
	return false;
}

bool CodeviewSymbolProvider::isGnuCOBOLModule(GixDebugger *gd, void *hproc, void *hmod, void *userdata, int *err)
{
	bool res = true;
	SYMBOL_INFO *pSymbol;
	pSymbol = (SYMBOL_INFO *)new BYTE[sizeof(SYMBOL_INFO) + MAX_SYM_NAME];
	pSymbol->SizeOfStruct = sizeof(SYMBOL_INFO);
	pSymbol->MaxNameLen = MAX_SYM_NAME;
	ZeroMemory(pSymbol, sizeof(SYMBOL_INFO) + MAX_SYM_NAME);

	if (!SymEnumSymbols(hproc, (ULONG64)hmod, "cob_module_global_enter", DLL_GetSymbolFromDLL, pSymbol)) {
		gd->printLastError();
		delete[](BYTE *)pSymbol;
		return false;
	}

	res &= ((bool)(pSymbol->Flags & 0x400000));

	return res;
}

void *CodeviewSymbolProvider::loadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *err)
{
	HANDLE hProcess = (HANDLE)hproc;
	DWORD64 dll_base = (DWORD64)hmod;

	DWORD64 hSymbols = SymLoadModule64(hProcess, NULL, mod_path.toLocal8Bit().constData(), 0, dll_base, 0);
	if (!hSymbols) {
		gd->printLastError();
		*err = GetLastError();
		return nullptr;
	}

	symbol_handles.push_back(hSymbols);

	if (!dll_base)
		dll_base = hSymbols;

	IMAGEHLP_MODULE64 module_info;
	module_info.SizeOfStruct = sizeof(module_info);
	if (!SymGetModuleInfo64(hProcess, dll_base, &module_info)) {
		gd->printLastError();
		*err = GetLastError();
		return nullptr;
	}

	if (!is_dwarf2pdb && module_info.SymType != SymPdb) {
		*err = 0x02;
		return nullptr;
	}

	*err = 0x00;
	return (void *)hSymbols;
}

SharedModuleInfo *CodeviewSymbolProvider::extractModuleDebugInfo(GixDebugger *gd, void *hproc, void *hmod, void *hsym, const QString &mod_path, void *userdata, int *err)
{
	HANDLE hProcess = (HANDLE)hproc;
	DWORD64 dll_base = (DWORD64)hmod;

	SharedModuleInfo *mi = new SharedModuleInfo();
	mi->owner = gd;
	//mi->name = _name;

	mi->dll_path = mod_path;
	mi->dll_base = hmod;
	mi->dll_symbols = hsym;

	//pd->start_addr = (PVOID)getSymbolAddress(hProcess, _name);

	//SymSetOptions(SYMOPT_LOAD_ANYTHING | SYMOPT_UNDNAME | SYMOPT_AUTO_PUBLICS | SYMOPT_INCLUDE_32BIT_MODULES);

	for (auto src_mask : gd->src_file_types) {
		if (!SymEnumSourceFiles(hProcess, (ULONG64)hsym, src_mask.toLocal8Bit().constData(), DLL_EnumSourceFilesProc, mi)) {
			gd->printLastError();
			return nullptr;
		}
	}

	for (auto sf : mi->source_files) {
		SymEnumLines(hProcess, (ULONG64)hsym, NULL, sf.toLocal8Bit().constData(), DLL_EnumLinesProc, mi);
	}

	/*
		To view local symbols or function parameters, call the SymSetContext function with the InstructionOffset
		member of the IMAGEHLP_STACK_FRAME structure set to the address of any function symbol.
		Subsequent calls to SymFromName and SymEnumSymbols can operate within the context of this address.
		To do so, set the BaseOfDll parameter to NULL and omit the module specifier from the Name or Mask parameter.
		This forces DbgHelp to search for matching symbols within the scope indicated by SymSetContext.
	*/

	IMAGEHLP_STACK_FRAME isf;
	ZeroMemory(&isf, sizeof(IMAGEHLP_STACK_FRAME));

	QList<QString> cml;
	ListDLLFunctions(mod_path, cml);
	for (auto m : cml) {
		if (m.startsWith("__GIX_SYM_"))
			continue;

		CobolModuleInfo *cmi = new CobolModuleInfo();
		cmi->name = m;
		cmi->owner = mi;
		cmi->entry_point = (void *)getSymbolAddress(gd, hproc, hmod, QString(m) + "_", NULL, err);
		mi->cbl_modules[m] = cmi;
		cmi->entry_breakpoint = new UserBreakpoint();
		cmi->entry_breakpoint->address = cmi->entry_point;
		cmi->entry_breakpoint->automatic = true;
		cmi->entry_breakpoint->key = QString(m) + ":0";
		cmi->entry_breakpoint->line = 0;
		cmi->entry_breakpoint->orig_instr = 0x00;
		cmi->entry_breakpoint->owner = mi;
		cmi->entry_breakpoint->source_file = "";

		initCobolModuleLocalInfo(gd, hproc, cmi);
	}
	return mi;
}

DWORD64 CodeviewSymbolProvider::getSymbolAddress(GixDebugger *gd, void *hproc, void *hmod, const QString &sym_name, void *userdata, int *err)
{
	HANDLE hProcess = (HANDLE)hproc;
	SYMBOL_INFO *pSymbol;
	pSymbol = (SYMBOL_INFO *)new BYTE[sizeof(SYMBOL_INFO) + MAX_SYM_NAME];
	pSymbol->SizeOfStruct = sizeof(SYMBOL_INFO);
	pSymbol->MaxNameLen = MAX_SYM_NAME;
	BOOL rc = SymFromName(hProcess, sym_name.toLocal8Bit().constData(), pSymbol);

	// Store address, before deleting pointer  
	DWORD64 dwAddress = pSymbol->Address;

	delete[](BYTE *)pSymbol; // Valid syntax!

	return rc ? dwAddress : NULL;
}

int CodeviewSymbolProvider::readSymbolValueAsInt(void *hproc, const QString &s)
{
	DWORD64 addr = getSymbolAddress(NULL, hproc, NULL, s, NULL, 0);
	if (!addr)
		return -1;

	SIZE_T st;
	uint8_t bfr[sizeof(int)];
	if (!ReadProcessMemory(hproc, (LPVOID)addr, &bfr, sizeof(int), &st))
		return -1;

	int i = *((int *)&bfr);
	return i;
}

uint8_t *CodeviewSymbolProvider::readSymbolValueInBuffer(void *hproc, const QString &s, int bfrlen)
{
	DWORD64 addr = getSymbolAddress(NULL, hproc, NULL, s, NULL, 0);
	if (!addr)
		return 0;

	uint8_t *bfr = (uint8_t *)calloc(1, bfrlen);

	SIZE_T st;
	if (!ReadProcessMemory(hproc, (LPVOID)addr, bfr, bfrlen, &st)) {
		free(bfr);
		return 0;
	}

	return bfr;
}

bool CodeviewSymbolProvider::initCobolModuleLocalInfo(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi)
{
	int __GIX_SYM_MOD_EC = readSymbolValueAsInt(hproc, "__GIX_SYM_" + cmi->name + "_EC");
	int __GIX_SYM_MOD_ES = readSymbolValueAsInt(hproc, "__GIX_SYM_" + cmi->name + "_ES");
	int __GIX_SYM_MOD_MC = readSymbolValueAsInt(hproc, "__GIX_SYM_" + cmi->name + "_MC");
	int __GIX_SYM_MOD_MS = readSymbolValueAsInt(hproc, "__GIX_SYM_" + cmi->name + "_MS");

	uint8_t *__GIX_SYM_MOD_E = readSymbolValueInBuffer(hproc, "__GIX_SYM_" + cmi->name + "_E", __GIX_SYM_MOD_ES);
	uint8_t *__GIX_SYM_MOD_M = readSymbolValueInBuffer(hproc, "__GIX_SYM_" + cmi->name + "_M", __GIX_SYM_MOD_MS);

	SymbolBufferReader sr(__GIX_SYM_MOD_E, __GIX_SYM_MOD_ES);
	for (int i = 0; i < __GIX_SYM_MOD_EC; i++) {
		VariableResolverData *rd = new VariableResolverData();
		rd->var_name = sr.readString();
		rd->var_path = sr.readString();
		int rd_type = sr.readInt();
		int rd_level = sr.readInt();
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

BOOL __stdcall  DLL_EnumSourceFilesProc(PSOURCEFILE pSourceFile, PVOID UserContext)
{
	SharedModuleInfo *mi = (SharedModuleInfo *)UserContext;
	if (mi)
		mi->source_files.push_back(QString(pSourceFile->FileName));

	return true;
}

BOOL __stdcall DLL_EnumLinesProc(PSRCCODEINFO LineInfo, PVOID UserContext)
{
	SharedModuleInfo *mi = (SharedModuleInfo *)UserContext;
	if (!mi)
		return false;

	SourceLineInfo *li = new SourceLineInfo();
	li->module = mi;
	li->source_file = LineInfo->FileName;

	li->line = LineInfo->LineNumber;
	li->addr = (void *)LineInfo->Address;

	QString k = li->source_file + ":" + QString::number(li->line);
	mi->owner->source_lines[k] = li;
	mi->owner->source_lines_by_addr[li->addr] = li;

	//fprintf(stderr, "%s : 0x%s\n", k.c_str(), DbgUtils::to_hex((uint64_t)li->addr).c_str());

	UserBreakpoint *bkp = new UserBreakpoint();
	bkp->owner = mi;
	bkp->automatic = true;
	bkp->address = li->addr;
	bkp->key = k;
	bkp->line = li->line;
	bkp->source_file = li->source_file;
	mi->owner->installHardwareBreakpoint(bkp);
	mi->owner->breakpoints[k] = bkp;

	return true;
}

//BOOL __stdcall DLL_SymEnumerateLocalSymbolsCallback(PSYMBOL_INFO pSymInfo, ULONG SymbolSize, PVOID UserContext)
//{
//	CobolModuleInfo *cmi = (CobolModuleInfo *)UserContext;
//
//	cmi->locals[pSymInfo->Name] = (void *)pSymInfo->Address;
//	//fprintf(stdout, "]> %s (%p)\n", pSymInfo->Name, pSymInfo->Address);
//	return true;
//}
//
