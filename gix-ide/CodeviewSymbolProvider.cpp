/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#include "CodeviewSymbolProvider.h"
#include "SymbolBufferReader.h"
#include "StackWalker.h"
#include "GixDebuggerWin.h"
#include <utils.h>
#include <QDir>
//#include <libcob.h>

static BOOL __stdcall DLL_EnumSourceFilesProc(PSOURCEFILE pSourceFile, PVOID UserContext);
static BOOL __stdcall DLL_EnumLinesProc(PSRCCODEINFO LineInfo, PVOID UserContext);

// Just in case: MSVC has some symbol decoration issue in x86/x64
#ifdef _WIN64
#define COB_MODULE_GLOBAL_ENTER "cob_module_global_enter"
#else
#define COB_MODULE_GLOBAL_ENTER "cob_module_global_enter"
#endif

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

static class BufferedStackWalker : public StackWalker
{
public:
	BufferedStackWalker(DWORD dwProcessId, HANDLE hProcess) : StackWalker(dwProcessId, hProcess) {}
	QStringList lines;

protected:
	virtual void OnOutput(LPCSTR szText)
	{
		lines.append(szText);
		StackWalker::OnOutput(szText);
	}
};


QString CodeviewSymbolProvider::dumpStackFrame(GixDebugger *gd, void *hproc, void *hthread)
{
#if 1
	GixDebuggerWin *gdw = (GixDebuggerWin *)gd;

	BufferedStackWalker sw(gdw->getProcessId(), hproc);
	sw.ShowCallstack(hthread);
	return sw.lines.join("\n");
#else
#ifdef _WIN64
	DWORD machine_type = IMAGE_FILE_MACHINE_AMD64;
#else
	DWORD machine_type = IMAGE_FILE_MACHINE_I386;
#endif
	QString res;

	STACKFRAME64 stack = { 0 };
	DWORD64 dwDisplacement64 = 0;
	DWORD dwDisplacement = 0;

	CONTEXT context;
	context.ContextFlags = CONTEXT_FULL;
	GetThreadContext(hthread, &context);

#ifdef _WIN64
	// Must be like this
	stack.AddrPC.Offset = context.Rip; // EIP - Instruction Pointer
	stack.AddrPC.Mode = AddrModeFlat;
	stack.AddrFrame.Offset = context.Rbp; // EBP
	stack.AddrFrame.Mode = AddrModeFlat;
	stack.AddrStack.Offset = context.Rsp; // ESP - Stack Pointer
	stack.AddrStack.Mode = AddrModeFlat;
#elif defined(_WIN32)
	// Must be like this
	stack.AddrPC.Offset = context.Eip; // EIP - Instruction Pointer
	stack.AddrPC.Mode = AddrModeFlat;
	stack.AddrFrame.Offset = context.Ebp; // EBP
	stack.AddrFrame.Mode = AddrModeFlat;
	stack.AddrStack.Offset = context.Esp; // ESP - Stack Pointer
	stack.AddrStack.Mode = AddrModeFlat;
#else
#error There's something wrong with the build environment
#endif
	BOOL bSuccess;
	do {
		bSuccess = StackWalk64(machine_type, hproc, hthread, &stack,
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
			res += QString("%1 - %2:%3\n").arg(QString::fromLocal8Bit(pSymbol->Name)).arg(line.FileName).arg(line.LineNumber);
		}
		else {
			res += QString("%1\n").arg(QString::fromLocal8Bit(pSymbol->Name));
		}


	} while (stack.AddrReturn.Offset != 0);

	return res;
#endif
}

#ifdef _WIN64
void *CodeviewSymbolProvider::resolveLocalVariableAddress(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi, uint64_t frame_ptr, VariableResolverData *vrootvar, VariableResolverData *vvar)
{
	int err = 0;
	IMAGEHLP_STACK_FRAME isf;
	ZeroMemory(&isf, sizeof(IMAGEHLP_STACK_FRAME));

	DWORD64 mod_address = (DWORD64) getSymbolAddress(gd, hproc, NULL, cmi->name + "_", NULL, &err);
	isf.InstructionOffset = mod_address;
	SymSetContext(hproc, &isf, NULL);

	DWORD64 root_addr = (DWORD64)getSymbolAddress(gd, hproc, NULL, vrootvar->local_sym_name, NULL, 0);
	if (!root_addr)
		return 0;

	//unsigned long long addr_1 = ((frame_ptr + root_addr + vvar->local_addr) - 1);	// DLL
	unsigned long long addr = ((frame_ptr + root_addr + vvar->local_addr));	// EXE

	if (gd->getDebuggedModuleType() == DebuggedModuleType::Shared)
		addr -= 1;

	return (void *)addr;
}

#else
void *CodeviewSymbolProvider::resolveLocalVariableAddress(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi, uint64_t frame_ptr, VariableResolverData *vrootvar, VariableResolverData *vvar)
{
	int err = 0;
	IMAGEHLP_STACK_FRAME isf;
	ZeroMemory(&isf, sizeof(IMAGEHLP_STACK_FRAME));

	DWORD mod_address = (DWORD)getSymbolAddress(gd, hproc, NULL, cmi->name + "_", NULL, &err);
	isf.InstructionOffset = mod_address;
	SymSetContext(hproc, &isf, NULL);

	DWORD root_addr = (DWORD)getSymbolAddress(gd, hproc, NULL, vrootvar->local_sym_name, NULL, 0);

//	unsigned long addr = ((frame_ptr + root_addr + vvar->local_addr) - 1); // x64 only
	return (void *)(root_addr + vvar->local_addr);
}
#endif

bool CodeviewSymbolProvider::initialize(GixDebugger *gd, void *hproc, void *userdata)
{
	dbg_instance = gd;

	QString wd = QDir::toNativeSeparators(gd->getWorkingDirectory());
#if _DEBUG	
	//SymSetOptions(SYMOPT_DEBUG);
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

	if (!SymEnumSymbols(hproc, (ULONG64)hmod, COB_MODULE_GLOBAL_ENTER, DLL_GetSymbolFromDLL, pSymbol)) {
		gd->printLastError();
		delete[](BYTE *)pSymbol;
		return false;
	}

#if _WIN64
	res &= ((bool)(pSymbol->Flags & 0x400000));
#else
	res = (strcmp(pSymbol->Name, COB_MODULE_GLOBAL_ENTER) == 0) && (pSymbol->Flags == 0);
#endif
	return res;
}

void *CodeviewSymbolProvider::loadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *err)
{
	HANDLE hProcess = (HANDLE)hproc;
	DWORD64 dll_base = (DWORD64)hmod;

	DWORD64 hSymbols = SymLoadModule64(hProcess, NULL, mod_path.toLocal8Bit().data(), 0, dll_base, 0);
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

bool CodeviewSymbolProvider::unloadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *err)
{
	return SymUnloadModule64((HANDLE)hproc, (DWORD64)hmod);
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


	// COBOL-line breakpoints are created here (not installed at hardware level)
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

	int __GIX_SYM_MODULES_C = readSymbolValueAsInt(hproc, "__GIX_SYM_MODULES_C");
	int __GIX_SYM_MODULES_S = readSymbolValueAsInt(hproc, "__GIX_SYM_MODULES_S");
	uint8_t *__GIX_SYM_MODULES = readSymbolValueInBuffer(hproc, "__GIX_SYM_MODULES", __GIX_SYM_MODULES_S);

	if (!__GIX_SYM_MODULES || __GIX_SYM_MODULES_C < 0 || __GIX_SYM_MODULES_S < 0) {
		gd->printMessage("ERROR: cannot parse module debug info (module info)\n");
		delete mi;
		return nullptr;
	}

	SymbolBufferReader sr(__GIX_SYM_MODULES, __GIX_SYM_MODULES_S);
	for (int i = 0; i < __GIX_SYM_MODULES_C; i++) {
		cml.append(sr.readString());
	}

	for (auto m : cml) {

		CobolModuleInfo *cmi = new CobolModuleInfo();
		cmi->name = m;
		cmi->owner = mi;
		cmi->entry_point = (void *)getSymbolAddress(gd, hproc, hmod, QString(m), NULL, err);
		mi->cbl_modules[m] = cmi;
		cmi->entry_breakpoint = UserBreakpoint::createInstance();
		cmi->entry_breakpoint->address = cmi->entry_point;
		cmi->entry_breakpoint->automatic = true;
		cmi->entry_breakpoint->key = QString(m) + ":0";
		cmi->entry_breakpoint->orig_instr = 0x00;
		cmi->entry_breakpoint->owner = mi;

/*
		 TODO: locate entry point line in COBOL source
		IMAGEHLP_LINE64 ln;
		DWORD  dwDisplacement;
		ZeroMemory(&ln, sizeof(IMAGEHLP_LINE64));
		ln.SizeOfStruct = sizeof(IMAGEHLP_LINE64);
		ln.Address = (DWORD64)cmi->entry_point;
		if (SymGetLineFromAddr64(hproc, (DWORD64)cmi->entry_point, &dwDisplacement, &ln)) {
			QString c_src_file = ln.FileName;	// These are C source locations
			mi->owner->

		}
		else {
			cmi->entry_breakpoint->source_file = "";
			cmi->entry_breakpoint->line = 0;
		}
*/
		cmi->entry_breakpoint->source_file = "";
		cmi->entry_breakpoint->line = 0;

		if (!initCobolModuleLocalInfo(gd, hproc, cmi)) {
			gd->printMessage("ERROR: cannot parse module debug info (source lines)\n");
			delete cmi;
			delete mi;
			return nullptr;
		}
	}
	return mi;
}

LibCobInfo *CodeviewSymbolProvider::extractLibCobInfo(GixDebugger *gd, void *hproc, void *hmod, void *hsym, const QString &mod_path, void *userdata, int *err)
{
	void *f_libcob_version = (void *)getSymbolAddress(gd, hproc, hmod, "libcob_version", NULL, err);
	void *f_cob_get_field_str = (void *)getSymbolAddress(gd, hproc, hmod, "cob_get_field_str", NULL, err);
	void *f_cob_put_field_str = (void *)getSymbolAddress(gd, hproc, hmod, "cob_put_field_str", NULL, err);
	void *f_cob_runtime_error = (void *)getSymbolAddress(gd, hproc, hmod, "cob_runtime_error", NULL, err);

	if (f_libcob_version && f_cob_get_field_str && f_cob_put_field_str && f_cob_runtime_error) {
		LibCobInfo *lci = new LibCobInfo();

		lci->f_libcob_version = (f_libcob_version_ptr) f_libcob_version;
		lci->f_cob_get_field_str = (f_cob_get_field_str_ptr) f_cob_get_field_str;
		lci->f_cob_put_field_str = (f_cob_put_field_str_ptr) f_libcob_version;
		lci->f_cob_runtime_error = (f_cob_runtime_error_ptr) f_libcob_version;

		return lci;
	}

	return nullptr;
}

void * CodeviewSymbolProvider::getSymbolAddress(GixDebugger *gd, void *hproc, void *hmod, const QString &sym_name, void *userdata, int *err)
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

	return rc ? (void *) dwAddress : NULL;
}

int CodeviewSymbolProvider::readSymbolValueAsInt(void *hproc, const QString &s)
{
	void *addr = getSymbolAddress(NULL, hproc, NULL, s, NULL, 0);
	if (!addr) {
		this->dbg_instance->printMessage("Cannot locate symbol [" + s + "]");
		return -1;
	}

	SIZE_T st;
	uint8_t bfr[sizeof(int)];
	if (!ReadProcessMemory(hproc, (LPVOID)addr, &bfr, sizeof(int), &st))
		return -2;

	int i = *((int *)&bfr);
	return i;
}

uint8_t *CodeviewSymbolProvider::readSymbolValueInBuffer(void *hproc, const QString &s, int bfrlen)
{
	DWORD64 addr = (DWORD64)getSymbolAddress(NULL, hproc, NULL, s, NULL, 0);
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

	if (__GIX_SYM_MOD_EC < 0|| __GIX_SYM_MOD_ES < 0 || __GIX_SYM_MOD_MC < 0 || __GIX_SYM_MOD_MS < 0 || !__GIX_SYM_MOD_E || !__GIX_SYM_MOD_M) {
		gd->printMessage("ERROR: cannot parse module debug info (COBOL variable data)\n");
		return false;
	}

	SymbolBufferReader sr(__GIX_SYM_MOD_E, __GIX_SYM_MOD_ES);
	for (int i = 0; i < __GIX_SYM_MOD_EC; i++) {
		VariableResolverData *rd = new VariableResolverData();
		rd->var_name = sr.readString();
		rd->var_path = sr.readString();
		rd->type = (WsEntryType) sr.readInt();
		rd->level = sr.readInt();
		rd->base_var_name = sr.readString();
		rd->local_addr = sr.readInt();
		rd->storage_size = sr.readInt();

		rd->display_size = sr.readInt();
		rd->is_signed = sr.readInt();
		rd->decimals = sr.readInt();
		rd->format = sr.readString();
		rd->storage_type = sr.readInt();
		rd->occurs = sr.readInt();
		rd->redefines = sr.readString();

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

#if _DEBUG
	char lbfr[1024];
	sprintf(lbfr, "DLL_EnumLinesProc: retrieving source line info: %s -> %p\n", k.toLocal8Bit().data(), li->addr);
	OutputDebugStringA(lbfr);
#endif

	UserBreakpoint *bkp = UserBreakpoint::createInstance();
	bkp->owner = mi;
	bkp->automatic = true;
	bkp->address = li->addr;
	bkp->key = k;
	bkp->line = li->line;
	bkp->source_file = li->source_file;
	
	mi->owner->breakPointAdd(bkp);
	bkp->install();

	return true;
}
