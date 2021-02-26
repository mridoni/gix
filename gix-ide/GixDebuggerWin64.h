#pragma once

#include "GixDebugger.h"
#include "ISymbolProvider.h"

#include <map>
#include <vector>
#include <string>
#include <functional>

#include <Windows.h>
#include <psapi.h>
#include <Dbghelp.h>
#include <tchar.h>

class GixDebuggerWin64 : public GixDebugger
{
public:
	GixDebuggerWin64();
	~GixDebuggerWin64();

	virtual int start() override;
	void setupEnvironmentBlock();
	virtual int stop() override;
	virtual bool step() override;
	virtual bool continue_running() override;

	virtual bool getVariables(QList<VariableData *> var_list) override;
	virtual QString getCurrentCobolModuleName() override;

	virtual void printLastError() override;
	virtual void removeHardwareBreakpoint(UserBreakpoint *bkp) override;
	virtual bool installHardwareBreakpoint(UserBreakpoint *bkp) override;

	virtual bool readProcessMemory(void *addr, void *bfr, int size) override;

private:
	HANDLE the_process = NULL;
	HANDLE the_thread = NULL;
	DWORD the_process_id = 0;
	DWORD the_thread_id = 0;
	PROCESS_INFORMATION process_info;
	bool __breakpoint_0_hit = false;
	bool is_on_break = false;

	ISymbolProvider *sym_provider = nullptr;
	ISymbolProvider *get_symbol_provider();

	HANDLE hChildStd_OUT_Rd = NULL;
	HANDLE hChildStd_OUT_Wr = NULL;

	HANDLE hChildStd_ERR_Rd = NULL;
	HANDLE hChildStd_ERR_Wr = NULL;

	HANDLE hThreadReadStdOut = NULL;
	HANDLE hThreadReadStdErr = NULL;

	DWORD dwThreadReadStdOutId = 0;
	DWORD dwThreadReadStdErrId = 0;

	char *envBlock = NULL;
	int exit_code = 0;

	bool error_exit = false;

	CobolModuleInfo *current_cbl_module = nullptr;

	static DWORD WINAPI PipeReaderThread(LPVOID lpParam);
	bool stop_reading_pipes = false;

	virtual void *getSymbolAddress(const char *sym_name) override;

	bool isCblEntryPoint(void *addr, CobolModuleInfo **cmi);

	bool processImage(HANDLE hProc, HANDLE imageBase, DWORD64 hSym, QString imageName);

	bool initCobolModuleLocalInfo(GixDebugger *gd, HANDLE hProc, CobolModuleInfo *cmi);

	void inject_helper(HANDLE hproc, HANDLE hthread);
};
