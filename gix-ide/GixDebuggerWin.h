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
#pragma once

#ifdef _WIN32

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

class GixDebuggerWin : public GixDebugger
{
public:
	GixDebuggerWin();
	~GixDebuggerWin();

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

	DWORD getProcessId();

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

	//void inject_helper(HANDLE hproc, HANDLE hthread);

	bool isLoadedGnuCOBOLImage(LPVOID base_addr, SharedModuleInfo **mi);
	bool unloadGnuCOBOLImage(SharedModuleInfo *mi);

	uint32_t extract_base_of_code(HANDLE hProc, void *dll_base);
};

#endif