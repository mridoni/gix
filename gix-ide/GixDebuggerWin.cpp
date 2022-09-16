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

#ifdef _WIN32

#include "GixDebuggerWin.h"
#include "CodeviewSymbolProvider.h"
#include "DwarfSymbolProvider.h"
#include "PathUtils.h"
#include "libcpputils.h"

#include <QDir>

#define BUFSIZE 65536

QString GetFileNameFromHandle(HANDLE hFile);

bool single_step = false;
QString last_source_file;
int last_source_line = 0;


struct stPipeThreadData
{
	GixDebuggerWin *gd;
	int channel;
};

static struct stPipeThreadData threadDataOut;
static struct stPipeThreadData threadDataErr;

#if _DEBUG
typedef BOOL(WINAPI *MINIDUMPWRITEDUMP)(HANDLE hProcess, DWORD dwPid, HANDLE hFile, MINIDUMP_TYPE DumpType, CONST PMINIDUMP_EXCEPTION_INFORMATION ExceptionParam, CONST PMINIDUMP_USER_STREAM_INFORMATION UserStreamParam, CONST PMINIDUMP_CALLBACK_INFORMATION CallbackParam);

void create_minidump(struct _EXCEPTION_POINTERS *apExceptionInfo, DWORD thread_id)
{
	HMODULE mhLib = ::LoadLibrary(_T("dbghelp.dll"));
	MINIDUMPWRITEDUMP pDump = (MINIDUMPWRITEDUMP)::GetProcAddress(mhLib, "MiniDumpWriteDump");

	HANDLE  hFile = ::CreateFile(_T("core.dmp"), GENERIC_WRITE, FILE_SHARE_WRITE, NULL, CREATE_ALWAYS,
		FILE_ATTRIBUTE_NORMAL, NULL);

	_MINIDUMP_EXCEPTION_INFORMATION ExInfo;
	ExInfo.ThreadId = thread_id;
	ExInfo.ExceptionPointers = apExceptionInfo;
	ExInfo.ClientPointers = FALSE;

	pDump(GetCurrentProcess(), GetCurrentProcessId(), hFile, MiniDumpWithFullMemoryInfo, &ExInfo, NULL, NULL);
	::CloseHandle(hFile);
}
#endif

GixDebuggerWin::GixDebuggerWin()
{
}


GixDebuggerWin::~GixDebuggerWin()
{
	if (envBlock)
		delete envBlock;
}

int GixDebuggerWin::start()
{
	STARTUPINFO si;
	int err = 0;
	UserBreakpoint *last_bkp = nullptr;
	char dbg_help_path[1024] = { 0, 0, 0, 0 };

	if (!if_blk) {
		fprintf(stderr, "Missing interface block\n");
		return 1;
	}

	if (is_debugging_enabled) {
		sym_provider = get_symbol_provider();
		if (!sym_provider) {
			if_blk->debuggerError(this, 1, "Cannot instantiate symbol provider");
			return 1;
		}

		auto dbgver = ImagehlpApiVersion();

		HMODULE hDbgHelp = GetModuleHandle("dbghelp.dll");
		if (GetModuleFileName(hDbgHelp, dbg_help_path, sizeof(dbg_help_path)) == 0) {
			int ret = GetLastError();
			fprintf(stderr, "GetModuleFileName failed, error = %d\n", ret);
		}
		if_blk->debuggerMessage(this, QString("Using dbgHelp version: %1.%2.%3: %4").arg(dbgver->MajorVersion).arg(dbgver->MinorVersion).arg(dbgver->Revision).arg(dbg_help_path), 0);
	}

	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);

	ZeroMemory(&process_info, sizeof(process_info));

	setupEnvironmentBlock();

	QString fcl = exepath + " " + cmd_line_args;

	SECURITY_ATTRIBUTES saAttr;
	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL;

	if (!use_external_console) {

		if (!CreatePipe(&hChildStd_OUT_Rd, &hChildStd_OUT_Wr, &saAttr, 0)) {
			if_blk->debuggerError(this, 1, QString("Cannot open stdout pipe"));
			return 1;
		}

		if (!SetHandleInformation(hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0)) {
			if_blk->debuggerError(this, 1, QString("Cannot configure stdout pipe"));
			return 1;
		}

		if (!CreatePipe(&hChildStd_ERR_Rd, &hChildStd_ERR_Wr, &saAttr, 0)) {
			if_blk->debuggerError(this, 1, QString("Cannot open stderr pipe"));
			return 1;
		}

		if (!SetHandleInformation(hChildStd_ERR_Rd, HANDLE_FLAG_INHERIT, 0)) {
			if_blk->debuggerError(this, 1, QString("Cannot configure stderr pipe"));
			return 1;
		}

		si.hStdError = hChildStd_ERR_Wr;
		si.hStdOutput = hChildStd_OUT_Wr;
		si.dwFlags |= STARTF_USESTDHANDLES;

		threadDataOut = { this, 1 };
		hThreadReadStdOut = CreateThread(
			NULL,								// default security attributes
			0,									// use default stack size  
			&GixDebuggerWin::PipeReaderThread,	// thread function name
			(LPVOID)&threadDataOut,				// argument to thread function 
			0,									// use default creation flags 
			&dwThreadReadStdOutId);				// returns the thread identifier 

		if (hThreadReadStdOut == NULL) {
			if_blk->debuggerError(this, 1, QString("Cannot configure stdout reader thread"));
			return 1;
		}

		threadDataErr = { this, 2 };
		hThreadReadStdErr = CreateThread(
			NULL,								// default security attributes
			0,									// use default stack size  
			&GixDebuggerWin::PipeReaderThread,	// thread function name
			(LPVOID)&threadDataErr,				// argument to thread function 
			0,									// use default creation flags 
			&dwThreadReadStdErrId);				// returns the thread identifier 

		if (hThreadReadStdErr == NULL) {
			stop_reading_pipes = true;
			CancelSynchronousIo(hThreadReadStdOut);
			if_blk->debuggerError(this, 1, QString("Cannot configure stderr reader thread"));
			return 1;
		}
	}
	
	DWORD createFlags = NORMAL_PRIORITY_CLASS | DEBUG_ONLY_THIS_PROCESS;
	
	if (use_external_console)
		createFlags |= CREATE_NEW_CONSOLE;

	if (!stdin_file.isEmpty() && QFile::exists(stdin_file)) {

		if (!CreatePipe(&hChildStd_IN_Rd, &hChildStd_IN_Wr, &saAttr, 0)) {
			if_blk->debuggerError(this, 1, QString("Cannot open stdin pipe"));
			return 1;
		}

		if (!SetHandleInformation(hChildStd_IN_Wr, HANDLE_FLAG_INHERIT, 0)) {
			if_blk->debuggerError(this, 1, QString("Cannot configure stdin pipe"));
			return 1;
		}

		si.hStdInput = hChildStd_IN_Rd;
		si.dwFlags |= STARTF_USESTDHANDLES;
		if (use_external_console) {
			si.hStdError = NULL;
			si.hStdOutput = NULL;
		}
	}


	// TODO: verify that this works in all cases (internal/external console, stdir redirected or not, etc.)
	// WAS : if (!CreateProcess(NULL, (LPSTR) fcl.toStdString().c_str(), NULL, NULL, !use_external_console, createFlags, envBlock, working_dir.toStdString().c_str(), &si, &process_info)) { // DEBUG_ONLY_THIS_PROCESS
	if_blk->debuggerMessage(this, QString("Working directory is: [%1]").arg(working_dir), 0);
	if_blk->debuggerMessage(this, QString("CreateProcess called with: [%1]").arg(fcl), 0);
	if (!CreateProcess(NULL, (LPSTR) fcl.toStdString().c_str(), NULL, NULL, TRUE, createFlags, envBlock, working_dir.toStdString().c_str(), &si, &process_info)) { // DEBUG_ONLY_THIS_PROCESS
		TerminateThread(hThreadReadStdOut, 0);
		TerminateThread(hThreadReadStdErr, 0);
		
		if_blk->debuggerError(this, 1, "Cannot start process");
		printLastError();
		return 1;
	}

	if (!stdin_file.isEmpty() && QFile::exists(stdin_file)) {
		HANDLE hStdInFile = CreateFile(
			stdin_file.toLocal8Bit().data(),
			GENERIC_READ,
			0,
			NULL,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_READONLY,
			NULL);

		if (hStdInFile == INVALID_HANDLE_VALUE) {
			if_blk->debuggerError(this, 1, QString("Cannot open stdin file"));
			return 1;
		}

		if (!WriteFileToPipe(hStdInFile, hChildStd_IN_Wr)) {
			if_blk->debuggerError(this, 1, QString("Cannot read or pipe stdin file"));
			return 1;
		}

		CloseHandle(hStdInFile);
	}

	if (!is_debugging_enabled) {
		if_blk->debuggerReady(this, exepath);
	}

	if_blk->debuggerProcessStarted(this, exepath);

	QString strEventMessage;

	DEBUG_EVENT debug_event = { 0 };

	bool bContinueDebugging = true;

	DWORD dwContinueStatus = DBG_CONTINUE;

	DebugSetProcessKillOnExit(TRUE);

	while (bContinueDebugging) {
		if (!WaitForDebugEvent(&debug_event, INFINITE))
			return 1;

		switch (debug_event.dwDebugEventCode) {
			
			case CREATE_PROCESS_DEBUG_EVENT:
			{
				the_process = debug_event.u.CreateProcessInfo.hProcess;
				the_process_id = GetProcessId(the_process);
				the_thread = debug_event.u.CreateProcessInfo.hThread;

				if (!is_debugging_enabled)
					break;

				strEventMessage = "CREATE_PROCESS_DEBUG_EVENT";
				

				sym_provider->initialize(this, the_process, NULL);

				if (debuggee_type == DebuggedModuleType::Executable) {
					QString sEXEName = GetFileNameFromHandle(debug_event.u.CreateProcessInfo.hFile);
					strEventMessage = QString("Loaded EXE: ") + sEXEName;
					DWORD64 hSymbols = (DWORD64)sym_provider->loadSymbols(this, the_process, debug_event.u.CreateProcessInfo.lpBaseOfImage, sEXEName, (void *) debug_event.u.CreateProcessInfo.lpStartAddress, &err);
					if (!processImage(the_process, debug_event.u.CreateProcessInfo.lpBaseOfImage, hSymbols, sEXEName)) {
						error_exit = true;
						if_blk->debuggerError(this, -1, "Error loading image information for: " + sEXEName);
						bContinueDebugging = false;
					}
				}

			}
			break;

			case CREATE_THREAD_DEBUG_EVENT:
				if (!is_debugging_enabled)
					break;

				if_blk->debuggerMessage(this, QString::fromStdString(string_format("Thread 0x%x (Id: %d) created at: 0x%x",
					debug_event.u.CreateThread.hThread,
					debug_event.dwThreadId,
					debug_event.u.CreateThread.lpStartAddress)), 0); // Thread 0xc (Id: 7920) created at: 0x77b15e58
				//strEventMessage = "CREATE_THREAD_DEBUG_EVENT";			
				break;

			case EXIT_THREAD_DEBUG_EVENT:
				if (!is_debugging_enabled)
					break;

				if_blk->debuggerMessage(this, QString::fromStdString(string_format("The thread %d exited with code: %d",
					debug_event.dwThreadId,
					debug_event.u.ExitThread.dwExitCode)), 0);	// The thread 2760 exited with code: 0
				strEventMessage = "EXIT_THREAD_DEBUG_EVENT";
				break;

			case EXIT_PROCESS_DEBUG_EVENT:
				exit_code = (int)debug_event.u.ExitProcess.dwExitCode;
				if_blk->debuggerProcessExit(this, exit_code, exepath);		
#if _DEBUG
				if_blk->debuggerMessage(this, ":: EXIT CODE: " + QString::number(exit_code), 0);
				OutputDebugStringA(string_format("*** %s says goodbye (%d)\n", exepath.toLocal8Bit().constData(), exit_code).c_str());
#endif
				bContinueDebugging = false;
				break;

			case LOAD_DLL_DEBUG_EVENT:
			{
				if (!is_debugging_enabled)
					break;

				QString sDLLName = GetFileNameFromHandle(debug_event.u.LoadDll.hFile);

#if _DEBUG
				char bfr[1024];
				sprintf(bfr, "Gix-IDE debugger: loading DLL: %s at %p\n", sDLLName.toLocal8Bit().data(), debug_event.u.LoadDll.lpBaseOfDll);
				OutputDebugStringA(bfr);
#endif
				QString cp = QDir::cleanPath(sDLLName);
				if (properties["symformat"] == "dwarf" && !cp.startsWith(this->module_dir))
					break;

				strEventMessage = QString("Loaded DLL: ") + sDLLName;
				DWORD64 hSymbols = (DWORD64)sym_provider->loadSymbols(this, the_process, debug_event.u.LoadDll.lpBaseOfDll, sDLLName, NULL, &err);

				if (!processImage(the_process, debug_event.u.LoadDll.lpBaseOfDll, hSymbols, sDLLName)) {
					error_exit = true;
					if_blk->debuggerError(this, -1, "Error loading image information for: " + sDLLName);
					bContinueDebugging = false;
				}

			}
			break;

			case UNLOAD_DLL_DEBUG_EVENT:
			{
				if (!is_debugging_enabled)
					break;

				strEventMessage = "UNLOAD_DLL_DEBUG_EVENT";

				SharedModuleInfo *mi = nullptr;
				if (isLoadedGnuCOBOLImage(debug_event.u.UnloadDll.lpBaseOfDll, &mi)) {
#if _DEBUG
					char bfr[1024];
					sprintf(bfr, "Gix-IDE debugger: unloading DLL: %s\n", mi->dll_path.toLocal8Bit().data());
					OutputDebugStringA(bfr);
#endif
					unloadGnuCOBOLImage(mi);
				}

			}
				break;

			case OUTPUT_DEBUG_STRING_EVENT:
			{
				if (!is_debugging_enabled)
					break;

				OUTPUT_DEBUG_STRING_INFO &DebugString = debug_event.u.DebugString;
				// LPSTR p = ;

				char *msg = new char[DebugString.nDebugStringLength];
				ZeroMemory(msg, DebugString.nDebugStringLength);

				ReadProcessMemory(process_info.hProcess, DebugString.lpDebugStringData, msg, DebugString.nDebugStringLength, NULL);

				if (DebugString.fUnicode)
					strEventMessage = msg;
				else
					strEventMessage = (LPSTR)msg;

				_DBG_OUT(msg);

				delete[]msg;
			}

			break;

			case EXCEPTION_DEBUG_EVENT:
			{
				if (!is_debugging_enabled)
					break;

				strEventMessage = "";
				EXCEPTION_DEBUG_INFO &exception = debug_event.u.Exception;

				switch (exception.ExceptionRecord.ExceptionCode) {
					case EXCEPTION_BREAKPOINT:
					{
						if (!__breakpoint_0_hit) {
							strEventMessage = "Break point 0";
							__breakpoint_0_hit = true;
							//inject_helper(the_process, the_thread);
							if_blk->debuggerReady(this, exepath);
							_DBG_OUT("EXCEPTION_BREAKPOINT: Breakpoint 0 hit\n");
							break;
						}

						_DBG_OUT("EXCEPTION_BREAKPOINT: Breakpoint hit\n");

						PVOID ex_addr = exception.ExceptionRecord.ExceptionAddress;

						bool is_cbl_entry_point = isCblEntryPoint(ex_addr, &current_cbl_module);

						getAndResolveUserBreakpoints();
						
						UserBreakpoint *bp = findBreakpointByAddress(ex_addr);
						if (!bp) {	// This will probably lead to a crash
#if _DEBUG
							char bbfr[256];
							sprintf(bbfr, "EXCEPTION_BREAKPOINT: Breakpoint not found: %p\n", ex_addr);
							OutputDebugStringA(bbfr);
#endif
							break;
						}

						_DBG_OUT("EXCEPTION_BREAKPOINT: Breakpoint at address %p is at source line %d of %s\n", bp->address, bp->line, bp->source_file.toLocal8Bit().data());

						CONTEXT lcContext;
						lcContext.ContextFlags = CONTEXT_ALL;
						
						// We need to get the handle of the thread that actually generated the breakpoint exception, 
						// that not necessarily is the main application thread we stored
						HANDLE h_exc_thread = OpenThread(THREAD_ALL_ACCESS, FALSE, debug_event.dwThreadId);
						GetThreadContext(h_exc_thread, &lcContext);
#ifdef _WIN64
						DWORD64 dwWriteSize;
						lcContext.Rip--; // Move back one byte, for 64bit
#else
						DWORD dwWriteSize;
						lcContext.Eip--; // Move back one byte, for 32bit
#endif
						lcContext.EFlags |= 0x100;

						SetThreadContext(h_exc_thread, &lcContext);
						CloseHandle(h_exc_thread);

						bp->uninstall();
						_DBG_OUT("EXCEPTION_BREAKPOINT: Hardware breakpoint uninstalled at %p\n", bp->address);

						last_bkp = bp;
						last_source_file = bp->source_file;
						last_source_line = bp->line;
					}
					break;

					case EXCEPTION_SINGLE_STEP:
					{
						if (!is_debugging_enabled)
							break;

						_DBG_OUT("EXCEPTION_SINGLE_STEP: Breakpoint single-step\n");

						is_on_break = true;
#ifdef _WIN64
						DWORD64 dwWriteSize;
#else
						DWORD dwWriteSize;
#endif
						uint8_t brk_inst = 0xcc;
						if (last_bkp) {
							last_bkp->install();
							_DBG_OUT("EXCEPTION_SINGLE_STEP: Reinstalled hardware breakpoint at %p\n", last_bkp->address);

							if (!last_bkp->automatic || single_step) {
								_DBG_OUT("EXCEPTION_SINGLE_STEP: User/single step breakpoint: yes\n");
								if (this->source_lines_by_addr.find(last_bkp->address) != source_lines_by_addr.end()) {
									_DBG_OUT("EXCEPTION_SINGLE_STEP: Successfully decoded source line info\n");
									SourceLineInfo *sli = source_lines_by_addr[last_bkp->address];
									if_blk->debuggerMessage(this, QString::fromStdString(string_format("Found breakpoint at 0x%08p (%s:%d)\n", last_bkp->address, sli->source_file, sli->line)), 0);
									// Check if breakpoint is on the first line of a generated preprocessor block
									// If yes, pass the line corresponding to the start of the original block
									int actual_line = 0;
									if (!is_first_line_of_preproc_block(current_cbl_module, sli->source_file, sli->line, &actual_line)) {
									if_blk->debuggerBreak(this, current_cbl_module->name, sli->source_file, sli->line);
										_DBG_OUT("Debugger breaking at: %s:%d\n", sli->source_file.toLocal8Bit().constData(), sli->line);
									}
									else {
										if_blk->debuggerBreak(this, current_cbl_module->name, sli->source_file, actual_line);
										_DBG_OUT("Debugger breaking at: %s:%d\n", sli->source_file.toLocal8Bit().constData(), actual_line);
									}
								}
							}
							else {
								_DBG_OUT("EXCEPTION_SINGLE_STEP: User/single step breakpoint: no\n");
							}
						}

						is_on_break = false;
						last_bkp = NULL;
					}
					break;

					default:
						if (exception.ExceptionRecord.ExceptionCode == 0x406D1388)	// Handle SetThreadName-generated exceptions
							break;

						printLastError();

						fprintf(stderr, "Exception code: %08x", exception.ExceptionRecord.ExceptionCode);

						if (exception.dwFirstChance == 1) {
							if (is_debugging_enabled) {

								HANDLE h_exc_thread = OpenThread(THREAD_ALL_ACCESS, FALSE, debug_event.dwThreadId);

								QString stack_dump = sym_provider->dumpStackFrame(this, the_process, h_exc_thread);
								if (stack_dump.isEmpty())
									stack_dump = "not available";
								else
									stack_dump = "\n" + stack_dump;
#if _DEBUG
								EXCEPTION_POINTERS eptrs;
								GetThreadContext(h_exc_thread, eptrs.ContextRecord);
								eptrs.ExceptionRecord = &exception.ExceptionRecord;
								create_minidump(&eptrs, debug_event.dwThreadId);
#endif
								CloseHandle(h_exc_thread);

								strEventMessage = QString::fromStdString(string_format("First chance exception at %016p, exception-code: 0x%08x\nStack frame: %s\n================\n",
									exception.ExceptionRecord.ExceptionAddress,
									exception.ExceptionRecord.ExceptionCode,
									stack_dump.toLocal8Bit().constData()));
							}
							else {
								strEventMessage = QString::fromStdString(string_format("First chance exception at %016p, exception-code: 0x%08x\nStack frame: not available\n================\n",
									exception.ExceptionRecord.ExceptionAddress,
									exception.ExceptionRecord.ExceptionCode));
							}

							error_exit = true;
							if_blk->debuggerError(this, exception.ExceptionRecord.ExceptionCode, strEventMessage);
						}
						// else
						// { Let the OS handle }


						// There are cases where OS ignores the dwContinueStatus, 
						// and executes the process in its own way.
						// For first chance exceptions, this parameter is not-important
						// but still we are saying that we have NOT handled this event.

						// Changing this to DBG_CONTINUE (for 1st chance exception also), 
						// may cause same debugging event to occur continously.
						// In short, this debugger does not handle debug exception events
						// efficiently, and let's keep it simple for a while!
						//dwContinueStatus = DBG_EXCEPTION_NOT_HANDLED;
				}

				break;
			}
		}

		

		if (strEventMessage != "")
			if_blk->debuggerMessage(this, strEventMessage, 0);

		if (!bContinueDebugging)
			break;

		ContinueDebugEvent(debug_event.dwProcessId,
			debug_event.dwThreadId,
			dwContinueStatus);

		// Reset
		dwContinueStatus = DBG_CONTINUE;
	}

	// We give the pipe reader threads enough time to read and display output
	Sleep(200);

	if_blk->debuggerMessage(this, "Canceling reader threads", 0);
	
	if (!use_external_console) {
		stop_reading_pipes = true;
		CancelSynchronousIo(hThreadReadStdOut);
		CancelSynchronousIo(hThreadReadStdErr);
		WaitForSingleObject(hThreadReadStdOut, 500);
		WaitForSingleObject(hThreadReadStdErr, 500);

		CloseHandle(hThreadReadStdOut);
		CloseHandle(hThreadReadStdErr);

		CloseHandle(hChildStd_OUT_Rd);
		CloseHandle(hChildStd_ERR_Rd);
	}

	if (is_debugging_enabled) {
		sym_provider->deinit(this, the_process);
		delete sym_provider;
		SymCleanup(the_process);
	}

	//CloseHandle(the_process);
	//CloseHandle(the_thread);
	DebugActiveProcessStop(the_process_id);


	return 0;
}



void GixDebuggerWin::setupEnvironmentBlock()
{
	int sz = 0;
	int pos = 0;
	for (auto it = environment.begin(); it != environment.end(); ++it) {
		sz += (it.key().length() + it.value().length() + 2);	// 2 = "=", \0
	}

	sz += 1;	//block terminating zero

	envBlock = new char[sz];
	for (auto it = environment.begin(); it != environment.end(); ++it) {
#if _DEBUG
		char bfr[65535];
		sprintf(bfr, "Process environment - %s : %s\n", it.key().toLocal8Bit().constData(), it.value().toLocal8Bit().constData());
		OutputDebugStringA(bfr);
#endif
		memcpy(envBlock + pos, it.key().toLocal8Bit().constData(), it.key().length()); pos += it.key().length();
		*(envBlock + pos) = '='; pos++;

		memcpy(envBlock + pos, it.value().toLocal8Bit().constData(), it.value().length()); pos += it.value().length();
		*(envBlock + pos) = '\0'; pos++;
	}
	*(envBlock + pos) = '\0';
}

int GixDebuggerWin::stop()
{
	DWORD rc;

	GetExitCodeProcess(the_process, &rc);
	if (rc == STILL_ACTIVE) {
		exit_code = 0xDEAD;
		BOOL b = TerminateProcess(the_process, exit_code);
		const DWORD result = WaitForSingleObject(the_process, 3000);
	}

	//if (!error_exit && the_process)
	//	if_blk->debuggerProcessExit(this, exit_code, GetFileNameFromHandle(the_process));
	
	return exit_code;
}

bool GixDebuggerWin::step()
{
	//SetEvent(dbg_brk_handle);
	single_step = true;
	return true;
}

bool GixDebuggerWin::continue_running()
{
	single_step = false;
	return true;
}

void GixDebuggerWin::printLastError()
{
	if (!verbose)
		return;

	LPVOID lpMsgBuf;
	LPVOID lpDisplayBuf = NULL;
	DWORD dw = GetLastError();

	FormatMessage(
		FORMAT_MESSAGE_ALLOCATE_BUFFER |
		FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		dw,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR)&lpMsgBuf,
		0, NULL);

	if_blk->debuggerMessage(this, (char *)lpMsgBuf, 0);

	OutputDebugStringA((char *)lpMsgBuf);
	LocalFree(lpDisplayBuf);
}

bool GixDebuggerWin::getVariables(QList<VariableData *> var_list)
{
	if (!is_on_break)
		return false;

	if (!var_list.size())
		return true;


	CONTEXT lcContext;
	lcContext.ContextFlags = CONTEXT_ALL;
	GetThreadContext(process_info.hThread, &lcContext);

#ifdef _WIN64
	DWORD64 dwReadBytes;
	unsigned long long frame_ptr = lcContext.Rdi;
#else
	DWORD dwReadBytes;
	unsigned long frame_ptr = lcContext.Edi;
#endif

	for (VariableData *vd : var_list) {

		// Resolve COBOL variable name to a local symbol in the current stack frame + offset
		if (current_cbl_module->locals.contains(vd->var_name)) {

			VariableResolverData *vrd = current_cbl_module->locals[vd->var_name];
			if (current_cbl_module->locals.contains(vrd->base_var_name)) {
				
				VariableResolverData *vrootvar = current_cbl_module->locals[vrd->base_var_name];
#ifdef _WIN64
				unsigned long long addr = (unsigned long long) sym_provider->resolveLocalVariableAddress(this, the_process, current_cbl_module, frame_ptr, vrootvar, vrd);
#else
				unsigned long addr = (unsigned long) sym_provider->resolveLocalVariableAddress(this, the_process, current_cbl_module, frame_ptr, vrootvar, vrd);
#endif
				vd->data = new uint8_t[vrd->storage_size];
				vd->resolver_data = vrd;

				memset(vd->data, 0x00, vrd->storage_size);

				if (!ReadProcessMemory(the_process, (LPCVOID)addr, vd->data, vrd->storage_size, &dwReadBytes)) {
					this->printLastError();
					continue;
				}
			}
		}
	}

	return true;
}

bool GixDebuggerWin::readProcessMemory(void *addr, void *bfr, int size)
{
	SIZE_T dwReadBytes;

	if (!addr || !bfr ||!size)
		return false;

	return ReadProcessMemory(the_process, (void *)addr, bfr, size, &dwReadBytes) && (dwReadBytes == size);
}

DWORD GixDebuggerWin::getProcessId()
{
	return the_process_id;
}

HANDLE GixDebuggerWin::getProcess()
{
	return the_process;
}

QString GixDebuggerWin::getCurrentCobolModuleName()
{
	if (current_cbl_module)
		return current_cbl_module->name;

	return QString();
}

struct injection_data_t
{
	int opcode;
	unsigned char opdata1[256];
	unsigned char opdata2[1024];
};

#pragma check_stack (off)

static void AfterThreadProc(void) {}

#pragma check_stack 


ISymbolProvider *GixDebuggerWin::get_symbol_provider()
{
	if (properties.find("symformat") == properties.end())
		return nullptr;

	if (properties["symformat"] == "pdb")
		return new CodeviewSymbolProvider();

	if (properties["symformat"] == "dwarf")
		return new DwarfSymbolProvider();

	return nullptr;
}


QString GetFileNameFromHandle(HANDLE hFile)
{
	BOOL bSuccess = FALSE;
	TCHAR pszFilename[MAX_PATH + 1];
	HANDLE hFileMap;

	QString strFilename;

	if (!hFile)
		return strFilename;

	// Get the file size.
	DWORD dwFileSizeHi = 0;
	DWORD dwFileSizeLo = GetFileSize(hFile, &dwFileSizeHi);

	if (dwFileSizeLo == 0 && dwFileSizeHi == 0) {
		return FALSE;
	}

	// Create a file mapping object.
	hFileMap = CreateFileMapping(hFile,
		NULL,
		PAGE_READONLY,
		0,
		1,
		NULL);

	if (hFileMap) {
		// Create a file mapping to get the file name.
		void *pMem = MapViewOfFile(hFileMap, FILE_MAP_READ, 0, 0, 1);

		if (pMem) {
			if (GetMappedFileName(GetCurrentProcess(),
				pMem,
				pszFilename,
				MAX_PATH)) {

				// Translate path with device name to drive letters.
				TCHAR szTemp[BUFSIZE];
				szTemp[0] = '\0';

				if (GetLogicalDriveStrings(BUFSIZE - 1, szTemp)) {
					TCHAR szName[MAX_PATH];
					TCHAR szDrive[3] = TEXT(" :");
					BOOL bFound = FALSE;
					TCHAR *p = szTemp;

					do {
						// Copy the drive letter to the template string
						*szDrive = *p;

						// Look up each device name
						if (QueryDosDevice(szDrive, szName, MAX_PATH)) {
							size_t uNameLen = _tcslen(szName);

							if (uNameLen < MAX_PATH) {
								bFound = _tcsnicmp(pszFilename, szName,
									uNameLen) == 0;

								if (bFound) {
									//strFilename.Format(L"%s%s", szDrive, pszFilename + uNameLen);							
									QString drive = QString((const char *)szDrive);
									QString fn = QString((const char *)pszFilename);

									strFilename = drive + fn.mid(uNameLen);
								}
							}
						}

						// Go to the next NULL character.
						while (*p++);
					} while (!bFound && *p); // end of string
				}
			}
			bSuccess = TRUE;
			UnmapViewOfFile(pMem);
		}

		CloseHandle(hFileMap);
	}

	return(strFilename);
}

DWORD WINAPI GixDebuggerWin::PipeReaderThread(LPVOID lpParam)
{
	DWORD dwRead;
	CHAR chBuf[BUFSIZE];
	BOOL bSuccess = FALSE;
	HANDLE hPipe;

	stPipeThreadData *data = (stPipeThreadData *)lpParam;

	hPipe = (data->channel == 1) ? data->gd->hChildStd_OUT_Rd : data->gd->hChildStd_ERR_Rd;
	while (!data->gd->stop_reading_pipes) {
		bSuccess = ReadFile(hPipe, chBuf, BUFSIZE, &dwRead, NULL);
		if (!bSuccess || dwRead == 0) {
			break;
		}

		chBuf[dwRead] = 0;

		if (data->channel == 1)
			data->gd->if_blk->debuggerStdOutAvailable(data->gd, chBuf);
		else
			data->gd->if_blk->debuggerStdErrAvailable(data->gd, chBuf);
	}
	return 0;
}

void *GixDebuggerWin::getSymbolAddress(const char *sym_name)
{
	//return (void *)sym_provider->getSymbolAddress(this, the_process, libcob_base, sym_name, NULL, NULL);
	return nullptr;
}

uint32_t GixDebuggerWin::extract_base_of_code(HANDLE hProc, void *dll_base)
{
	uint8_t mz_header[0x3f + 1];
	uint8_t pe_header[0x51 + 1];

	ZeroMemory(mz_header, sizeof(mz_header));

	if (!ReadProcessMemory(hProc, dll_base, mz_header, sizeof(mz_header), NULL))
		return 0;

	uint32_t pe_hdr_offset = (uint32_t) *((uint32_t *)&mz_header[0x3c]);

#if BITNESS==64
	uint64_t pe_header_addr = (uint64_t)dll_base + pe_hdr_offset;
#else
	uint32_t pe_header_addr = (uint32_t)dll_base + pe_hdr_offset;
#endif
	if (!ReadProcessMemory(hProc, (LPCVOID)pe_header_addr, pe_header, sizeof(pe_header), NULL)) {
		printLastError();
		return 0;
	}

	uint32_t base_of_code = (uint32_t) *((uint32_t *)&pe_header[0x2c]);

	return base_of_code;
}

bool GixDebuggerWin::processImage(HANDLE hProc, HANDLE imageBase, DWORD64 hSym, QString imageName)
{
	int err = 0;
	if (hSym) {
		if_blk->debuggerMessage(this, ":: Loaded symbols for " + imageName, 0);
		if (sym_provider->isGnuCOBOLModule(this, the_process, imageBase, NULL, &err)) {
			if_blk->debuggerMessage(this, imageName + " is a GnuCOBOL module", 0);
			uint32_t base_of_code = extract_base_of_code(hProc, imageBase);
			SharedModuleInfo *smi = sym_provider->extractModuleDebugInfo(this, the_process, imageBase, (void *)hSym, imageName, (void *) base_of_code, &err);

			if (smi) {
				shared_modules.push_back(smi);
				for (auto it = smi->cbl_modules.begin(); it != smi->cbl_modules.end(); ++it) {
					if_blk->debuggerMessage(this, QString("Installing hardware breakpoint for module " + it.value()->name), 0);
					it.value()->entry_breakpoint->install();
					breakPointAdd(it.value()->entry_breakpoint);
				}
				getAndResolveUserBreakpoints();
			}
			else
				return false;
		}
		else {
			if (err) {

			}

			if_blk->debuggerMessage(this, imageName + " is NOT a GnuCOBOL module", 0);
			if (PathUtils::getFilename(imageName.toLower()).startsWith("libcob.")) {
				uint32_t base_of_code = extract_base_of_code(hProc, imageBase);
				libcob_info = sym_provider->extractLibCobInfo(this, the_process, imageBase, (void *)hSym, imageName, (void *)base_of_code, &err);
			}
		}

	}

	return true;
}

bool GixDebuggerWin::initCobolModuleLocalInfo(GixDebugger *gd, HANDLE hProc, CobolModuleInfo *cmi)
{
	return sym_provider->initCobolModuleLocalInfo(gd, hProc, cmi);
}

bool GixDebuggerWin::isLoadedGnuCOBOLImage(LPVOID base_addr, SharedModuleInfo **p_mi)
{
	for (SharedModuleInfo *mi : shared_modules) {
		if (mi->dll_base == base_addr) {
			*p_mi = mi;
			return true;
		}
	}
	return false;
}

bool GixDebuggerWin::unloadGnuCOBOLImage(SharedModuleInfo *mi)
{
	if (!mi)
		return false;

	if_blk->debuggerMessage(this, "Shared module " + mi->dll_path + " is being unloaded", 0);

	QList<UserBreakpoint *> to_be_removed;
	for (auto it = this->breakpoints.begin(); it != this->breakpoints.end(); ++it) {
		UserBreakpoint *bkp = *it;
		if (bkp->owner == mi) {
			//removeHardwareBreakpoint(it.value());
			to_be_removed.append(bkp);
		}
	}

	for (UserBreakpoint *bkp: to_be_removed) {
		breakPointRemove(bkp);
	}

	for (auto cmi : mi->cbl_modules.values()) {
		if (cmi && cmi->entry_breakpoint) {
			//removeHardwareBreakpoint(cmi->entry_breakpoint);
		}
	}

	bool b = sym_provider->unloadSymbols(this, the_process, mi->dll_base, mi->dll_path, NULL, 0);
	shared_modules.removeOne(mi);
	
	delete mi;

	return true;
}

bool GixDebuggerWin::WriteFileToPipe(HANDLE input_file, HANDLE the_pipe)

// Read from a file and write its contents to the pipe for the child's STDIN.
// Stop when there is no more data. 
{
	DWORD dwRead, dwWritten;
	CHAR chBuf[BUFSIZE];
	BOOL bSuccess = FALSE;

	for (;;) {
		bSuccess = ReadFile(input_file, chBuf, BUFSIZE, &dwRead, NULL);
		if (!bSuccess || dwRead == 0) break;

		bSuccess = WriteFile(the_pipe, chBuf, dwRead, &dwWritten, NULL);
		if (!bSuccess) break;
	}

	// Close the pipe handle so the child process stops reading. 

	if (!CloseHandle(the_pipe))
		return false;

	return bSuccess;
}

bool WinUserBreakpoint::install()
{
	uint8_t cInstruction = 0x00;
	SIZE_T dwReadBytes;

	if (isInstalled()) {
        _DBG_OUT("Breakpoint at 0x%p for %d@%s is already installed, skipping\n", this->address, this->line, this->source_file.toLocal8Bit().constData());
		return true;
	}

	if (!this->address)
		return false;

	GixDebuggerWin *gdwin = (GixDebuggerWin *) this->owner->owner;

	_DBG_OUT("Installing hardware breakpoint at 0x%p for %d@%s\n", this->address, this->line, this->source_file.toLocal8Bit().constData());

	// Read the instruction    
	if (!ReadProcessMemory(gdwin->getProcess(), (void *)this->address, &cInstruction, 1, &dwReadBytes)) {
		gdwin->printLastError();
		return false;
	}

	// Save it!
	this->orig_instr = cInstruction;

	// Replace it with Breakpoint
	cInstruction = 0xCC;
	if (!WriteProcessMemory(gdwin->getProcess(), this->address, &cInstruction, 1, &dwReadBytes)) {
		gdwin->printLastError();
		return false;
	}

	if (!FlushInstructionCache(gdwin->getProcess(), this->address, 1)) {
		gdwin->printLastError();
		return false;
	}

	_DBG_OUT("Successfully installed hardware breakpoint at 0x%p for %d@%s\n", this->address, this->line, this->source_file.toLocal8Bit().constData());

	return true;
}

bool WinUserBreakpoint::uninstall()
{
	uint8_t cInstruction;
	SIZE_T dwWrittenBytes;

	if (!isInstalled()) {
		_DBG_OUT("Breakpoint at 0x%p for %d@%s is not installed, skipping\n", this->address, this->line, this->source_file.toLocal8Bit().constData());
		return true;
	}

	if (!this->address)
		return false;

	_DBG_OUT("Uninstalling hardware breakpoint at 0x%p for %d@%s\n", this->address, this->line, this->source_file.toLocal8Bit().constData());

	GixDebuggerWin *gdwin = (GixDebuggerWin *)this->owner->owner;

	cInstruction = this->orig_instr;
	if (!WriteProcessMemory(gdwin->getProcess(), this->address, &cInstruction, 1, &dwWrittenBytes)) {
		gdwin->printLastError();
		return false;
	}
	else {
		if (!FlushInstructionCache(gdwin->getProcess(), this->address, 1)) {
			gdwin->printLastError();
			return false;
		}
	}

	this->orig_instr = 0x00;

	_DBG_OUT("Successfully uninstalled hardware breakpoint at 0x%p for %d@%s\n", this->address, this->line, this->source_file.toLocal8Bit().constData());
	
	return true;
}

#endif
