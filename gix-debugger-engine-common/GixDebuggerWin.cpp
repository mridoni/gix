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
#include "libcpputils.h"

#include <sstream>

#define BUFSIZE 65536

std::string GetFileNameFromHandle(HANDLE hFile);

bool single_step = false;
std::string last_source_file;
int last_source_line = 0;


struct stPipeThreadData
{
	GixDebuggerWin *gd;
	PipeChannelType channel_type;
};

static struct stPipeThreadData threadDataOut;
static struct stPipeThreadData threadDataErr;

#if _DEBUG
typedef BOOL(WINAPI *MINIDUMPWRITEDUMP)(HANDLE hProcess, DWORD dwPid, HANDLE hFile, MINIDUMP_TYPE DumpType, CONST PMINIDUMP_EXCEPTION_INFORMATION ExceptionParam, CONST PMINIDUMP_USER_STREAM_INFORMATION UserStreamParam, CONST PMINIDUMP_CALLBACK_INFORMATION CallbackParam);

void create_minidump(struct _EXCEPTION_POINTERS *apExceptionInfo, DWORD thread_id)
{
	HMODULE mhLib = ::LoadLibrary(_T("dbghelp.dll"));
	if (!mhLib) {
		//log("Cannot lod dbghelp.dll, minidump will not be created", LOG_ERR);
		return;
	}

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

	std::hash<std::thread::id> hasher;
	std::ostringstream ss;
	ss << std::this_thread::get_id();
	std::string idstr = ss.str();
	spdlog::info("GixDebugger (Windows) is starting (thread id: {})", idstr);

	if (is_debugging_enabled()) {
		sym_provider = get_symbol_provider();
		if (!sym_provider) {
			debug_driver->dbgr_client_debuggerError(this, 1, "Cannot instantiate symbol provider");
			
			return 1;
		}

		auto dbgver = ImagehlpApiVersion();

		HMODULE hDbgHelp = GetModuleHandle("dbghelp.dll");
		if (GetModuleFileName(hDbgHelp, dbg_help_path, sizeof(dbg_help_path)) == 0) {
			int ret = GetLastError();
			spdlog::warn("GetModuleFileName failed, error = {}", ret);
		}
		spdlog::debug("Using dbgHelp version: {}.{}.{}: {}", dbgver->MajorVersion, dbgver->MinorVersion, dbgver->Revision, dbg_help_path);
	}

	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);

	ZeroMemory(&process_info, sizeof(process_info));

	setupEnvironmentBlock();

	std::string fcl = exepath + " " + cmd_line_args;

	SECURITY_ATTRIBUTES saAttr;
	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL;

	if (!use_external_console) {

		if (!CreatePipe(&hChildStd_OUT_Rd, &hChildStd_OUT_Wr, &saAttr, 0)) {
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot open stdout pipe"));
			return 1;
		}

		if (!SetHandleInformation(hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0)) {
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot configure stdout pipe"));
			
			return 1;
		}

		if (!CreatePipe(&hChildStd_ERR_Rd, &hChildStd_ERR_Wr, &saAttr, 0)) {
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot open stderr pipe"));
			
			return 1;
		}

		if (!SetHandleInformation(hChildStd_ERR_Rd, HANDLE_FLAG_INHERIT, 0)) {
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot configure stderr pipe"));
			
			return 1;
		}

		si.hStdError = hChildStd_ERR_Wr;
		si.hStdOutput = hChildStd_OUT_Wr;
		si.dwFlags |= STARTF_USESTDHANDLES;

		threadDataOut = { this, PipeChannelType::Out };
		hThreadReadStdOut = CreateThread(
			NULL,								// default security attributes
			0,									// use default stack size  
			&GixDebuggerWin::PipeReaderThread,	// thread function name
			(LPVOID)&threadDataOut,				// argument to thread function 
			0,									// use default creation flags 
			&dwThreadReadStdOutId);				// returns the thread identifier 

		if (hThreadReadStdOut == NULL) {
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot configure stdout reader thread"));
			
			return 1;
		}

		threadDataErr = { this, PipeChannelType::Err };
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
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot configure stderr reader thread"));
			
			return 1;
		}
	}
	
	DWORD createFlags = NORMAL_PRIORITY_CLASS | DEBUG_ONLY_THIS_PROCESS;
	
	if (use_external_console)
		createFlags |= CREATE_NEW_CONSOLE;

	if (!stdin_file.empty() && file_exists(stdin_file)) {

		if (!CreatePipe(&hChildStd_IN_Rd, &hChildStd_IN_Wr, &saAttr, 0)) {
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot open stdin pipe"));
			
			return 1;
		}

		if (!SetHandleInformation(hChildStd_IN_Wr, HANDLE_FLAG_INHERIT, 0)) {
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot configure stdin pipe"));
			
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
	spdlog::debug("Working directory is: [{}]", working_dir);
	spdlog::debug("CreateProcess called with: [{}]", fcl);
	if (!CreateProcess(NULL, (LPSTR) fcl.c_str(), NULL, NULL, TRUE, createFlags, envBlock, working_dir.c_str(), &si, &process_info)) { // DEBUG_ONLY_THIS_PROCESS
		TerminateThread(hThreadReadStdOut, 0);
		TerminateThread(hThreadReadStdErr, 0);
		
		spdlog::error("Cannot start process");

		debug_driver->dbgr_client_debuggerError(this, 1, "Cannot start process");
		printLastError();
		
		return 1;
	}

	spdlog::trace("Process launched, PID is {}, thread ID is {}", process_info.dwProcessId, process_info.dwThreadId);

	target_is_running = true;

	if (!stdin_file.empty() && file_exists(stdin_file)) {
		HANDLE hStdInFile = CreateFile(
			stdin_file.c_str(),
			GENERIC_READ,
			0,
			NULL,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_READONLY,
			NULL);

		if (hStdInFile == INVALID_HANDLE_VALUE) {
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot open stdin file"));
			
			return 1;
		}

		if (!WriteFileToPipe(hStdInFile, hChildStd_IN_Wr)) {
			debug_driver->dbgr_client_debuggerError(this, 1, std::string("Cannot read or pipe stdin file"));
			
			return 1;
		}

		CloseHandle(hStdInFile);
	}

	if (!is_debugging_enabled()) {
		spdlog::trace("Sending \"ready\" message to client");
		debug_driver->dbgr_client_debuggerReady(this, exepath);
	}

	spdlog::trace("Sending \"started\" message to client");
	debug_driver->dbgr_client_debuggerProcessStarted(this, exepath);

	std::string strEventMessage;

	DEBUG_EVENT debug_event = { 0 };

	bool bContinueDebugging = true;

	DWORD dwContinueStatus = DBG_CONTINUE;

	DebugSetProcessKillOnExit(TRUE);

	while (bContinueDebugging) {
		if (!WaitForDebugEvent(&debug_event, INFINITE)) {
			
			return 1;
		}

		switch (debug_event.dwDebugEventCode) {
			
			case CREATE_PROCESS_DEBUG_EVENT:
			{
				the_process = debug_event.u.CreateProcessInfo.hProcess;
				the_process_id = GetProcessId(the_process);
				the_thread = debug_event.u.CreateProcessInfo.hThread;

				if (!is_debugging_enabled())
					break;

				strEventMessage = "CREATE_PROCESS_DEBUG_EVENT";
				

				sym_provider->initialize(this, the_process, NULL);

				if (debuggee_type == DebuggedModuleType::Executable) {
					std::string exe_image_name = GetFileNameFromHandle(debug_event.u.CreateProcessInfo.hFile);
					strEventMessage = std::string("Loaded EXE: ") + exe_image_name;
					DWORD64 hSymbols = (DWORD64)sym_provider->loadSymbols(this, the_process, debug_event.u.CreateProcessInfo.lpBaseOfImage, exe_image_name, (void *) debug_event.u.CreateProcessInfo.lpStartAddress, &err);
					if (!processImage(the_process, debug_event.u.CreateProcessInfo.lpBaseOfImage, hSymbols, exe_image_name)) {
						spdlog::warn("Error loading image information for: {}", exe_image_name);
						error_exit = true;
						debug_driver->dbgr_client_debuggerError(this, -1, "Error loading image information for: " + exe_image_name);
						bContinueDebugging = false;
					}
					image_name = exe_image_name;
				}

			}
			break;

			case CREATE_THREAD_DEBUG_EVENT:
				if (!is_debugging_enabled())
					break;

				spdlog::trace("Thread {} (Id: {}) created at: {}",
					debug_event.u.CreateThread.hThread,
					debug_event.dwThreadId,
					(void *)debug_event.u.CreateThread.lpStartAddress); // Thread 0xc (Id: 7920) created at: 0x77b15e58
				strEventMessage = "CREATE_THREAD_DEBUG_EVENT";			
				break;

			case EXIT_THREAD_DEBUG_EVENT:
				if (!is_debugging_enabled())
					break;

				spdlog::trace("The thread {} exited with code: {}",
					debug_event.dwThreadId,
					debug_event.u.ExitThread.dwExitCode);
				strEventMessage = "EXIT_THREAD_DEBUG_EVENT";
				break;

			case EXIT_PROCESS_DEBUG_EVENT:
				exit_code = (int)debug_event.u.ExitProcess.dwExitCode;
				debug_driver->dbgr_client_debuggerProcessExit(this, exit_code, exepath);		
				spdlog::debug(":: Process exit code: {}", exit_code);
				bContinueDebugging = false;
				break;

			case LOAD_DLL_DEBUG_EVENT:
			{
				if (!is_debugging_enabled())
					break;

				std::string dll_image_name = GetFileNameFromHandle(debug_event.u.LoadDll.hFile);
				spdlog::trace("Gix-IDE debugger: loading DLL: {} at {}", dll_image_name, debug_event.u.LoadDll.lpBaseOfDll);

				std::string cp = filename_clean_path(dll_image_name);
				if (properties["symformat"] == "dwarf" && !starts_with(cp, this->module_dir))
					break;

				strEventMessage = std::string("Loaded DLL: ") + dll_image_name;
				DWORD64 hSymbols = (DWORD64)sym_provider->loadSymbols(this, the_process, debug_event.u.LoadDll.lpBaseOfDll, dll_image_name, NULL, &err);

				if (!processImage(the_process, debug_event.u.LoadDll.lpBaseOfDll, hSymbols, dll_image_name)) {
					spdlog::warn("Error loading image information for: {}", dll_image_name);
					error_exit = true;
					debug_driver->dbgr_client_debuggerError(this, -1, "Error loading image information for: " + dll_image_name);
					bContinueDebugging = false;
				}

			}
			break;

			case UNLOAD_DLL_DEBUG_EVENT:
			{
				if (!is_debugging_enabled())
					break;

				strEventMessage = "UNLOAD_DLL_DEBUG_EVENT";

				SharedModuleInfo *mi = nullptr;
				if (isLoadedGnuCOBOLImage(debug_event.u.UnloadDll.lpBaseOfDll, &mi)) {
					spdlog::trace("Gix-IDE debugger: unloading DLL: {}", mi->dll_path);
					unloadGnuCOBOLImage(mi);
				}

			}
				break;

			case OUTPUT_DEBUG_STRING_EVENT:
			{
				if (!is_debugging_enabled())
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

				spdlog::trace(msg);

				delete[]msg;
			}

			break;

			case EXCEPTION_DEBUG_EVENT:
			{
				if (!is_debugging_enabled())
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
							debug_driver->dbgr_client_debuggerReady(this, exepath);
							spdlog::trace("EXCEPTION_BREAKPOINT: Breakpoint 0 hit");
							break;
						}

						spdlog::trace("EXCEPTION_BREAKPOINT: Breakpoint hit");

						PVOID ex_addr = exception.ExceptionRecord.ExceptionAddress;

						bool is_cbl_entry_point = isCblEntryPoint(ex_addr, &current_cbl_module);

						getAndResolveUserBreakpoints();
						
						UserBreakpoint *bp = findBreakpointByAddress(ex_addr);
						if (!bp) {	// This will probably lead to a crash
							spdlog::error("EXCEPTION_BREAKPOINT: Breakpoint not found: {}", ex_addr);
							break;
						}

						spdlog::trace("EXCEPTION_BREAKPOINT: Breakpoint at address {} is at source line {} of {}", bp->address, bp->line, bp->source_file);

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
						spdlog::trace("EXCEPTION_BREAKPOINT: Hardware breakpoint uninstalled at {}", bp->address);

						last_bkp = bp;
						last_source_file = bp->source_file;
						last_source_line = bp->line;
					}
					break;

					case EXCEPTION_SINGLE_STEP:
					{
						if (!is_debugging_enabled())
							break;

						spdlog::trace("EXCEPTION_SINGLE_STEP: Breakpoint single-step");

						is_on_break = true;
#ifdef _WIN64
						DWORD64 dwWriteSize;
#else
						DWORD dwWriteSize;
#endif
						uint8_t brk_inst = 0xcc;
						if (last_bkp) {
							last_bkp->install();
							spdlog::trace("EXCEPTION_SINGLE_STEP: Reinstalled hardware breakpoint at {}", last_bkp->address);

							if (!last_bkp->automatic || single_step) {
								spdlog::trace("EXCEPTION_SINGLE_STEP: User/single step breakpoint: yes");
								if (this->source_lines_by_addr.find(last_bkp->address) != source_lines_by_addr.end()) {
									spdlog::trace("EXCEPTION_SINGLE_STEP: Successfully decoded source line info");
									SourceLineInfo *sli = source_lines_by_addr[last_bkp->address];
									spdlog::trace("Found breakpoint at {} ({}:{})", last_bkp->address, sli->source_file, sli->line);

 									// Check if breakpoint is on the first line of a generated preprocessor block
 									// If yes, pass the line corresponding to the start of the original block
 									int actual_line = 0;
									if (!is_first_line_of_preproc_block(current_cbl_module, sli->source_file, sli->line, &actual_line)) {
										debug_driver->dbgr_client_debuggerBreak(this, current_cbl_module->name, sli->source_file, sli->line);
										spdlog::trace("Debugger breaking at: %s:%d", sli->source_file, sli->line);
									}
									else {
										debug_driver->dbgr_client_debuggerBreak(this, current_cbl_module->name, sli->source_file, actual_line);
										spdlog::trace("Debugger breaking at: %s:%d", sli->source_file, actual_line);
									}
								}
							}
							else {
								spdlog::trace("EXCEPTION_SINGLE_STEP: User/single step breakpoint: no");
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

						//log(string_format("Exception code: %08x", exception.ExceptionRecord.ExceptionCode), LOG_ERR);

						if (exception.dwFirstChance == 1) {
							if (is_debugging_enabled()) {

								HANDLE h_exc_thread = OpenThread(THREAD_ALL_ACCESS, FALSE, debug_event.dwThreadId);

								std::string stack_dump = sym_provider->dumpStackFrame(this, the_process, h_exc_thread);
								if (stack_dump.empty())
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

								strEventMessage = string_format("First chance exception at %016p, exception-code: 0x%08x\nStack frame: %s\n================\n",
									exception.ExceptionRecord.ExceptionAddress,
									exception.ExceptionRecord.ExceptionCode,
									stack_dump.c_str());
							}
							else {
								strEventMessage = string_format("First chance exception at %016p, exception-code: 0x%08x\nStack frame: not available\n================\n",
									exception.ExceptionRecord.ExceptionAddress,
									exception.ExceptionRecord.ExceptionCode);
							}

							error_exit = true;
							debug_driver->dbgr_client_debuggerError(this, exception.ExceptionRecord.ExceptionCode, strEventMessage);
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
			spdlog::trace(strEventMessage);

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

	spdlog::trace("Canceling reader threads");
	
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

	if (is_debugging_enabled()) {
		sym_provider->deinit(this, the_process);
		delete sym_provider;
		SymCleanup(the_process);
	}

	//CloseHandle(the_process);
	//CloseHandle(the_thread);
	DebugActiveProcessStop(the_process_id);
	
	target_is_running = false;
	
	return 0;
}



void GixDebuggerWin::setupEnvironmentBlock()
{
	int sz = 0;
	int pos = 0;
	for (auto it = environment.begin(); it != environment.end(); ++it) {
		sz += (it->first.length() + it->second.length() + 2);	// 2 = "=", \0
	}

	sz += 1;	//block terminating zero

	envBlock = new char[sz];
	for (auto it = environment.begin(); it != environment.end(); ++it) {
#if _DEBUG
		char bfr[65535];
		sprintf(bfr, "Process environment - %s : %s\n", it->first.c_str(), it->second.c_str());
		OutputDebugStringA(bfr);
#endif
		memcpy(envBlock + pos, it->first.c_str(), it->first.size()); pos += it->first.size();
		*(envBlock + pos) = '='; pos++;

		memcpy(envBlock + pos, it->second.c_str(), it->second.size()); pos += it->second.length();
		*(envBlock + pos) = '\0'; pos++;
	}
	*(envBlock + pos) = '\0';
}

int GixDebuggerWin::stop()
{
	DWORD rc;
	bool user_initiated_stop = false;
	GetExitCodeProcess(the_process, &rc);
	if (rc == STILL_ACTIVE) {
		exit_code = 0xDEAD;
		BOOL b = TerminateProcess(the_process, exit_code);
		const DWORD result = WaitForSingleObject(the_process, 3000);
		user_initiated_stop = true;
	}

	// Here we can only log locally: the client might not exist anymore, so we have to prevent the debugger from deadlocking on a remote log call
	log_local(string_format("Process %d (%s) stopped with exit code 0x%x (user-initiated: %s)", the_process_id, image_name, exit_code, user_initiated_stop ? "yes" : "no"), spdlog::level::info);
	if (user_initiated_stop) {
		log_local("Session has been shut down by the client (" + std::to_string(exit_code) + ")", spdlog::level::info);
	}

	return exit_code;
}

bool GixDebuggerWin::step()
{
	//SetEvent(dbg_brk_handle);
	single_step = true;
	debug_driver->releaseWaitLock();
	return true;
}

bool GixDebuggerWin::continue_running()
{
	single_step = false;
	debug_driver->releaseWaitLock();
	return true;
}

void GixDebuggerWin::printLastError()
{
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

	spdlog::trace("Last error: {}", (char *)lpMsgBuf);

	LocalFree(lpDisplayBuf);
}

bool GixDebuggerWin::getVariables(const std::vector<std::string>& var_names, std::map<std::string, VariableDisplayData>& var_list)
{
	if (!is_on_break) {
		spdlog::warn("GixDebuggerWin is trying to resolve variable while not on break");
		return false;
	}

	if (!var_names.size()) {
		spdlog::trace("No variables to resolve");
		return true;
	}

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

	for (std::string var_name : var_names) {
		spdlog::trace("GixDebuggerWin is trying to resolve variable [{}]", var_name);
		// Resolve COBOL variable name to a local symbol in the current stack frame + offset
		if (map_contains(current_cbl_module->locals, var_name)) {

			VariableResolverData *vrd = current_cbl_module->locals[var_name];
			if (map_contains(current_cbl_module->locals, vrd->base_var_name)) {
				
				VariableResolverData *vrootvar = current_cbl_module->locals[vrd->base_var_name];
#ifdef _WIN64
				unsigned long long addr = (unsigned long long) sym_provider->resolveLocalVariableAddress(this, the_process, current_cbl_module, frame_ptr, vrootvar, vrd);
#else
				unsigned long addr = (unsigned long) sym_provider->resolveLocalVariableAddress(this, the_process, current_cbl_module, frame_ptr, vrootvar, vrd);
#endif
				VariableDisplayData vd = vrd->toDisplayData();
				uint8_t* raw_data = new uint8_t[vrd->storage_size];

				memset(raw_data, 0x00, vrd->storage_size);

				if (!ReadProcessMemory(the_process, (LPCVOID)addr, raw_data, vrd->storage_size, &dwReadBytes)) {
					this->printLastError();
					continue;
				}

				if (!this->buildVariableDisplayData(vd, raw_data)) {
					spdlog::warn("Cannot build displayable data for variable {}", var_name);
				}


				var_list[vd.var_name] = vd;
			}
			else {
				spdlog::warn("GixDebuggerWin cannot find variable [{}] in the locals list", var_name);
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

std::string GixDebuggerWin::getCurrentCobolModuleName()
{
	if (current_cbl_module)
		return current_cbl_module->name;

	return std::string();
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


std::string GetFileNameFromHandle(HANDLE hFile)
{
	BOOL bSuccess = FALSE;
	TCHAR pszFilename[MAX_PATH + 1];
	HANDLE hFileMap;

	std::string strFilename;

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
									std::string drive = std::string((const char *)szDrive);
									std::string fn = std::string((const char *)pszFilename);

									strFilename = drive + fn.substr(uNameLen);
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

	hPipe = (data->channel_type == PipeChannelType::Out) ? data->gd->hChildStd_OUT_Rd : data->gd->hChildStd_ERR_Rd;
	while (!data->gd->stop_reading_pipes) {
		bSuccess = ReadFile(hPipe, chBuf, BUFSIZE, &dwRead, NULL);
		if (!bSuccess || dwRead == 0) {
			break;
		}

		chBuf[dwRead] = 0;

		if (data->channel_type == PipeChannelType::Out)
			data->gd->debug_driver->dbgr_client_debuggerStdOutAvailable(data->gd, chBuf);
		else
			data->gd->debug_driver->dbgr_client_debuggerStdErrAvailable(data->gd, chBuf);
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

bool GixDebuggerWin::processImage(HANDLE hProc, HANDLE imageBase, DWORD64 hSym, std::string imageName)
{
	int err = 0;
	if (hSym) {
		spdlog::debug("Loaded symbols for {}", imageName);
		if (sym_provider->isGnuCOBOLModule(this, the_process, imageBase, NULL, &err)) {
			image_name = imageName;
			spdlog::debug("{} is a GnuCOBOL module", imageName);
			uint32_t base_of_code = extract_base_of_code(hProc, imageBase);
			SharedModuleInfo *smi = sym_provider->extractModuleDebugInfo(this, the_process, imageBase, (void *)hSym, imageName, (void *) base_of_code, &err);

			if (smi) {
				shared_modules.push_back(smi);
				for (auto it = smi->cbl_modules.begin(); it != smi->cbl_modules.end(); ++it) {
					spdlog::trace("Installing hardware breakpoint for module {}", it->second->name);
					it->second->entry_breakpoint->install();
					breakPointAdd(it->second->entry_breakpoint);
				}
				getAndResolveUserBreakpoints();
			}
			else
				return false;
		}
		else {
			if (err) {
				// Now what?
			}

			spdlog::debug("{} is NOT a GnuCOBOL module", imageName);
			if (starts_with(filename_get_name(to_lower(imageName)), "libcob.")) {
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

	spdlog::debug("Shared module {} is being unloaded", mi->dll_path);

	std::vector<UserBreakpoint *> to_be_removed;
	for (auto it = this->breakpoints.begin(); it != this->breakpoints.end(); ++it) {
		UserBreakpoint *bkp = *it;
		if (bkp->owner == mi) {
			//removeHardwareBreakpoint(it.value());
			to_be_removed.push_back(bkp);
		}
	}

	for (UserBreakpoint *bkp: to_be_removed) {
		breakPointRemove(bkp);
	}

	for (auto it_cmi = mi->cbl_modules.begin(); it_cmi != mi->cbl_modules.end(); ++it_cmi) {
		auto cmi = it_cmi->second;
		if (cmi && cmi->entry_breakpoint) {
			//removeHardwareBreakpoint(cmi->entry_breakpoint);
		}
	}

	bool b = sym_provider->unloadSymbols(this, the_process, mi->dll_base, mi->dll_path, NULL, 0);
	vector_remove(shared_modules, mi);
	
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
		spdlog::trace("Breakpoint at {} for {}@{} is already installed, skipping", this->address, this->line, this->source_file.c_str());
		return true;
	}

	if (!this->address)
		return false;

	GixDebuggerWin *gdwin = (GixDebuggerWin *) this->owner->owner;

	spdlog::trace("Installing hardware breakpoint at {} for {}@{}", this->address, this->line, this->source_file.c_str());

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

	spdlog::trace("Successfully installed hardware breakpoint at {} for {}@{}", this->address, this->line, this->source_file.c_str());

	return true;
}

bool WinUserBreakpoint::uninstall()
{
	uint8_t cInstruction;
	SIZE_T dwWrittenBytes;

	if (!isInstalled()) {
		spdlog::trace("Breakpoint at {} for {}@{} is not installed, skipping", this->address, this->line, this->source_file.c_str());
		return true;
	}

	if (!this->address)
		return false;

	spdlog::trace("Uninstalling hardware breakpoint at {} for {}@{}", this->address, this->line, this->source_file.c_str());

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

	spdlog::trace("Successfully uninstalled hardware breakpoint at {} for {}@{}", this->address, this->line, this->source_file.c_str());
	
	return true;
}

#endif
