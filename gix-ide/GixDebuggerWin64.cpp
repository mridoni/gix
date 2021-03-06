#include "GixDebuggerWin64.h"
#include "CodeviewSymbolProvider.h"
#include "DwarfSymbolProvider.h"
//#include "InjectThread.h"
//#include "libcob.h"
#include "utils.h"

#include <QDir>

#define BUFSIZE 65536

QString GetFileNameFromHandle(HANDLE hFile);

bool single_step = false;
QString last_source_file;
int last_source_line = 0;


struct stPipeThreadData
{
	GixDebuggerWin64 *gd;
	int channel;
};

static struct stPipeThreadData threadDataOut;
static struct stPipeThreadData threadDataErr;


GixDebuggerWin64::GixDebuggerWin64()
{
}


GixDebuggerWin64::~GixDebuggerWin64()
{
	if (envBlock)
		delete envBlock;
}

int GixDebuggerWin64::start()
{
	STARTUPINFO si;
	int err = 0;
	UserBreakpoint *last_bkp = nullptr;
	wchar_t dbg_help_path[1024] = { 0, 0, 0, 0 };

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

		HMODULE hDbgHelp = GetModuleHandle(L"dbghelp.dll");
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

	if (!use_external_console) {
		SECURITY_ATTRIBUTES saAttr;
		saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
		saAttr.bInheritHandle = TRUE;
		saAttr.lpSecurityDescriptor = NULL;

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
			&GixDebuggerWin64::PipeReaderThread,	// thread function name
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
			&GixDebuggerWin64::PipeReaderThread,	// thread function name
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
	
	//if (is_debugging_enabled)
	//	createFlags |= DEBUG_ONLY_THIS_PROCESS;

	if (use_external_console)
		createFlags |= CREATE_NEW_CONSOLE;

	if (!CreateProcess(NULL, (LPWSTR) fcl.toStdWString().c_str(), NULL, NULL, !use_external_console, createFlags, envBlock, working_dir.toStdWString().c_str(), &si, &process_info)) { // DEBUG_ONLY_THIS_PROCESS
		TerminateThread(hThreadReadStdOut, 0);
		TerminateThread(hThreadReadStdErr, 0);
		
		if_blk->debuggerError(this, 1, "Cannot start process");
		printLastError();
		return 1;
	}

	//if (!is_debugging_enabled)
	//	return 0;

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
						if_blk->debuggerMessage(this, "Error loading image information for: " + sEXEName, 0);
					}
				}

			}
			break;

			case CREATE_THREAD_DEBUG_EVENT:
				if (!is_debugging_enabled)
					break;

				if_blk->debuggerMessage(this, QString::fromStdString(std_string_format("Thread 0x%x (Id: %d) created at: 0x%x",
					debug_event.u.CreateThread.hThread,
					debug_event.dwThreadId,
					debug_event.u.CreateThread.lpStartAddress)), 0); // Thread 0xc (Id: 7920) created at: 0x77b15e58
				//strEventMessage = "CREATE_THREAD_DEBUG_EVENT";			
				break;

			case EXIT_THREAD_DEBUG_EVENT:
				if (!is_debugging_enabled)
					break;

				if_blk->debuggerMessage(this, QString::fromStdString(std_string_format("The thread %d exited with code: %d",
					debug_event.dwThreadId,
					debug_event.u.ExitThread.dwExitCode)), 0);	// The thread 2760 exited with code: 0
				strEventMessage = "EXIT_THREAD_DEBUG_EVENT";
				break;

			case EXIT_PROCESS_DEBUG_EVENT:
				exit_code = (int)debug_event.u.ExitProcess.dwExitCode;
				if_blk->debuggerProcessExit(this, exit_code, exepath);		
#if _DEBUG
				if_blk->debuggerMessage(this, ":: EXIT CODE: " + QString::number(exit_code), 0);
				OutputDebugStringA(std_string_format("*** %s says goodbye (%d)\n", exepath.toLocal8Bit().constData(), exit_code).c_str());
#endif
				bContinueDebugging = false;
				break;

			case LOAD_DLL_DEBUG_EVENT:
			{
				if (!is_debugging_enabled)
					break;

				QString sDLLName = GetFileNameFromHandle(debug_event.u.LoadDll.hFile);
				if (sDLLName.toUpper().endsWith("WEBTEST001.DLL")) {
					int nnn = 01;
				}
				QString cp = QDir::cleanPath(sDLLName);
				if (properties["symformat"] == "dwarf" && !cp.startsWith(this->module_dir))
					break;

				strEventMessage = QString("Loaded DLL: ") + sDLLName;
				DWORD64 hSymbols = (DWORD64)sym_provider->loadSymbols(this, the_process, debug_event.u.LoadDll.lpBaseOfDll, sDLLName, NULL, &err);
				if (!processImage(the_process, debug_event.u.LoadDll.lpBaseOfDll, hSymbols, sDLLName)) {
					if_blk->debuggerMessage(this, "Error loading image information for: " + sDLLName, 0);
				}

			}
			break;

			case UNLOAD_DLL_DEBUG_EVENT:
				if (!is_debugging_enabled)
					break;

				strEventMessage = "UNLOAD_DLL_DEBUG_EVENT";
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
					case EXCEPTION_BREAKPOINT:\
					{
						if (!__breakpoint_0_hit) {
							strEventMessage = "Break point 0";
							__breakpoint_0_hit = true;
							//inject_helper(the_process, the_thread);
							break;
						}

						PVOID ex_addr = exception.ExceptionRecord.ExceptionAddress;

						bool is_cbl_entry_point = isCblEntryPoint(ex_addr, &current_cbl_module);

						getAndResolveUserBreakpoints();
						
						UserBreakpoint *bp = findBreakpointByAddress(ex_addr);
						if (!bp)	// This will probably lead to a crash
							break;

						CONTEXT lcContext;
						lcContext.ContextFlags = CONTEXT_ALL;
						GetThreadContext(process_info.hThread, &lcContext);
						lcContext.Rip--; // Move back one byte, lcContext.Eip-- for 32bit
						lcContext.EFlags |= 0x100;
						SetThreadContext(process_info.hThread, &lcContext);
						DWORD64 dwWriteSize;
						WriteProcessMemory(process_info.hProcess, exception.ExceptionRecord.ExceptionAddress, &bp->orig_instr, 1, &dwWriteSize);
						FlushInstructionCache(the_process, ex_addr, 1);

						last_bkp = bp;
						last_source_file = bp->source_file;
						last_source_line = bp->line;

						//if (is_cbl_entry_point && current_cbl_module) {
						//	if_blk->debuggerMessage(this, "At entry point for module " + current_cbl_module->name, 0);
						//	if (!current_cbl_module->initialized) {
						//		bool b = initCobolModuleLocalInfo(this, the_process, current_cbl_module);
						//	}
						//}

					}
					break;

					case EXCEPTION_SINGLE_STEP:
					{
						if (!is_debugging_enabled)
							break;

						is_on_break = true;
						DWORD64 dwWriteSize;
						uint8_t brk_inst = 0xcc;
						if (last_bkp) {
							WriteProcessMemory(process_info.hProcess, last_bkp->address, &brk_inst, 1, &dwWriteSize);
							FlushInstructionCache(the_process, last_bkp->address, 1);

							if (!last_bkp->automatic || single_step) {
								if (this->source_lines_by_addr.find(last_bkp->address) != source_lines_by_addr.end()) {
									SourceLineInfo *sli = source_lines_by_addr[last_bkp->address];
									if_blk->debuggerMessage(this, QString::fromStdString(std_string_format("Found breakpoint at 0x%08p (%s:%d)\n", last_bkp->address, sli->source_file, sli->line)), 0);
									if_blk->debuggerBreak(this, current_cbl_module->name, sli->source_file, sli->line);
								}
							}
						}

						is_on_break = false;
						last_bkp = NULL;
					}
					break;

					default:
						if (exception.ExceptionRecord.ExceptionCode == 0x406D1388)	// Handle SetThreadName-generated exceptions
							break;

						if (exception.dwFirstChance == 1) {
							if (is_debugging_enabled) {
								QString stack_dump = sym_provider->dumpStackFrame(this, the_process, the_thread);
								if (stack_dump.isEmpty())
									stack_dump = "not available";
								else
									stack_dump = "\n" + stack_dump;

								strEventMessage = QString::fromStdString(std_string_format("First chance exception at %x, exception-code: 0x%08x\nStack frame: %s\n",
									exception.ExceptionRecord.ExceptionAddress,
									exception.ExceptionRecord.ExceptionCode,
									stack_dump.toLocal8Bit().constData()));
							}
							else {
								strEventMessage = QString::fromStdString(std_string_format("First chance exception at %x, exception-code: 0x%08x\nStack frame: not available\n",
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

void GixDebuggerWin64::setupEnvironmentBlock()
{
	int sz = 0;
	int pos = 0;
	for (auto it = environment.begin(); it != environment.end(); ++it) {
		sz += (it.key().length() + it.value().length() + 2);	// 2 = "=", \0
	}

	sz += 1;	//block terminating zero

	envBlock = new char[sz];
	for (auto it = environment.begin(); it != environment.end(); ++it) {
		memcpy(envBlock + pos, it.key().toLocal8Bit().constData(), it.key().length()); pos += it.key().length();
		*(envBlock + pos) = '='; pos++;

		memcpy(envBlock + pos, it.value().toLocal8Bit().constData(), it.value().length()); pos += it.value().length();
		*(envBlock + pos) = '\0'; pos++;
	}
	*(envBlock + pos) = '\0';
}

int GixDebuggerWin64::stop()
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

bool GixDebuggerWin64::step()
{
	//SetEvent(dbg_brk_handle);
	single_step = true;
	return true;
}

bool GixDebuggerWin64::continue_running()
{
	single_step = false;
	return true;
}

void GixDebuggerWin64::printLastError()
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

	OutputDebugStringW((wchar_t *)lpMsgBuf);
	LocalFree(lpDisplayBuf);
}

void GixDebuggerWin64::removeHardwareBreakpoint(UserBreakpoint *bkp)
{
	uint8_t cInstruction;
	SIZE_T dwReadBytes;

	if (!bkp->address || !bkp->orig_instr)
		return;

	cInstruction = bkp->orig_instr;
	WriteProcessMemory(the_process, (void *)bkp->address, &cInstruction, 1, &dwReadBytes);
	FlushInstructionCache(the_process, (void *)bkp->address, 1);
}

bool GixDebuggerWin64::installHardwareBreakpoint(UserBreakpoint *bkp)
{
	uint8_t cInstruction = 0x00;
	SIZE_T dwReadBytes;

	if (!bkp->address)
		return false;

	char bfr[256];
	sprintf(bfr, "Installing breakpoint at 0x%p for %d@%s\n", bkp->address, bkp->line, bkp->source_file.toLocal8Bit().constData());
	OutputDebugStringA(bfr);

	// Read the instruction    
	if (!ReadProcessMemory(the_process, (void *)bkp->address, &cInstruction, 1, &dwReadBytes)) {
		this->printLastError();
		return false;
	}

	// Save it!
	bkp->orig_instr = cInstruction;

	// Replace it with Breakpoint
	cInstruction = 0xCC;
	WriteProcessMemory(the_process, (void *)bkp->address, &cInstruction, 1, &dwReadBytes);
	FlushInstructionCache(the_process, (void *)bkp->address, 1);

	return true;
}

bool GixDebuggerWin64::getVariables(QList<VariableData *> var_list)
{
	if (!is_on_break)
		return false;

	if (!var_list.size())
		return true;


	CONTEXT lcContext;
	lcContext.ContextFlags = CONTEXT_ALL;
	GetThreadContext(process_info.hThread, &lcContext);
	unsigned long long frame_ptr = lcContext.Rdi;

	for (VariableData *vd : var_list) {

		// Resolve COBOL variable name to a local symbol in the current stack frame + offset
		if (current_cbl_module->locals.contains(vd->var_name)) {

			VariableResolverData *vvar = current_cbl_module->locals[vd->var_name];
			if (current_cbl_module->locals.contains(vvar->base_var_name)) {
				
				VariableResolverData *vrootvar = current_cbl_module->locals[vvar->base_var_name];

				unsigned long long addr = (unsigned long long) sym_provider->resolveLocalVariableAddress(this, the_process, current_cbl_module, frame_ptr, vrootvar, vvar);

				vd->data = new uint8_t[vvar->storage_len];
				vd->storage_length = vvar->storage_len;
				DWORD64 dwReadBytes;
				if (!ReadProcessMemory(the_process, (LPCVOID)addr, vd->data, vvar->storage_len, &dwReadBytes)) {
					this->printLastError();
					continue;
				}
#if _DEBUG
				vd->data[vvar->storage_len] = 0;
				char bfr[256];
				sprintf(bfr, "%s : [%s]\n", vd->var_name.toLocal8Bit().constData(), vd->data);
				OutputDebugStringA(bfr);
#endif
			}
		}
	}

	return true;
}

bool GixDebuggerWin64::readProcessMemory(void *addr, void *bfr, int size)
{
	SIZE_T dwReadBytes;

	if (!addr || !bfr ||!size)
		return false;

	return ReadProcessMemory(the_process, (void *)addr, bfr, size, &dwReadBytes) && (dwReadBytes == size);
}

QString GixDebuggerWin64::getCurrentCobolModuleName()
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


ISymbolProvider *GixDebuggerWin64::get_symbol_provider()
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
									QString drive = QString::fromWCharArray((const wchar_t *)szDrive);
									QString fn = QString::fromWCharArray((const wchar_t *)pszFilename);

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

DWORD WINAPI GixDebuggerWin64::PipeReaderThread(LPVOID lpParam)
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

void *GixDebuggerWin64::getSymbolAddress(const char *sym_name)
{
	//return (void *)sym_provider->getSymbolAddress(this, the_process, libcob_base, sym_name, NULL, NULL);
	return nullptr;
}

bool GixDebuggerWin64::isCblEntryPoint(void *addr, CobolModuleInfo **cmi)
{
	for (auto mi : shared_modules) {
		for (auto it = mi->cbl_modules.begin(); it != mi->cbl_modules.end(); ++it) {
			if (it.value()->entry_point == addr) {
				*cmi = it.value();
				return true;
			}
		}
	}
	return false;
}

uint64_t extract_base_of_code(HANDLE hProc, void *dll_base)
{
	uint8_t mz_header[0x3f];
	uint8_t pe_header[0x51];

	ZeroMemory(mz_header, sizeof(mz_header));

	if (!ReadProcessMemory(hProc, dll_base, mz_header, sizeof(mz_header), NULL))
		return 0;

	uint32_t pe_hdr_offset = (uint32_t) *((uint32_t *)&mz_header[0x3c]);

	uint64_t pe_header_addr = (uint64_t)dll_base + pe_hdr_offset;

	if (!ReadProcessMemory(hProc, (void *)pe_header_addr, pe_header, sizeof(pe_header), NULL))
		return 0;

	uint32_t base_of_code = (uint32_t) *((uint32_t *)&pe_header[0x2c]);

	return base_of_code;
}

bool GixDebuggerWin64::processImage(HANDLE hProc, HANDLE imageBase, DWORD64 hSym, QString imageName)
{
	int err = 0;
	if (hSym) {
		if_blk->debuggerMessage(this, ":: Loaded symbols for " + imageName, 0);
		if (sym_provider->isGnuCOBOLModule(this, the_process, imageBase, NULL, &err)) {
			uint64_t base_of_code = extract_base_of_code(hProc, imageBase);
			SharedModuleInfo *mi = sym_provider->extractModuleDebugInfo(this, the_process, imageBase, (void *)hSym, imageName, (void *) base_of_code, &err);
			if (mi) {
				
				shared_modules.push_back(mi);
				for (auto it = mi->cbl_modules.begin(); it != mi->cbl_modules.end(); ++it) {
					if_blk->debuggerMessage(this, QString("Installing hardware breakpoint for module " + it.value()->name), 0);
					installHardwareBreakpoint(it.value()->entry_breakpoint);
					breakpoints[it.value()->entry_breakpoint->key] = it.value()->entry_breakpoint;
				}
			}
			else
				return false;
		}
	}

	return true;
}

bool GixDebuggerWin64::initCobolModuleLocalInfo(GixDebugger *gd, HANDLE hProc, CobolModuleInfo *cmi)
{
	return sym_provider->initCobolModuleLocalInfo(gd, hProc, cmi);
}