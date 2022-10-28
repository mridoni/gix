#pragma once

#include <string>
#include <map>
#include <vector>
#include <functional>

#include <stdint.h>

#include "any_type.h"
#include "GixDebuggerSessionConfig.h"
#include "DebuggerHostInputMessage.h"
#include "DebuggerHostOutputMessage.h"

class DebuggerHostResponse;

class NetworkManager;

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include "Windows.h"
#define PID_T	DWORD
#else
#define PID_T	pid_t
#endif

class GixDebuggerClient
{
public:
	GixDebuggerClient();
	~GixDebuggerClient();

	void setClientConfig(GixDebuggerSessionConfig* cfg);
	GixDebuggerSessionConfig* getClientConfig();

	bool init();
	bool cleanup();

	bool startSession();
	bool stopSession();
	bool step();
	bool continue_running();
	bool getVariables(const std::vector<std::string>& var_names, std::map<std::string, struct VariableDisplayData>& var_data_req);

	bool enumPrograms(std::map<PID_T, std::string> programs);

	DebuggerHostOutputMessage getStatus();

	std::string get_last_error();
	std::string get_last_output();

	int get_exit_code();
	std::string get_exit_message();

	std::function<bool(GixDebuggerClient*, std::vector<std::pair<std::string, int>>&)> getBreakpoints;
	std::function<bool(GixDebuggerClient*, std::string, std::string, int)> debuggerBreak;
	std::function<bool(GixDebuggerClient*, std::string, int)> debuggerMessage;
	std::function<bool(GixDebuggerClient*, int, std::string)> debuggerError;
	std::function<bool(GixDebuggerClient*, int, std::string)> debuggerProcessExit;
	std::function<bool(GixDebuggerClient*, std::string)> debuggerProcessStarted;
	std::function<bool(GixDebuggerClient*, std::string)> debuggerReady;
	std::function<bool(GixDebuggerClient*, std::string)> debuggerStdOutAvailable;
	std::function<bool(GixDebuggerClient*, std::string)> debuggerStdErrAvailable;

	std::function<bool(GixDebuggerClient*, std::string, int level)> debuggerClientLog;
	std::function<bool(GixDebuggerClient*, std::string, int level)> debuggerHostLog;


private:

	GixDebuggerSessionConfig* client_config = nullptr;

	std::string last_error;
	std::string last_output;

	bool program_is_running = false;

	bool is_started = false;
	bool is_cleaned_up = false;

	int exit_code = 1;
	std::string exit_message;

	NetworkManager* network_manager = nullptr;
	
	std::string get_arch(std::string);
	bool handle_incoming_message(const DebuggerHostOutputMessage& msg, bool* response_sent);

	// Auto launch debugger host
	bool launched_dbgr_host_success = false;
	bool launch_local_debugger_host();
#ifdef _WIN32
	PROCESS_INFORMATION launched_dbgr_host_process_info = {};
#endif
};

