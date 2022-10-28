#pragma once

#if defined(__MINGW32__)
typedef unsigned char byte;
#endif

#include <string>
#include <map>
#include <functional>
#include <mutex>
#include <memory>
#include <condition_variable>

class GixDebugger;
class GixDebuggerSessionConfig;
class NetworkManager;

class DebuggerHostDriver {

public:

	DebuggerHostDriver();
	~DebuggerHostDriver();

	void setNetworkManager(std::shared_ptr<NetworkManager> nm);

	// Messages/Requests from debugged program to client debugger (VS, etc.)
	const static std::string CMD_DEBUGGER_STARTING;
	const static std::string CMD_GET_BREAKPOINTS;
	const static std::string CMD_DBGRBREAK;
	const static std::string CMD_DBGR_MOD_CHANGED;
	const static std::string CMD_DBGR_MOD_EXIT;
	const static std::string CMD_DBGR_PRG_EXIT;

	// Messages/Requests from client debugger (VS, etc.)  to debugged program
	const static std::string CMD_GET_CURPOS;
	const static std::string CMD_GET_VAR;
	const static std::string CMD_GET_VARS;
	const static std::string CMD_STEP;
	const static std::string CMD_STEP_INTO;
	const static std::string CMD_CONTINUE;
	const static std::string CMD_ABORT;

	// Standard responses
	const static std::string RESP_OK;
	const static std::string RESP_OK_WITH_RESULT;

	const static std::string RESP_KO;
	const static std::string RESP_KO_WITH_RESULT;

public:

	void setSessionConfiguration(GixDebuggerSessionConfig* cfg);
	GixDebugger* getDebuggerInstance();

	//virtual bool stop();
	//virtual std::string getLastResponse();
	//virtual void write(std::string);
	//virtual void writeToProcess(std::string);
	//virtual bool isStarted();
	//virtual bool getVariables(const std::vector<std::string>& var_names, std::map<std::string, struct VariableDisplayData>& var_data_req);

	// Debugger client interface (debugger messages are handled by a specialized spdlog sink)
	bool dbgr_client_getBreakpoints(GixDebugger*, std::vector<std::pair<std::string, int>>&);
	bool dbgr_client_debuggerBreak(GixDebugger* gd, std::string module_name, std::string source_file, int line);
	bool dbgr_client_debuggerError(GixDebugger*, int, std::string);
	bool dbgr_client_debuggerProcessExit(GixDebugger*, int, std::string);
	bool dbgr_client_debuggerProcessStarted(GixDebugger*, std::string);
	bool dbgr_client_debuggerReady(GixDebugger*, std::string);
	bool dbgr_client_debuggerStdOutAvailable(GixDebugger*, std::string);
	bool dbgr_client_debuggerStdErrAvailable(GixDebugger*, std::string);

	void acquireWaitLock();
	void releaseWaitLock();

protected:

	GixDebuggerSessionConfig* debug_session_config = nullptr;
	std::condition_variable cv_on_break;
	std::mutex cv_m;

private:

	GixDebugger* debugger_instance = nullptr;

	std::string current_module;

	bool keep_waiting_host_cmds = false;
	bool is_started = false;

	std::string last_response;
	std::string cmd_line;

	std::shared_ptr<NetworkManager> network_manager;

};
