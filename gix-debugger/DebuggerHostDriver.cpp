#include "DebuggerHostDriver.h"
#include "GixDebugger.h"
#include "DebuggerHostInputMessage.h"
#include "DebuggerHostOutputMessage.h"
#include "debugger-msg-defs.h"
#include "NetworkManager.h"

#include <spdlog/spdlog.h>

DebuggerHostDriver::DebuggerHostDriver()
{
    debugger_instance = GixDebugger::get();
	debugger_instance->setDriver(this);
}

DebuggerHostDriver::~DebuggerHostDriver()
{
    if (debugger_instance)
        delete debugger_instance;
}

void DebuggerHostDriver::setNetworkManager(std::shared_ptr<NetworkManager> nm)
{
    network_manager = nm;
}

void DebuggerHostDriver::setSessionConfiguration(GixDebuggerSessionConfig* cfg)
{
    debug_session_config = cfg;
}

GixDebugger* DebuggerHostDriver::getDebuggerInstance()
{
    return debugger_instance;
}

bool DebuggerHostDriver::dbgr_client_getBreakpoints(GixDebugger *gd, std::vector<std::pair<std::string, int>>& user_breakpoints)
{
	spdlog::trace("Trying to get breakpoints from debugger client");

	DebuggerHostOutputMessage req;
	req.type = DBGR_MSG_OUT_TYPE_GET_BRKPS;
	DebuggerHostInputMessage client_resp;
	std::string resp_string;
	network_manager->send_rcv(req.serialize(), resp_string);
	if (client_resp.deserialize(resp_string) && client_resp.status == DBGR_STATUS_OK) {
		auto m = client_resp.payload1.toStringVector();
		for (auto it = m.begin(); it != m.end(); ++it) {
			std::string brkp = *it;
			int sep_pos = brkp.find("@");
			if (sep_pos == std::string::npos)
				continue;

			std::string src_file = brkp.substr(sep_pos + 1);
			int line = atoi(brkp.substr(0, sep_pos).c_str());
			user_breakpoints.push_back(std::pair(src_file, line));
		}
		spdlog::trace("Received {} breakpoints from debugger client", user_breakpoints.size());
		return true;
	}
	else {
		spdlog::trace("No breakpoints received (error)");
	}

	return false;
}

bool DebuggerHostDriver::dbgr_client_debuggerBreak(GixDebugger* gd, std::string module_name, std::string source_file, int line)
{
	DebuggerHostOutputMessage req;
	req.type = DBGR_MSG_OUT_TYPE_BREAK;
	req.payload1 = module_name;
	req.payload2 = source_file;
	req.payload3 = line;
	DebuggerHostInputMessage client_resp;
	std::string resp_string;
	network_manager->send_rcv(req.serialize(), resp_string);
	client_resp.deserialize(resp_string);
	spdlog::trace("req/resp : {}/{}", req.serialize(), resp_string);
	if (client_resp.status != DBGR_STATUS_OK)
		return false;

#ifdef _WIN32
	spdlog::trace("Waiting on break (thread id: {})", GetCurrentThreadId());
#else
	spdlog::trace("Waiting on break (thread id: {})", gettid());
#endif

    spdlog::trace("Acquiring break wait lock");
	this->acquireWaitLock();

	spdlog::trace("Resuming from break");

	return true;
}

bool DebuggerHostDriver::dbgr_client_debuggerError(GixDebugger *gd, int err_code, std::string msg)
{
	DebuggerHostOutputMessage req;
	req.type = DBGR_MSG_OUT_TYPE_ERROR;
	req.payload1 = msg;
	req.payload2 = 0;
	DebuggerHostInputMessage client_resp;
	std::string resp_string;
	network_manager->send_rcv(req.serialize(), resp_string);
	client_resp.deserialize(resp_string);
	return client_resp.status == DBGR_STATUS_OK;
}

bool DebuggerHostDriver::dbgr_client_debuggerProcessExit(GixDebugger *gd, int exit_code, std::string exe_path)
{
	DebuggerHostOutputMessage req;
	req.type = DBGR_MSG_OUT_TYPE_PROC_EXIT;
	req.payload1 = exit_code;
	req.payload2 = exe_path;
	DebuggerHostInputMessage client_resp;
	std::string resp_string;
	network_manager->send_rcv(req.serialize(), resp_string);
	client_resp.deserialize(resp_string);
	return client_resp.status == DBGR_STATUS_OK;
}

bool DebuggerHostDriver::dbgr_client_debuggerProcessStarted(GixDebugger *gd, std::string exe_path)
{
	spdlog::trace("network_manager");
	DebuggerHostOutputMessage req;
	req.type = DBGR_MSG_OUT_TYPE_PROC_START;
	req.payload1 = exe_path;
	DebuggerHostInputMessage client_resp;
	std::string resp_string;
	network_manager->send_rcv(req.serialize(), resp_string);
	client_resp.deserialize(resp_string);
	return client_resp.status == DBGR_STATUS_OK;
}

bool DebuggerHostDriver::dbgr_client_debuggerReady(GixDebugger *gd, std::string exe_path)
{
	DebuggerHostOutputMessage req;
	req.type = DBGR_MSG_OUT_TYPE_READY;
	req.payload1 = exe_path;
	DebuggerHostInputMessage client_resp;
	std::string resp_string;
	network_manager->send_rcv(req.serialize(), resp_string);
	client_resp.deserialize(resp_string);
	return client_resp.status == DBGR_STATUS_OK;
}

bool DebuggerHostDriver::dbgr_client_debuggerStdOutAvailable(GixDebugger *gd, std::string data)
{
	DebuggerHostOutputMessage req;
	req.type = DBGR_MSG_OUT_TYPE_STDOUT_AVAILABLE;
	req.payload1 = data;
	DebuggerHostInputMessage client_resp;
	std::string resp_string;
	network_manager->send_rcv(req.serialize(), resp_string);
	client_resp.deserialize(resp_string);
	return client_resp.status == DBGR_STATUS_OK;
}

bool DebuggerHostDriver::dbgr_client_debuggerStdErrAvailable(GixDebugger *gd, std::string data)
{
	DebuggerHostOutputMessage req;
	req.type = DBGR_MSG_OUT_TYPE_STDERR_AVAILABLE;
	req.payload1 = data;
	DebuggerHostInputMessage client_resp;
	std::string resp_string;
	network_manager->send_rcv(req.serialize(), resp_string);
	client_resp.deserialize(resp_string);
	return client_resp.status == DBGR_STATUS_OK;
}

void DebuggerHostDriver::acquireWaitLock()
{
    std::unique_lock<std::mutex> lk(cv_m);
    cv_on_break.wait(lk);
}

void DebuggerHostDriver::releaseWaitLock()
{
    cv_on_break.notify_one();
}
