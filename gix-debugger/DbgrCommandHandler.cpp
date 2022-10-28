#include "DbgrCommandHandler.h"
#include "DebuggerHostInputMessage.h"
#include "DebuggerHostOutputMessage.h"
#include "debugger-msg-defs.h"

#include "spdlog/spdlog.h"

extern std::thread debug_session_thread;

void DebugSessionThreadStub(std::shared_ptr<NetworkManager> nm, GixDebugger* gd);

bool DbgrCommandHandler::handleCommand(std::shared_ptr<NetworkManager> nm, const DebuggerHostInputMessage& m, bool *keep_running)
{		
	spdlog::trace("DbgrCommandHandler is processing: {}", m.serialize());

	if (m.cmd == DBGR_MSG_IN_CMD_HELLO) {
		if (nm->getMode() != NetworkManagerMode::Passive) {
			// Hope somebody is listening
			return nm->emit_response_ko("Invalid command, mode must be passive");
		}
		
		auto url = m.payload1.toString();
		if (!nm->setRemoteUrl(url)) {
			// we cannot return a response, obviously
			spdlog::error("Cannot parse URL from client: {}", url);
			return false;
		}
		spdlog::debug("remote URL set: {}", url);
		spdlog::debug("opening remote socket");

		if (!nm->setup_remote_connection()) {
			// we cannot return a response, obviously
			spdlog::error("Cannot setup remote connection to {}", url);
			return false;
		}

		spdlog::debug("remote socket open");

		// we have a connection to the remote party (client), we can finally write something
		return nm->emit_response_ok("Hello " + url + ", nice meeting you");
	}

	if (m.cmd == DBGR_MSG_IN_CMD_START_SESSION) {

		if (debug_driver) {
			spdlog::debug("Session already started");
			return nm->emit_response_ko("Session already started");
		}

		GixDebuggerSessionConfig cfg = m.payload1.toGixDebuggerSessionConfig();
		debug_driver = new DebuggerHostDriver();

		if (!debug_driver->getDebuggerInstance()) {
			spdlog::error("gix-debugger: cannot instantiate debugger engine");
			return false;
		}
		
		debug_driver->setNetworkManager(nm);
		debug_driver->getDebuggerInstance()->setConfiguration(cfg);
		debug_driver->getDebuggerInstance()->setProperty("symformat", "pdb");	// TODO: use parameter

		// The debug session is blocking, so we need to start it on a separate thread
		debug_session_thread = std::thread(DebugSessionThreadStub, nm, debug_driver->getDebuggerInstance());
		debug_session_thread.detach();

		return nm->emit_response_ok("Trying to start  debug session");
	}

	if (m.cmd == DBGR_MSG_IN_CMD_STOP_SESSION) {
		if (!debug_driver->getDebuggerInstance()) {
			return nm->emit_response_ko("Session not started");
		}

		int exit_code = debug_driver->getDebuggerInstance()->stop();
		DebuggerHostOutputMessage m_ack_stop(DBGR_MSG_IN_CMD_STOP_SESSION "_reply", DBGR_STATUS_OK, "Session stopped (" + std::to_string(exit_code) + ")");
		m_ack_stop.payload1 = exit_code;
		return nm->reply(m_ack_stop);
	}

	if (m.cmd == DBGR_MSG_IN_CMD_QUIT) {
		*keep_running = false;
		return nm->emit_response_ok("Debugger host is quitting");
	}

	if (m.cmd == DBGR_MSG_IN_CMD_STEP) {
		if (!debug_driver->getDebuggerInstance()) {
			return nm->emit_response_ko("Session not started");
		}

		debug_driver->getDebuggerInstance()->step();
		return nm->emit_response_ok("Single-stepping");
	}

	if (m.cmd == DBGR_MSG_IN_CMD_CONTINUE) {
		if (!debug_driver->getDebuggerInstance()) {
			return nm->emit_response_ko("Session not started");
		}

		debug_driver->getDebuggerInstance()->continue_running();
		return nm->emit_response_ok("Continuing");
	}

	if (m.cmd == DBGR_MSG_IN_CMD_GET_VARS) {
		if (!debug_driver->getDebuggerInstance()) {
			return nm->emit_response_ko("Session not started");
		}

		std::map<std::string, VariableDisplayData> var_data;
		std::vector<std::string> var_names = m.payload1.toStringVector();

		spdlog::trace("get_vars - requesting variables: {}", fmt::join(var_names, ", "));

		bool b = debug_driver->getDebuggerInstance()->getVariables(var_names, var_data);
		if (!b) {
			return nm->emit_response_ko("Cannot retrieve variable data");
		}

		spdlog::trace("get_vars - debugger process returned: {}");
		DebuggerHostOutputMessage m_vars(DBGR_MSG_IN_CMD_GET_VARS "_reply", DBGR_STATUS_OK, "Variable data follows");
		m_vars.payload1 = var_data;
		spdlog::trace("get_vars - debugger process returned: {}", m_vars.serialize());
		return nm->reply(m_vars);
	}

	return false;
}

void DebugSessionThreadStub(std::shared_ptr<NetworkManager> nm, GixDebugger* gd)
{
	int rc = gd->start();
}
