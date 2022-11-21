#include "GixDebuggerClient.h"
#include "libcpputils.h"
#include "NetworkManager.h"

#include "debugger-msg-defs.h"


#ifdef _WIN32
#include "Windows.h"
#include <Shlwapi.h>

#pragma comment(lib, "shlwapi.lib")

#endif

#ifdef __linux__
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#endif

#define CHECK_CLIENT_STARTED 	if (!is_started) \
										{ last_error = "Debugger client not started"; return false; }

#define __LOG(m,l) if (debuggerClientLog != nullptr) debuggerClientLog(this,m,l)
#define __HOST_LOG(m,l) if (debuggerHostLog != nullptr) debuggerHostLog(this,m,l)

GixDebuggerClient::GixDebuggerClient()
{
	network_manager = new NetworkManager(NetworkManagerMode::Active);
}

void GixDebuggerClient::setClientConfig(GixDebuggerSessionConfig* cfg)
{
	client_config = cfg;
}

GixDebuggerSessionConfig* GixDebuggerClient::getClientConfig()
{
	return client_config;
}

GixDebuggerClient::~GixDebuggerClient()
{
	if (!is_cleaned_up)
		cleanup();
}

bool GixDebuggerClient::init()
{

	if (is_started) {
		last_error = "Debugger client already started";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	if (client_config->dbgr_host_launch) {
		// Run a copy of gix_debugger
		if (!launch_local_debugger_host()) {
			// last_error set above
			return false;
		}
	}

	network_manager->setRemoteAddr(client_config->getHostDebuggerAddr());
	network_manager->setRemotePort(client_config->getHostDebuggerPort());
	
	network_manager->setLocalAddr(client_config->getBindingAddr());
	network_manager->setLocalPort(client_config->getBindingPort());

	network_manager->onDataReceived = [this](NetworkManager* nws, std::string data, bool* response_sent) {

		*response_sent = true;

		std::string parse_err;
		DebuggerHostOutputMessage rcvd_msg;
		__LOG("Client received: " + data, LOG_LEVEL_TRACE);
		if (!rcvd_msg.deserialize(data)) {
			last_error = "Cannot deserialize data (" + data + ")";
			__LOG(last_error, LOG_LEVEL_ERROR);
			return false;
		}

		if (!handle_incoming_message(rcvd_msg, response_sent)) {
			// error must be set/logged in handle_incoming_message
			if (!*response_sent)
				nws->emit_response_ok("Good to know");

			return false;
		}

		// We ALWAYS need to acknowledge the incoming message
		if (!*response_sent)
			nws->emit_response_ok("Good to know");

		return true;
	};

	if (!network_manager->init()) {
		last_error = "cannot initialize network manager: " + network_manager->last_error;
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	//if (!network_manager->send_rcv(req.serialize(), resp_content)) {
	//	resp.deserialize(resp_content);
	//	last_error = string_format("Cannot connect to debugger host (2) at %s:%d : %s", network_manager->remote_host, network_manager->remote_port, resp.message);
	//	return false;
	//}

	//resp.deserialize(resp_content);

	//if (resp.type != DBGR_MSG_OUT_TYPE_STATUS ||  resp.status != DBGR_STATUS_OK) {
	//	last_error = resp.message;
	//	return false;
	//}

	is_started = true;
	last_error = "";
	__LOG("Debugger client initialized", LOG_LEVEL_INFO);
	return true;
}

bool GixDebuggerClient::cleanup()
{
	if (is_cleaned_up)
		return true;

	if (client_config && client_config->dbgr_host_launch && launched_dbgr_host_success) {

		if (is_started) {
			stopSession();
		}

		std::string resp;
		DebuggerHostInputMessage m(DBGR_MSG_IN_CMD_QUIT);
		std::string data = m.serialize();

		// We are cleaning up, potentially the debugger host is not even running, so we don't wait for an answer
		if (!network_manager->send_rcv(data, resp, 3000)) {
			DebuggerHostOutputMessage rm;
			rm.deserialize(resp);
			last_error = rm.message;
			__LOG(last_error, LOG_LEVEL_ERROR);
		}

		// We try to terminate the debugger host, if it is still  running

#ifdef _WIN32
		DWORD rc = WaitForSingleObject(launched_dbgr_host_process_info.hProcess, 5 * 1000);
		if (rc != WAIT_OBJECT_0) {
			// Still running, maybe it hung? We try to kill it
			if (!TerminateProcess(launched_dbgr_host_process_info.hProcess, 0x00)) {
				// Failed, still running?
				last_error = "debugger host process is possibily still running";
				__LOG(last_error, LOG_LEVEL_WARN);
			}
			else {
				rc = WaitForSingleObject(launched_dbgr_host_process_info.hProcess, 5 * 1000);
				if (rc != WAIT_OBJECT_0) {
					last_error = "debugger host process is possibily still running";
					__LOG(last_error, LOG_LEVEL_WARN);
				}
			}
		}
#endif

#ifdef __linux__

#endif
	}

	if (network_manager)
		delete network_manager;

	is_cleaned_up = true;

	return true;
}

bool GixDebuggerClient::GixDebuggerClient::startSession()
{
	CHECK_CLIENT_STARTED

	if (program_is_running) {
		last_error = "A program is already running";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	if (!client_config) {
		last_error = "Configuration not set";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	DebuggerHostInputMessage c(DBGR_MSG_IN_CMD_START_SESSION);
	c.payload1 = *client_config;
	std::string resp;
	std::string data = c.serialize();
	if (!network_manager->send_rcv(data, resp)) {
		DebuggerHostOutputMessage rm;
		rm.deserialize(resp);
		last_error = rm.message;
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	return true;
}

bool GixDebuggerClient::GixDebuggerClient::stopSession()
{
	if (!is_started)
		return true;

	std::string resp;
	DebuggerHostInputMessage m(DBGR_MSG_IN_CMD_STOP_SESSION);
	std::string data = m.serialize();
	bool b = network_manager->send_rcv(data, resp);
	DebuggerHostOutputMessage rm;
	rm.deserialize(resp);

	if (!b) {
		last_error = rm.message;
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	exit_code = rm.payload1.toInt();
	exit_message = rm.message;

	is_started = false;
	return true;
}

bool GixDebuggerClient::step()
{
	CHECK_CLIENT_STARTED

	std::string resp;
	DebuggerHostInputMessage m(DBGR_MSG_IN_CMD_STEP);
	std::string data = m.serialize();
	if (!network_manager->send_rcv(data, resp)) {
		DebuggerHostOutputMessage rm;
		rm.deserialize(resp);
		last_error = rm.message;
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}
	return true;
}

bool GixDebuggerClient::continue_running()
{
	CHECK_CLIENT_STARTED

	std::string resp;
	DebuggerHostInputMessage m(DBGR_MSG_IN_CMD_CONTINUE);
	std::string data = m.serialize();
	if (!network_manager->send_rcv(data, resp)) {
		DebuggerHostOutputMessage rm;
		rm.deserialize(resp);
		last_error = rm.message;
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}
	return true;
}

bool GixDebuggerClient::getVariables(const std::vector<std::string>& var_names, std::map<std::string, struct VariableDisplayData>& var_data_req)
{
	CHECK_CLIENT_STARTED

	std::string resp;
	DebuggerHostInputMessage m(DBGR_MSG_IN_CMD_GET_VARS);
	m.payload1 = var_names;
	std::string data = m.serialize();
	bool b = network_manager->send_rcv(data, resp);
	DebuggerHostOutputMessage rm;
	rm.deserialize(resp);
	if (!b) {
		last_error = rm.message;
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	var_data_req = rm.payload1.toVariableDisplayDataMap();
	return true;
}

bool GixDebuggerClient::enumPrograms(std::map<PID_T, std::string> programs)
{
	CHECK_CLIENT_STARTED

		return false;
}

DebuggerHostOutputMessage GixDebuggerClient::getStatus()
{
	std::string resp;
	DebuggerHostInputMessage req("status");

	if (network_manager->is_initialized()) {
		DebuggerHostOutputMessage ko(DBGR_MSG_OUT_TYPE_STATUS, DBGR_STATUS_KO, "Status unknown, host unreachable or not started");
		last_error = "Status unknown, host unreachable or not started";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return ko;
	}

	if (!network_manager->send_rcv(req.serialize(), resp)) {
		DebuggerHostOutputMessage ko(DBGR_MSG_OUT_TYPE_STATUS, DBGR_STATUS_KO, "Status unknown, host unreachable or not started");
		last_error = "Status unknown, host unreachable or not started";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return ko;
	}

	last_error = "";
	return DebuggerHostOutputMessage();
}

std::string GixDebuggerClient::get_last_error()
{
	return last_error;
}

std::string GixDebuggerClient::get_last_output()
{
	return last_output;
}

int GixDebuggerClient::get_exit_code()
{
	return exit_code;
}

std::string GixDebuggerClient::get_exit_message()
{
	return exit_message;
}


bool GixDebuggerClient::launch_local_debugger_host()
{
	static std::vector<std::string> log_level_t = { "trace", "debug", "info", "warn", "error", "critical" };

	__LOG("Launching locally debugger host", LOG_LEVEL_INFO);
#ifdef _DEBUG
	std::string arch = "x64";
#else
	if (client_config->program.empty() || !file_exists(client_config->program)) {
		last_error = "The program specified is invalid or missing (" + (client_config->program.empty() ? "N/A" : client_config->program) + ")";
		return false;
	}

	std::string arch = get_arch(client_config->program);
	if (arch.empty()) {
		last_error = "Invalid or unsupported architecture: " + arch;
		return false;
	}
#endif

	__LOG("Architecture is " + arch, LOG_LEVEL_INFO);

	std::string host_exe_name = "gix-debugger-" + arch;

	std::string args = string_format("-v -s %s -p %d", client_config->getHostDebuggerAddr(), client_config->getHostDebuggerPort());

	if (client_config->dbgr_host_encrypt)
		args += " --encrypt";

	if (client_config->dbgr_host_session_type == DebuggerSessionType::Debug)
		args += " --debug";

	if (client_config->dbgr_host_local_log_level < LOG_LEVEL_TRACE || client_config->dbgr_host_local_log_level > LOG_LEVEL_CRITICAL) {
		last_error = "Invalid log level: " + std::to_string(client_config->dbgr_host_local_log_level);
		return false;
	}

	if (client_config->dbgr_host_remote_log_level < LOG_LEVEL_TRACE || client_config->dbgr_host_remote_log_level > LOG_LEVEL_CRITICAL) {
		last_error = "Invalid log level: " + std::to_string(client_config->dbgr_host_remote_log_level);
		return false;
	}

	std::string lll = log_level_t.at(client_config->dbgr_host_local_log_level);
	std::string rll = log_level_t.at(client_config->dbgr_host_remote_log_level);

	args += " -l " + client_config->dbgr_host_local_log_file + " -L " + lll + "," + rll;

	__LOG("Launching: " + host_exe_name + " " + args, LOG_LEVEL_DEBUG);

#ifdef _WIN32
	host_exe_name += ".exe";

	char actual_path[MAX_PATH];
	strcpy(actual_path, host_exe_name.c_str());

	char gix_ide_exe_path[MAX_PATH];
	int l = 0;
	if ((l = GetModuleFileName(NULL, gix_ide_exe_path, MAX_PATH)) == 0) {
		last_error = "Cannot locate current directory";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	std::filesystem::path p(gix_ide_exe_path);
	p = p.parent_path();
	std::string cur_dir = p.make_preferred().string();
	
	const char* dirs[] = { cur_dir.c_str(), NULL};

	if (!PathFindOnPath(actual_path, dirs)) {
		last_error = "Cannot locate host debugger executable\"" + host_exe_name + "\"";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	STARTUPINFO si;
	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);

	ZeroMemory(&launched_dbgr_host_process_info, sizeof(launched_dbgr_host_process_info));
	
	DWORD createFlags = NORMAL_PRIORITY_CLASS | (client_config->dbgr_host_new_window ? CREATE_NEW_CONSOLE : CREATE_NO_WINDOW);

	if (!CreateProcess(actual_path, (char*)args.c_str(), NULL, NULL, TRUE, createFlags, NULL, NULL, &si, &launched_dbgr_host_process_info)) {
		last_error = "Cannot launch host debugger executable\"" + host_exe_name + "\"";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

    // Let's wait a little for the debugger to start listening   
    Sleep(500);    
#endif

#ifdef __linux__

	char gix_ide_exe_path[4096];
	int l = 0;
    if (readlink("/proc/self/exe", (char*)&gix_ide_exe_path, sizeof (gix_ide_exe_path)) <= 0) {
		last_error = "Cannot locate current directory";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}

	std::filesystem::path p(gix_ide_exe_path);
	p = p.parent_path();
	p = p.make_preferred().append(host_exe_name);
	
	if (!std::filesystem::exists(p) || !std::filesystem::is_regular_file(p)) {
		last_error = "Cannot locate host debugger executable\"" + p.string() + "\"";
		__LOG(last_error, LOG_LEVEL_ERROR);
		return false;
	}
    
    std::vector<std::string> cmd_args;
    split_in_args(cmd_args, args, true);
    cmd_args.insert(cmd_args.begin(), host_exe_name);
    
    char **argv = new char*[cmd_args.size() + 1];
    for (int i = 0; i < cmd_args.size(); i++)
        argv[i] = (char *)cmd_args[i].c_str();
    
    argv[cmd_args.size()] = 0;
    
    std::string cmd_line = p.string();
    int fork_dbgr = fork();
    if (!fork_dbgr){

        execv(cmd_line.c_str(), argv);
    
        // in case execl fails
        _exit(1);
    }
    else if (fork_dbgr == -1)
    {
        last_error = "Cannot launch host debugger executable\"" + p.string() + "\"";
        __LOG(last_error, LOG_LEVEL_ERROR);
        return false;
    }
    
    // Let's wait a little for the debugger to start listening   
    usleep(500 * 1000);
#endif


	__LOG("Successsfully launched " + host_exe_name, LOG_LEVEL_INFO);    
    
	last_error = "";
	launched_dbgr_host_success = true;
	return true;
}

std::string GixDebuggerClient::get_arch(std::string p)
{
	std::string arch;

#ifdef _WIN32
	FILE* f = fopen(p.c_str(), "rb");
	if (f) {
		uint32_t offset = 0;
		if (fseek(f, 0x3c, SEEK_SET)) return arch;
		fread(&offset, sizeof(offset), 1, f);
		if (ferror(f) || feof(f)) return arch;
		if (fseek(f, offset + 4, SEEK_SET)) return arch;

		uint16_t machine = 0;
		fread(&machine, sizeof(machine), 1, f);
		if (ferror(f) || feof(f)) return arch;
		fclose(f);

		if (machine == IMAGE_FILE_MACHINE_AMD64)
			arch = "x64";

		if (machine == IMAGE_FILE_MACHINE_I386)
			arch = "x86";
	}
#endif

#ifdef __linux__
    FILE* f = fopen(p.c_str(), "rb");
	if (f) {
        if (fseek(f, 0x04, SEEK_SET)) {
            fclose(f);
            std::string s = strerror(errno);
            fprintf(stderr, "%s", s.c_str());
            return arch;
        }
        
		uint8_t elf_class = 0;
		fread(&elf_class, sizeof(elf_class), 1, f);
        fclose(f);

		if (elf_class == 2)
			arch = "x64";

		if (elf_class == 1)
			arch = "x86";
	}
#endif

	return arch;
}

bool GixDebuggerClient::handle_incoming_message(const DebuggerHostOutputMessage& msg, bool* response_sent)
{
	*response_sent = false;

	if (msg.type == DBGR_MSG_OUT_TYPE_MESSAGE) {
		if (debuggerMessage != nullptr) {
			return debuggerMessage(this, msg.payload1.toString(), msg.payload2.toInt());
		}
	}

	if (msg.type == DBGR_MSG_OUT_TYPE_READY) {
		if (debuggerReady != nullptr) {
			return debuggerReady(this, msg.payload1.toString());
		}
	}

	if (msg.type == DBGR_MSG_OUT_TYPE_PROC_START) {
		if (debuggerProcessStarted != nullptr) {
			return debuggerProcessStarted(this, msg.payload1.toString());
		}
	}

	if (msg.type == DBGR_MSG_OUT_TYPE_PROC_EXIT) {
		if (debuggerProcessExit!= nullptr) {
			return debuggerProcessExit(this, msg.payload1.toInt(), msg.payload2.toString());
		}
	}

	if (msg.type == DBGR_MSG_OUT_TYPE_STDOUT_AVAILABLE) {
		if (debuggerStdOutAvailable != nullptr) {
			return debuggerStdOutAvailable(this, msg.payload1.toString());
		}
	}

	if (msg.type == DBGR_MSG_OUT_TYPE_STDERR_AVAILABLE) {
		if (debuggerStdErrAvailable != nullptr) {
			return debuggerStdErrAvailable(this, msg.payload1.toString());
		}
	}

	if (msg.type == DBGR_MSG_OUT_TYPE_BREAK) {
		if (debuggerBreak != nullptr) {
			DebuggerHostInputMessage r(DBGR_MSG_OUT_TYPE_BREAK "_reply");
			r.status = DBGR_STATUS_OK;
			*response_sent = this->network_manager->reply(r);
			return debuggerBreak(this, msg.payload1.toString(), msg.payload2.toString(), msg.payload3.toInt());
		}
	}

	if (msg.type == DBGR_MSG_OUT_TYPE_GET_BRKPS) {
		if (getBreakpoints != nullptr) {
			std::vector<std::pair<std::string, int>> user_breakpoints;
			if (getBreakpoints(this, user_breakpoints)) {
				std::vector<std::string> v;
				for (auto it = user_breakpoints.begin(); it != user_breakpoints.end(); ++it) {
					v.push_back(std::to_string(it->second) + "@" + it->first);
				}
				DebuggerHostInputMessage r(DBGR_MSG_OUT_TYPE_GET_BRKPS "_reply");
				r.payload1 = v;
				r.status = DBGR_STATUS_OK;
				bool b = this->network_manager->reply(r);
				*response_sent = b;
				return b;
			}
		}
	}

	return false;
}
