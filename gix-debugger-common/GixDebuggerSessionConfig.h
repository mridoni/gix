#pragma once

#include <string>
#include <vector>
#include <map>
#include <variant>

#include "json11.hpp"

#include "gix-debugger-types.h"

#define LOG_LEVEL_TRACE		0
#define LOG_LEVEL_DEBUG		1
#define LOG_LEVEL_INFO		2
#define LOG_LEVEL_WARN		3
#define LOG_LEVEL_ERROR		4
#define LOG_LEVEL_CRITICAL	5

class GixDebuggerSessionConfig
{
	friend class GixDebugger;
	friend class GixDebuggerClient;

public:

	// Debugger host features
	void setHostDebugger(const std::string& h, uint16_t p);
	std::string getHostDebuggerAddr();
	uint16_t getHostDebuggerPort();

	// Local binding for "reverse" connection (if empty the client will set a default)
	void setBinding(const std::string& h, uint16_t p);
	void setBindingAddr(const std::string& h);
	void setBindingPort(uint16_t p);
	std::string getBindingAddr();
	uint16_t getBindingPort();
	
	void setHostSessionType(DebuggerSessionType t);
	void setHostEncrypt(bool b);
	void setHostProperty(const std::string& k, const std::string& v);
	void setHostProperties(const std::map<std::string, std::string>& props);
	void setHostLaunch(bool b);
	void setHostLocalLogLevel(int ll);
	void setHostLocalLogFile(const std::string lf);
	void setHostRemoteLogLevel(int ll);
	void setHostOpensInNewWindow(bool b);	// Only applies to locally/auto-opened debugger host instances

	//  Program data
	void setPid(uint64_t p);
	void setProgram(const std::string& program);
	void setProgramArgs(const std::vector<std::string>& args);
	void setWorkingDirectory(const std::string& wd);
	void setModuleDirectory(const std::string& md);
	void setBuildDirectory(const std::string& bd);
	void setDebuggedModuleType(DebuggedModuleType mt);
	void setUseExternalConsole(bool b);
	void setStdInFile(const std::string& si);

	// Environment
	void setMergeEnv(bool b);
	void setEnvironment(const std::map<std::string, std::string>& env);
	void clearEnvironment();
	void setEnvironmentVariable(const std::string& k, const std::string& v);
	bool setEnvironmentVariable(const std::string& kv);
	void delEnvironmentVariable(const std::string& k);
	bool getEnvironmentVariable(const std::string& k, std::string& v);

	json11::Json to_json() const;

private:
	bool cfg_merge_env = true;

	std::string dbgr_host_addr;
	uint16_t dbgr_host_port;

	std::string binding_addr;
	uint16_t binding_port;

	int dbgr_host_local_log_level = LOG_LEVEL_WARN;
	int dbgr_host_remote_log_level = LOG_LEVEL_WARN;
	std::string dbgr_host_local_log_file = "gix-debugger-host.log";
	bool dbgr_host_encrypt = false;
	bool dbgr_host_launch = false;
	bool dbgr_host_new_window = false;
	DebuggerSessionType dbgr_host_session_type = DebuggerSessionType::NotSet;

	std::string program;
	uint64_t pid = 0;
	std::vector<std::string> program_args;
	std::string working_dir;
	std::string module_dir;
	std::string build_dir;
	DebuggedModuleType module_type;
	bool use_external_console;
	bool use_host_external_console;
	std::string stdin_file;

	std::map<std::string, std::string> environment;
	std::map<std::string, std::string> dbgr_host_properties;
};

