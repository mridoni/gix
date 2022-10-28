#include "GixDebuggerSessionConfig.h"
#include "json11.hpp"

void GixDebuggerSessionConfig::setHostDebugger(const std::string& h, uint16_t p)
{
	dbgr_host_addr = h;
	dbgr_host_port = p;
}

void GixDebuggerSessionConfig::setHostSessionType(DebuggerSessionType t)
{
	dbgr_host_session_type = t;
}

void GixDebuggerSessionConfig::setHostEncrypt(bool b)
{
	dbgr_host_encrypt = b;
}

void GixDebuggerSessionConfig::setHostProperty(const std::string& k, const std::string& v)
{
	dbgr_host_properties[k] = v;
}

void GixDebuggerSessionConfig::setHostProperties(const std::map<std::string, std::string>& props)
{
	dbgr_host_properties = props;
}

void GixDebuggerSessionConfig::setHostLaunch(bool b)
{
	dbgr_host_launch = b;
}

void GixDebuggerSessionConfig::setHostLocalLogLevel(int ll)
{
	dbgr_host_local_log_level = ll;
}

void GixDebuggerSessionConfig::setHostLocalLogFile(const std::string lf)
{
	dbgr_host_local_log_file = lf;
}

void GixDebuggerSessionConfig::setHostRemoteLogLevel(int ll)
{
	dbgr_host_remote_log_level = ll;
}

void GixDebuggerSessionConfig::setHostOpensInNewWindow(bool b)
{
	dbgr_host_new_window = b;
}

void GixDebuggerSessionConfig::setPid(uint64_t p)
{
	pid = p;
}

void GixDebuggerSessionConfig::setProgram(const std::string& p)
{
	program = p;
}

void GixDebuggerSessionConfig::setProgramArgs(const std::vector<std::string>& v)
{
	//std::string s;

	//for (std::vector< std::string>::const_iterator p = v.begin();
	//	p != v.end(); ++p) {
	//	s += *p;
	//	if (p != v.end() - 1)
	//		s += ' ';
	//}
	//program_args = s;
	program_args = v;
}

void GixDebuggerSessionConfig::setWorkingDirectory(const std::string& wd)
{
	working_dir = wd;
}

void GixDebuggerSessionConfig::setBuildDirectory(const std::string& bd)
{
	build_dir = bd;
}


void GixDebuggerSessionConfig::setModuleDirectory(const std::string& md)
{
	module_dir = md;
}

void GixDebuggerSessionConfig::setDebuggedModuleType(DebuggedModuleType mt)
{
	module_type = mt;
}

void GixDebuggerSessionConfig::setUseExternalConsole(bool b)
{
	use_external_console = b;
}

void GixDebuggerSessionConfig::setStdInFile(const std::string& si)
{
	stdin_file = si;
}

void GixDebuggerSessionConfig::setMergeEnv(bool b)
{
	cfg_merge_env = b;
}

void GixDebuggerSessionConfig::setEnvironment(const std::map<std::string, std::string>& env)
{
	environment = env;
}

void GixDebuggerSessionConfig::clearEnvironment()
{
	environment.clear();
}

void GixDebuggerSessionConfig::setEnvironmentVariable(const std::string& k, const std::string& v)
{
	std::string tv;
	if (!cfg_merge_env || environment.find(k) != environment.end()) {
		tv = v;
	}
	else {
#ifdef _WIN32
		tv = v + ";" + environment[k];
#else
		tv = v + ":" + environment[k];
#endif

		environment[k] = tv;
	}
}

bool GixDebuggerSessionConfig::setEnvironmentVariable(const std::string& kv)
{
	if (kv.empty() || kv.find('=') == std::string::npos)
		return false;

	int p = kv.find('=');
	if (p == kv.size() - 1)
		return false;

	std::string k = kv.substr(0, p);
	std::string v = kv.substr(p + 1);

	setEnvironmentVariable(k, v);
	return true;
}

void GixDebuggerSessionConfig::delEnvironmentVariable(const std::string& k)
{
	if (environment.find(k) != environment.end()) {
		environment.erase(k);
	}
}

bool GixDebuggerSessionConfig::getEnvironmentVariable(const std::string& k, std::string& v)
{
	if (environment.find(k) != environment.end()) {
		v = environment[k];
		return true;
	}

	return false;
}

json11::Json GixDebuggerSessionConfig::to_json() const {

	return json11::Json::object{
	{ "__obj_type__", "GixDebuggerSessionConfig"},
	{ "dbgr_host_addr", dbgr_host_addr },
	{ "dbgr_host_port", (int)dbgr_host_port },
	{ "dbgr_host_local_log_level", dbgr_host_local_log_level },
	{ "dbgr_host_local_log_file", dbgr_host_local_log_file },
	{ "dbgr_host_remote_log_level", dbgr_host_remote_log_level },
	{ "dbgr_host_encrypt", dbgr_host_encrypt },
	{ "dbgr_host_properties", dbgr_host_properties },
	{ "dbgr_host_session_type", (int)dbgr_host_session_type },
	{ "pid", (double)pid },
	{ "program", program },
	{ "program_args", program_args },
	{ "working_dir", working_dir },
	{ "module_dir", module_dir },
	{ "module_type", (int)module_type },
	{ "use_external_console", use_external_console },
	{ "use_host_external_console", use_host_external_console },
	{ "stdin_file", stdin_file },
	{ "environment", environment }
	};
}

std::string GixDebuggerSessionConfig::getHostDebuggerAddr()
{
	return this->dbgr_host_addr;
}
uint16_t GixDebuggerSessionConfig::getHostDebuggerPort()
{
	return this->dbgr_host_port;
}

void GixDebuggerSessionConfig::setBinding(const std::string& h, uint16_t p)
{
	binding_addr = h;
	binding_port = p;
}

void GixDebuggerSessionConfig::setBindingAddr(const std::string& h)
{
	binding_addr = h;
}

void GixDebuggerSessionConfig::setBindingPort(uint16_t p)
{
	binding_port = p;
}

std::string GixDebuggerSessionConfig::getBindingAddr()
{
	return binding_addr;
}

uint16_t GixDebuggerSessionConfig::getBindingPort()
{
	return binding_port;
}


//std::string GixDebuggerSessionConfig::serialize()
//{
//	json11::Json obj = json11::Json::object{
//		{ "dbgr_host_addr", dbgr_host_addr },
//		{ "dbgr_host_port", dbgr_host_port },
//		{ "dbgr_host_verbose", dbgr_host_verbose },
//		{ "dbgr_host_encrypt", dbgr_host_encrypt },
//		{ "dbgr_host_debug_enabled", dbgr_host_debug_enabled },
//		{ "dbgr_host_properties", dbgr_host_properties },
//		{ "program", program },
//		{ "program_args", program_args },
//		{ "working_dir", working_dir },
//		{ "module_dir", module_dir },
//		{ "module_type", (int)module_type },
//		{ "use_external_console", use_external_console },
//		{ "use_host_external_console", use_host_external_console },
//		{ "stdin_file", stdin_file },
//		{ "environment", environment }
//	};
//
//	return obj.dump();
//}
