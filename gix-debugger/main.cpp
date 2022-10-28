// gix-debugger.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#define NOMINMAX 

#include "NetworkManager.h"

#include <iostream>
#include <string>
#include <vector>
#include <functional>

#include "GixDebugger.h"
#include "DbgrCommandHandler.h"

#include "popl.hpp"

#include "spdlog/spdlog.h"
#include "spdlog/sinks/msvc_sink.h"

#include <json11.hpp>
#include <spdlog/sinks/basic_file_sink.h>
#include <spdlog/sinks/stdout_color_sinks.h>

#include "debugger_host_sink.h"

#define GIXDBGR_VER	"1.1.0dev1"

#define DBGR_HOST_DEFAULT_ADDR	"127.0.0.1"
#define DBGR_HOST_DEFAULT_PORT	13009

#ifdef _WIN32
#define _CUR_THREAD_ID_ GetCurrentThreadId()
#else
#define _CUR_THREAD_ID_ gettid()
#endif

enum class DebuggerMode
{
	Local = 1,
	Remote = 2
};

static char* env_str = nullptr;
static char* env_file = nullptr;
static char* working_dir = nullptr;

static bool set_log_levels(const std::string& logdef, spdlog::level::level_enum *local, spdlog::level::level_enum *remote);

/*
	Debugger host           : the gix-debugger executable, handles commands and actually controls the program being debugged
	Debugger client         : any program that sends commands to the Debugger Host (e.g. Gix-IDE)
	Debugger client library : the client library used by Debugger Client implementations to communicate with a Debugger Host
*/

static std::string dbg_host_addr = DBGR_HOST_DEFAULT_ADDR;
static uint16_t dbg_host_port = DBGR_HOST_DEFAULT_PORT;

static bool encrypt = false;
static bool verbose = false;
static bool debug = false;

static DbgrCommandHandler dbgr_cmd_handler;
static std::queue<DebuggerHostInputMessage> cmd_queue;
static std::condition_variable cv;
static std::mutex cv_m;

std::thread debug_session_thread;

using namespace popl;

int main(int argc, char** argv)
{
	int rc = 0;

	const auto args = argv;

	char vbfr[1024];
	sprintf(vbfr, "gix-debugger - The debugger for Gix-IDE (and more)\nVersion: %s\n\nOptions", GIXDBGR_VER);

	OptionParser options(vbfr);

	auto opt_help = options.add<Switch>("h", "help", "displays help on commandline options");
	auto opt_version = options.add<Switch>("V", "version", "displays version information");
	auto opt_verbose = options.add<Switch>("v", "verbose", "verbose output");
	auto opt_debug = options.add<Switch>("d", "debug", "emit output for debugging gix-debugger itself");
	auto opt_host = options.add<Value<std::string>>("s", "host", "debugger bind address");
	auto opt_port = options.add<Value<int>>("p", "port", "debugger port");
	auto opt_encrypt = options.add<Switch>("e", "encrypt", "encrypt data exchange");
	auto opt_logfile = options.add<Value<std::string>>("l", "logfile", "log file");
	auto opt_loglevel = options.add<Value<std::string>>("L", "loglevel", "log level (=<local>,<remote>|=<global>");

	options.parse(argc, argv);

	if (opt_help->is_set()) {
		std::cout << options << std::endl;
		return 0;
	}

	if (opt_version->is_set()) {
		sprintf(vbfr, "gix-debugger - The debugger for Gix-IDE (and more)\nVersion: %s\n\nOptions", GIXDBGR_VER);
		std::cout << vbfr << std::endl;
		return 0;
	}

	std::string logfile = opt_logfile->is_set() ? opt_logfile->value() : "gix-debugger-host.log";
	
	spdlog::level::level_enum local_log_level = spdlog::level::warn;
	spdlog::level::level_enum remote_log_level = spdlog::level::warn;

	if (opt_loglevel->is_set()) {
		if (!set_log_levels(opt_loglevel->value(), &local_log_level, &remote_log_level)) {
			fprintf(stderr, "Invalid log level definition");
			return 1;
		}
	}
	auto local_sink = std::make_shared<spdlog::sinks::basic_file_sink_mt>(logfile, true);
	auto remote_sink = std::make_shared<debugger_host_sink_mt>();
	std::vector<spdlog::sink_ptr> sinks = { local_sink, remote_sink };
	//std::vector<spdlog::sink_ptr> sinks = { local_sink };
	auto logger = std::make_shared<spdlog::logger>("gix-debugger-host", begin(sinks), end(sinks));
	spdlog::set_default_logger(logger);
	spdlog::set_level(spdlog::level::trace);	// max log level, will be limited by the sink-specific levels
	local_sink->set_level(local_log_level);
	remote_sink->set_level(remote_log_level);
	logger->flush_on(spdlog::level::trace);

	spdlog::info("gix-debugger is starting (thread id: {})", _CUR_THREAD_ID_);
	spdlog::info("Local log level is set to: {}", spdlog::level::to_string_view(local_log_level));
	spdlog::info("Remote log level is set to: {}", spdlog::level::to_string_view(remote_log_level));

	if (opt_host->is_set()) {
		dbg_host_addr = opt_host->value();
	}

	if (opt_port->is_set()) {
		dbg_host_port = opt_port->value();
	}

	verbose = opt_verbose->is_set();
	debug = opt_debug->is_set();

	// Non-option arguments
	std::vector<std::string> extra_args = options.non_option_args();

	std::shared_ptr<NetworkManager> nm(new NetworkManager(NetworkManagerMode::Passive));
	nm->setLocalAddr(dbg_host_addr);
	nm->setLocalPort(dbg_host_port);

	/*
		The debugging engine (GixDebugger and its derived classes) sends data to the client
		through the NetworkServer. This handles data sent from the client directly to
		the NetworkServer while it is running (e.g. a stop request)
	*/
	nm.get()->onDataReceived = [&nm](NetworkManager* nws, std::string data, bool* response_sent) {

		spdlog::trace("Handling content: {}", data);

		std::string parse_err;
		DebuggerHostInputMessage rcvd_msg;
		if (!rcvd_msg.deserialize(data)) {
			nm->emit_response_ko("Syntax error");
			*response_sent = true;
			return false;
		}
		
		spdlog::trace("Pushing message {} (thread id: {})", rcvd_msg.cmd, _CUR_THREAD_ID_);
		cmd_queue.push(rcvd_msg);
		cv.notify_one();
	};


	if (!nm->init()) {
		spdlog::error("gix-debugger: debugger host cannot initialize network manager, now exiting");
		return 1;
	}

	remote_sink->setNetworkManager(nm);

	bool keep_running = true;
	while (keep_running) {

		spdlog::trace("Acquiring command queue lock (thread id: {})", _CUR_THREAD_ID_);

		std::unique_lock<std::mutex> lk(cv_m);
		//cv.wait(lk, [] { return !cmd_queue.empty(); });
		cv.wait(lk);
		lk.unlock();
		spdlog::trace("Waking up and processing commands");
		while (!cmd_queue.empty()) {
			DebuggerHostInputMessage rcvd_msg = cmd_queue.front();
			spdlog::trace("Working on message {}", rcvd_msg.cmd);
			cmd_queue.pop();
			bool b = dbgr_cmd_handler.handleCommand(nm, rcvd_msg, &keep_running);
			spdlog::trace("finished queue loop iteration");
		}
		spdlog::trace("finished global loop iteration");
	}

	spdlog::trace("gix-debugger process is exiting ({})", rc);

	return rc;
}


void print_help()
{
	std::cout << "gix-debugger" << std::endl;
	std::cout << "Usage:" << std::endl;
	std::cout << "\t" << std::endl;
}

bool set_log_levels(const std::string& logdef, spdlog::level::level_enum* local, spdlog::level::level_enum* remote)
{
	std::string sll, slr;

	int p = logdef.find(",");
	if (p == std::string::npos) {
		sll = logdef;
		slr = logdef;
	}
	else {
		sll = logdef.substr(0, p);
		slr = logdef.substr(p + 1);
	}

	auto ll = spdlog::level::from_str(sll);
	auto lr = spdlog::level::from_str(slr);

	// Parse error
	if (ll == spdlog::level::off || lr == spdlog::level::off)
		return false;

	*local = ll;
	*remote = lr;

	return true;
}
