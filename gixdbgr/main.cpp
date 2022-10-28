#define NOMINMAX

#include <iostream>

#include "GixDebuggerClient.h"
#include "GixDebuggerSessionConfig.h"

#include "linenoise.h"
#include "libcpputils.h"
#include "debugger-msg-defs.h"
#include "popl.hpp"

#define GIXDBGR_VER	"1.1.0"

#define DEFAULT_AUTOSTART_CLIENT	true

using namespace popl;

static int handle_commands();
static std::string get_history_filename();
static bool process_cmd(std::string cmdline);

std::unique_ptr<GixDebuggerClient> client;
std::shared_ptr<GixDebuggerSessionConfig> session_config;

int main(int argc, char** argv)
{
	int rc = -1;

	// Do processing here
	const auto args = argv;

	char vbfr[1024];
	sprintf(vbfr, "gixdbgr - Command line debugger interface\nVersion: %s\n\nOptions", GIXDBGR_VER);

	OptionParser options(vbfr);

	auto opt_help = options.add<Switch>("h", "help", "displays help on commandline options");
	auto opt_version = options.add<Switch>("V", "version", "displays version information");
	auto opt_autostart = options.add<Switch>("A", "autostart", "auto-start debugger client");
	auto opt_cmdfile = options.add<Value<std::string>>("c", "cmdfile", "command file");
	auto opt_host = options.add<Value<std::string>>("H", "host", "debug host address");
	auto opt_port = options.add<Value<uint16_t>>("p", "port", "debug host port");

	// Options
	bool autostart = opt_autostart->is_set() || DEFAULT_AUTOSTART_CLIENT;
	std::string dbgr_host_addr = "127.0.0.1";
	uint16_t dbgr_host_port = 13009;
	std::string cmd_file;
	// Options (end)

	options.parse(argc, argv);

	if (opt_host->is_set()) {
		dbgr_host_addr = opt_host->value();
	}

	if (opt_port->is_set()) {
		dbgr_host_port = opt_port->value();
	}

	if (opt_cmdfile->is_set()) {
		cmd_file = opt_cmdfile->value();
	}

	if (opt_help->is_set()) {
		rc = 0;
		std::cout << options << std::endl;
	}
	else {
		if (opt_version->is_set()) {
			sprintf(vbfr, "gixdbgr - Command line debugger interface\nVersion: %s\n\nOptions", GIXDBGR_VER);
			std::cout << vbfr << std::endl;
		}
		else {
			session_config = std::make_shared<GixDebuggerSessionConfig>();

			if (!cmd_file.empty()) {
				auto lines = file_read_all_lines(cmd_file);
				for (auto line : lines) {
					//std::cout << string_format("@%s: %s", cmd_file, line) << std::endl;
					std::cout << "\x1b[1;32mgixdbgr\x1b[0m> " << line << std::endl;
					process_cmd(line);
				}
			}
			else {
				printf("gixdbgr - Command line interface for gix-debugger (version %s)\n", GIXDBGR_VER);
			}

			rc = handle_commands();
		}
	}

	return rc;
}

static int handle_commands()
{
	std::string history_filename = get_history_filename();
	linenoiseHistoryLoad(history_filename.c_str());
	//linenoiseSetCompletionCallback(completionHook);

	char const* prompt = "\x1b[1;32mgixdbgr\x1b[0m> ";

	while (1) {
		char* result = linenoise(prompt);

		if (result == NULL) {
			break;
		}
		else if (!strncmp(result, "/history", 8)) {
			/* Display the current history. */
			for (int index = 0; ; ++index) {
				char* hist = linenoiseHistoryLine(index);
				if (hist == NULL) break;
				printf("%4d: %s\n", index, hist);
				free(hist);
			}
		}
		if (*result == '\0') {
			free(result);
			break;
		}

		linenoiseHistoryAdd(result);

		if (process_cmd(result))
			break;

		free(result);
	}

	linenoiseHistorySave(history_filename.c_str());
	linenoiseHistoryFree();

	return 0;
}

std::string get_history_filename()
{
	char bfr[4096];
	std::string homedir;
#if defined(_WIN32)
	if (GetEnvironmentVariable("USERPROFILE", bfr, sizeof(bfr)) != ERROR_ENVVAR_NOT_FOUND) {
		strcat(bfr, "\\.gixdbgr_history");
		homedir = bfr;
	}

#else
	char* c = getenv("HOME");
	if (c) {
		strcpy(bfr, c);
		strcat(bfr, "/.gixdbgr_history");
	}
#endif

	return homedir;
}

#define CMD_ERR(E) { b = false; err = E; }

bool try_get_bool(std::string s, bool* b)
{
	if (s == "on" || s == "off") {
		*b = (s == "on");
		return true;
	}

	return false;
}

bool process_cmd(std::string cmdline)
{
	std::string output;
	std::vector<std::string> items;

	split_in_args(items, cmdline, true);

	if (items.size() == 0)
		return true;

	std::string cmd = items[0];
	if (items.size() == 1)
		items.clear();
	else
		items.erase(items.begin());

	bool b = false;
	bool flag = false;
	std::string err;

	if (cmd == "run") {
		client = std::make_unique<GixDebuggerClient>();
		client->setClientConfig(session_config.get());

		client->debuggerMessage = [](GixDebuggerClient* gdc, std::string msg, int level) {

			fprintf(stderr, ("]] Debugger host says: " + msg + "\n").c_str());
			return true;
		};

		client->getBreakpoints = [](GixDebuggerClient* gdc, std::vector<std::pair<std::string, int>>& brkps) {

			brkps.clear();
			brkps.push_back(std::pair("C:\\Users\\marchetto\\Documents\\gix\\test000\\TEST000.cbl", 23));
			return true;
		};

		client->debuggerBreak = [](GixDebuggerClient* gdc, std::string s1, std::string s2, int i) {

			std::vector<std::string> var_names = { "A" };
			std::map<std::string, VariableDisplayData> var_data;
			bool b = client->getVariables(var_names, var_data);
			return true;
		};


		if (!client->init()) {
			err = "Cannot initialize client: " + client->get_last_error();
		}

		b = client->startSession();
		if (!b) {
			err = "Cannot start debugging session: " + client->get_last_error();
		}
	}

	if (cmd == "setenv") {
		b = session_config->setEnvironmentVariable(items[0]);
		if (!b)
			err = "Cannot set environment variable (program already started)?";
	}

	if (cmd == "set") {
		if (items.size() < 2) {
			err = "Syntax error";
			b = false;
		}

		std::string key = items[0];
		if (key == "host") {
			int p = atoi(items[2].c_str());
			if (!p || p > 65535) {
				CMD_ERR("Invalid port");
			}
			else {
				session_config->setHostDebugger(items[1], (uint16_t)p);
				b = true;
			}
		}

		if (key == "working-dir") {
			session_config->setWorkingDirectory(items[1]);
			b = true;
		}

		if (key == "envvar") {
			session_config->setEnvironmentVariable(items[1]);
			b = true;
		}


		if (key == "program") {
			session_config->setProgram(items[1]);
			b = true;
		}

		if (key == "program-args") {
			std::vector<std::string> m_items;
			for (int i = 1; i < items.size(); i++)
				m_items.push_back(items.at(i));

			session_config->setProgramArgs(m_items);
			b = true;
		}

		if (key == "external-console") {
			if (try_get_bool(items[1], &flag)) {
				session_config->setUseExternalConsole(flag);
				b = true;
			}
			else {
				b = false;
				err = "Bad parameter";
			}
		}

		if (key == "mergeenv") {
			if (try_get_bool(items[1], &flag)) {
				session_config->setMergeEnv(flag);
				b = true;
			}
			else {
				b = false;
				err = "Bad parameter";
			}
		}

		if (key == "encrypt") {
			if (try_get_bool(items[1], &flag)) {
				session_config->setHostEncrypt(flag);
				b = true;
			}
			else {
				b = false;
				err = "Bad parameter";
			}
		}

		//if (key == "verbose") {
		//	if (try_get_bool(items[1], &flag)) {
		//		session_config->setHostVerbose(flag);
		//		b = true;
		//	}
		//	else {
		//		b = false;
		//		err = "Bad parameter";
		//	}
		//}

		if (key == "debug") {
			if (try_get_bool(items[1], &flag)) {
				session_config->setHostSessionType(DebuggerSessionType::Debug);
				b = true;
			}
			else {
				err = "Bad parameter";
			}
		}
	}

	if (b)
		printf("OK\n");
	else
		printf("ERROR: %s\n", err.c_str());

	return false;
}
