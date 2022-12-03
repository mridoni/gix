#include "ExperimentalDebugDriver.h"

#include "DebugManager.h"
#include "MetadataManager.h"
#include "Changeling.h"
#include "Ide.h"
#include "GixDebugger.h"
#include "DebugDriverUtils.h"
#include "DataEntry.h"
#include "gix-debugger-types.h"

#include "DebuggerHostInputMessage.h"
#include "DebuggerHostOutputMessage.h"
#include "debugger-msg-defs.h"
#include "NetworkManager.h"

#include <QThread>
#include <QHostAddress>
#include <QEventLoop>
#include <QDir>
#include <QPair>

#include <spdlog/spdlog.h>

void ExperimentalDebugDriver::startDriver()
{
	if (!debug_session_config) {
		emit DebuggerProcessError("Cannot start debug driver: invalid configuration", -1);
		return;
	}

	debugger_client_instance.setClientConfig(debug_session_config);

	debugger_client_instance.debuggerBreak = [this](GixDebuggerClient* gd, std::string module_name, std::string src_file, int line) {
		return dbgr_client_debuggerBreak(nullptr, module_name, src_file, line);
	};

	debugger_client_instance.getBreakpoints = [this](GixDebuggerClient* gd, std::vector<std::pair<std::string, int>>& bps) {
		return dbgr_client_getBreakpoints(nullptr, bps);
	};

	debugger_client_instance.debuggerMessage = [this](GixDebuggerClient* gd, std::string msg, int l) {
		logger->trace(LOG_DEBUG,"[DebuggerHost]: {}", msg);
		return true;
	};

	debugger_client_instance.debuggerError = [this](GixDebuggerClient* gd, int err_code, std::string msg) {
		return dbgr_client_debuggerError(nullptr, err_code, msg);
	};

	debugger_client_instance.debuggerProcessStarted = [this](GixDebuggerClient* gd, std::string msg) {
		return dbgr_client_debuggerProcessStarted(nullptr, msg);
	};

	debugger_client_instance.debuggerReady = [this](GixDebuggerClient* gd, std::string msg) {
		return dbgr_client_debuggerReady(nullptr, msg);
	};

	debugger_client_instance.debuggerProcessExit = [this](GixDebuggerClient* gd, int rc, std::string msg) {
		return dbgr_client_debuggerProcessExit(nullptr, rc, msg);
	};

	debugger_client_instance.debuggerStdOutAvailable = [this](GixDebuggerClient* gd, std::string msg) {
		return dbgr_client_debuggerStdOutAvailable(nullptr, msg);
	};

	debugger_client_instance.debuggerStdErrAvailable = [this](GixDebuggerClient* gd, std::string msg) {
		return dbgr_client_debuggerStdErrAvailable(nullptr, msg);
	};

#if 1
	debugger_client_instance.debuggerClientLog = [this](GixDebuggerClient* gd, std::string msg, int level) {
		logger->trace(LOG_DEBUG, "[DebuggerClient]: {}", msg);
		return true;
	};
#endif

	debugger_client_instance.debuggerHostLog = [this](GixDebuggerClient* gd, std::string msg, int level) {
		logger->trace(LOG_DEBUG, "[DebuggerHost]: {}", msg);
		return true;
	};

	if (!debugger_client_instance.init()) {
		logger->error(LOG_DEBUG, "Cannot start debugger client");
		emit DebuggerProcessError("Cannot start debug driver or debugger client: " + QString::fromStdString(debugger_client_instance.get_last_error()), -1);
		return;
	}

	debugger_client_instance.startSession();
}

ExperimentalDebugDriver::ExperimentalDebugDriver(DebugManager *dm)
{
	debug_manager = dm;
	logger = GixGlobals::getLogManager();
}

ExperimentalDebugDriver::~ExperimentalDebugDriver()
{
}

bool ExperimentalDebugDriver::stop()
{
	bool b1 = debugger_client_instance.stopSession();
	bool b2 = debugger_client_instance.cleanup();

	dbgr_client_debuggerProcessExit(nullptr, debugger_client_instance.get_exit_code(), debugger_client_instance.get_exit_message());
	
	return b1 && b2;
}

QString ExperimentalDebugDriver::getLastResponse()
{
	return last_response;
}

void ExperimentalDebugDriver::write(QString payload)
{
	cmd_line = payload;
	cmd_available.wakeAll();
}

void ExperimentalDebugDriver::writeToProcess(QString)
{
	// nothing
}

bool ExperimentalDebugDriver::isStarted()
{
	return is_started;
}

bool ExperimentalDebugDriver::getVariables(const std::vector<std::string>& var_names, std::map<std::string, struct VariableDisplayData>& var_data_req)
{
	return debugger_client_instance.getVariables(var_names, var_data_req);
}

QStringList ExperimentalDebugDriver::extract_args(QStringList l)
{
	QStringList res;
	if (l.size() <= 1) {
		return QStringList();
	}

	for (int i = 1; i < l.size(); i++) {
		res.append(l.at(i));
	}
	return res;
}

bool ExperimentalDebugDriver::is_response(QString s)
{
	return s.startsWith(RESP_OK) || s.startsWith(RESP_KO) || s.startsWith(RESP_OK_WITH_RESULT) || s.startsWith(RESP_KO_WITH_RESULT);
}

bool ExperimentalDebugDriver::processCommand(const QString& cmd_line)
{
	QStringList cmd = cmd_line.split(' ');

	if (cmd.size() == 0 || cmd.at(0).trimmed() == "")
		return false;

	if (cmd.at(0) == CMD_STEP) {
		debugger_client_instance.step();
		return true;
	}

	if (cmd.at(0) == CMD_CONTINUE) {
		debugger_client_instance.continue_running();
		return true;
	}

	return false;
}




bool ExperimentalDebugDriver::dbgr_client_getBreakpoints(GixDebugger* gd, std::vector<std::pair<std::string, int>>& bps)
{
	bps.clear();
	for (auto bkp : debug_manager->getTranslatedBreakpoints()) {
		QString qpath = bkp.mid(bkp.indexOf("@") + 1);
		QString src_file = QDir::toNativeSeparators(qpath);
		int ln = bkp.mid(0, bkp.indexOf("@")).toInt();

		bps.push_back(std::pair<std::string, int>(src_file.toStdString(), ln));
	}
	return true;
}


bool ExperimentalDebugDriver::dbgr_client_debuggerBreak(GixDebugger* gd, std::string module_name, std::string source_file, int line)
{
	emit this->DebuggerBreak(QString::fromStdString(module_name), QString::fromStdString(source_file), line);

	while (true) {
		cmd_line = "";

		cmd_mutex.lock();
		logger->trace(LOG_DEBUG, "DebugDriver is waiting for commands");
		cmd_available.wait(&cmd_mutex);
		cmd_mutex.unlock();

		logger->trace(LOG_DEBUG, "DebugDriver received: {}", cmd_line);
		bool stop_processing = processCommand(cmd_line);
		if (stop_processing) {
			logger->trace(LOG_DEBUG, "DebugDriver will stop processing commands");
			break;
		}
	}

	return true;
}

bool ExperimentalDebugDriver::dbgr_client_debuggerError(GixDebugger* gd, int err_code, std::string msg)
{
	QString qmsg = QString::fromStdString(msg);
	logger->error(LOG_DEBUG, "{}", qmsg);
	emit DebuggerProcessError(qmsg, err_code);
	return true;
}

bool ExperimentalDebugDriver::dbgr_client_debuggerProcessExit(GixDebugger*, int rc, std::string exe_path)
{
	QString qmsg = QString::fromStdString(exe_path);
	logger->debug(LOG_DEBUG, "Program {} terminated with result code: {}", qmsg, rc);
	emit DebuggerProcessFinished(qmsg, rc);
	return true;
}

bool ExperimentalDebugDriver::dbgr_client_debuggerProcessStarted(GixDebugger*, std::string exe_path)
{
	QString qmsg = QString::fromStdString(exe_path);
	logger->debug(LOG_DEBUG, "Program {} started", qmsg);
	emit DebuggerProcessStarted(qmsg);
	return true;
}

bool ExperimentalDebugDriver::dbgr_client_debuggerReady(GixDebugger*, std::string exe_path)
{
	QString qmsg = QString::fromStdString(exe_path);
	logger->debug(LOG_DEBUG, "Debugger has loaded symbols and is ready to run {}", qmsg);
	is_started = true;
	emit DebuggerReady(qmsg);
	return true;
}

bool ExperimentalDebugDriver::dbgr_client_debuggerStdOutAvailable(GixDebugger*, std::string data)
{
	QString qmsg = QString::fromStdString(data);
	emit DebuggerStdOutAvailable(qmsg);
	return true;
}

bool ExperimentalDebugDriver::dbgr_client_debuggerStdErrAvailable(GixDebugger*, std::string data)
{
	QString qmsg = QString::fromStdString(data);
	emit DebuggerStdErrAvailable(qmsg);
	return true;
}
