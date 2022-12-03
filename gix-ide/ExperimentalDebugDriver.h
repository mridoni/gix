#pragma once

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#endif

#include <string>

#include <QObject>
#include <QStringList>
#include <QMap>
#include <QProcess>
#include <QThread>
#include <QMutex>
#include <QWaitCondition>

#if defined(__MINGW32__)
typedef unsigned char byte;
#endif

#include <QProcessEnvironment>
#include <functional>

#include "IDebugDriver.h"
#include "GixDebuggerClient.h"
#include "IGixLogManager.h"

class GixDebugger;
class DebugManager;
class NetworkManager;

class ExperimentalDebugDriver : public IDebugDriver {

	Q_OBJECT

public:
	ExperimentalDebugDriver(DebugManager*);
	~ExperimentalDebugDriver();

	virtual bool stop() override;
	virtual QString getLastResponse() override;
	virtual void write(QString) override;
	virtual void writeToProcess(QString) override;
	virtual bool isStarted() override;
	virtual bool getVariables(const std::vector<std::string>& var_names, std::map<std::string, struct VariableDisplayData>& var_data_req) override;


public slots:
	virtual void startDriver() override;

protected:

	// Debugger client interface (debugger messages are handled by a specialized spdlog sink)
	virtual bool dbgr_client_getBreakpoints(GixDebugger*, std::vector<std::pair<std::string, int>>&) override;
	virtual bool dbgr_client_debuggerBreak(GixDebugger* gd, std::string module_name, std::string source_file, int line) override;
	virtual bool dbgr_client_debuggerError(GixDebugger*, int, std::string) override;
	virtual bool dbgr_client_debuggerProcessExit(GixDebugger*, int, std::string) override;
	virtual bool dbgr_client_debuggerProcessStarted(GixDebugger*, std::string) override;
	virtual bool dbgr_client_debuggerReady(GixDebugger*, std::string) override;
	virtual bool dbgr_client_debuggerStdOutAvailable(GixDebugger*, std::string) override;
	virtual bool dbgr_client_debuggerStdErrAvailable(GixDebugger*, std::string) override;

private:
	IGixLogManager* logger = nullptr;

	GixDebugger* debugger_instance = nullptr;

	QString current_module;

	QStringList response_queue;

	QStringList extract_args(QStringList);
	bool is_response(QString);

	bool processCommand(const QString& cmd_line);

	bool keep_waiting_host_cmds = false;
	bool is_running = false;

	QString last_response;

	DebugManager* debug_manager = nullptr;

	QString cmd_line;
	QMutex cmd_mutex;
	QWaitCondition cmd_available;

	std::shared_ptr<NetworkManager> network_manager;
	GixDebuggerClient debugger_client_instance;

};
