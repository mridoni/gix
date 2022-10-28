#pragma once

#include <QObject>
#include <QString>
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

class GixDebugger;
class GixDebuggerSessionConfig;

class IDebugDriver : public QObject {

	Q_OBJECT

public:
	// Messages/Requests from debugged program to client debugger (VS, etc.)
	const static QString CMD_DEBUGGER_STARTING;
	const static QString CMD_GET_BREAKPOINTS;
	const static QString CMD_DBGRBREAK;
	const static QString CMD_DBGR_MOD_CHANGED;
	const static QString CMD_DBGR_MOD_EXIT;
	const static QString CMD_DBGR_PRG_EXIT;

	// Messages/Requests from client debugger (VS, etc.)  to debugged program
	const static QString CMD_GET_CURPOS;
	const static QString CMD_GET_VAR;
	const static QString CMD_GET_VARS;
	const static QString CMD_STEP;
	const static QString CMD_STEP_INTO;
	const static QString CMD_CONTINUE;
	const static QString CMD_ABORT;

	// Standard responses
	const static QString RESP_OK;
	const static QString RESP_OK_WITH_RESULT;

	const static QString RESP_KO;
	const static QString RESP_KO_WITH_RESULT;

public:

	void setSessionConfiguration(GixDebuggerSessionConfig* cfg);

	virtual bool stop() = 0;
	virtual QString getLastResponse() = 0;
	virtual void write(QString) = 0;
	virtual void writeToProcess(QString) = 0;
	virtual bool isStarted() = 0;
	virtual bool getVariables(const std::vector<std::string>& var_names, std::map<std::string, struct VariableDisplayData>& var_data_req) = 0;

	//DebugDriverRunParameters RunParameters;

	// Debugger client interface (debugger messages are handled by a specialized spdlog sink)
	virtual bool dbgr_client_getBreakpoints(GixDebugger*, std::vector<std::pair<std::string, int>>&) = 0;
	virtual bool dbgr_client_debuggerBreak(GixDebugger* gd, std::string module_name, std::string source_file, int line) = 0;
	virtual bool dbgr_client_debuggerError(GixDebugger*, int, std::string) = 0;
	virtual bool dbgr_client_debuggerProcessExit(GixDebugger*, int, std::string) = 0;
	virtual bool dbgr_client_debuggerProcessStarted(GixDebugger*, std::string) = 0;
	virtual bool dbgr_client_debuggerReady(GixDebugger*, std::string) = 0;
	virtual bool dbgr_client_debuggerStdOutAvailable(GixDebugger*, std::string) = 0;
	virtual bool dbgr_client_debuggerStdErrAvailable(GixDebugger*, std::string) = 0;
	virtual void acquireWaitLock();
	virtual void releaseWaitLock();

public slots:
	virtual void startDriver() = 0;

protected:

	GixDebugger* debugger_instance = nullptr;
	GixDebuggerSessionConfig* debug_session_config = nullptr;
	std::condition_variable cv_on_break;
	std::mutex cv_m;
	bool is_started = false;

signals:
	void DebuggerReady(QString);
	void DebuggerBreak(QString, QString, int);
	void DebuggerModuleChanged(QString, int);
	void DebuggerModuleExit(QString, int);
	void DebuggerProcessStarted(QString);
	void DebuggerProcessFinished(QString, int);
	void DebuggerProcessError(QString, int);
	void DebuggerStdOutAvailable(QString);
	void DebuggerStdErrAvailable(QString);

private:

	QString current_module;
	QStringList response_queue;
	QString last_response;
	QString cmd_line;

	bool keep_waiting_host_cmds = false;
};
