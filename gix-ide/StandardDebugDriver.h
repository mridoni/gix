/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#pragma once

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#endif

#include <string>
#include <vector>

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
#include "IGixLogManager.h"

class GixDebugger;
class DebugManager;

class DebugDriverRunParameters {

public:
	//QStringList Breakpoints;
};

class GixDebuggerInterfaceBlock
{

public:
	// Debugger events
	std::function<bool(GixDebugger*, QList<QPair<QString, int>>&)> getBreakpoints;
	std::function<bool(GixDebugger*, QString, QString, int)> debuggerBreak;
	std::function<bool(GixDebugger*, QString, int)> debuggerMessage;
	std::function<bool(GixDebugger*, int, QString)> debuggerError;
	std::function<bool(GixDebugger*, int, QString)> debuggerProcessExit;
	std::function<bool(GixDebugger*, QString)> debuggerProcessStarted;
	std::function<bool(GixDebugger*, QString)> debuggerReady;
	std::function<bool(GixDebugger*, QString)> debuggerStdOutAvailable;
	std::function<bool(GixDebugger*, QString)> debuggerStdErrAvailable;
};


class StandardDebugDriver : public IDebugDriver {

	Q_OBJECT

public:
	StandardDebugDriver(DebugManager *);
	~StandardDebugDriver();

	void setDebuggerInstance(GixDebugger * gd);
	GixDebugger *debuggerInstance();

	virtual bool stop() override;
	virtual QString getLastResponse() override;
	virtual void write(QString) override;
    virtual void writeToProcess(QString) override;
    virtual bool isStarted() override;
	virtual bool getVariables(const std::vector<std::string>& var_names, std::map<std::string, struct VariableDisplayData>& var_data_req) override;

	DebugDriverRunParameters RunParameters;

	static bool is_ok_response(QString);

public slots:
    virtual void startDriver() override;

protected:

private:

	IGixLogManager* logger = nullptr;

	virtual bool dbgr_client_getBreakpoints(GixDebugger*, std::vector<std::pair<std::string, int>>&) override;
	virtual bool dbgr_client_debuggerBreak(GixDebugger* gd, std::string module_name, std::string source_file, int line)  override;
	virtual bool dbgr_client_debuggerError(GixDebugger*, int, std::string)  override;
	virtual bool dbgr_client_debuggerProcessExit(GixDebugger*, int, std::string)  override;
	virtual bool dbgr_client_debuggerProcessStarted(GixDebugger*, std::string)  override;
	virtual bool dbgr_client_debuggerReady(GixDebugger*, std::string) override;
	virtual bool dbgr_client_debuggerStdOutAvailable(GixDebugger*, std::string) override;
	virtual bool dbgr_client_debuggerStdErrAvailable(GixDebugger*, std::string) override;

	GixDebuggerInterfaceBlock ib;

	QString current_module;

	QStringList response_queue;

	QStringList extract_args(QStringList);
	bool is_response(QString);

	bool processCommand(const QString &cmd_line);

	bool keep_waiting_host_cmds = false;
	bool is_running = false;

	QString last_response;

	DebugManager *debug_manager;

	QString cmd_line;
	QMutex cmd_mutex;
	QWaitCondition cmd_available;
};

