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

#include <QObject>
#include <QStringList>
#include <QMap>
#include <QProcess>
#include <QThread>
//#include <QLocalServer>
//#include <QLocalSocket>
#include <QMutex>
#include <QWaitCondition>

#if defined(__MINGW32__)
typedef unsigned char byte;
#endif

#include <QProcessEnvironment>
#include <functional>

class GixDebugger;
class DebugManager;

class DebugDriverRunParameters {

public:
	//QStringList Breakpoints;
};

class DebugDriver : public QObject {

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
	DebugDriver(DebugManager *);
	~DebugDriver();

	void setDebuggerInstance(GixDebugger * gd);
	GixDebugger *debuggerInstance();

	bool stop();
	QString getLastResponse();
	void write(QString);
    void writeToProcess(QString);
    bool isRunning();

	DebugDriverRunParameters RunParameters;

	static bool is_ok_response(QString);

public slots:
    void startDriver();

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

	GixDebugger *debugger_instance = nullptr;

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

