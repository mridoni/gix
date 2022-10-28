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

#include "StandardDebugDriver.h"
#include "DebugManager.h"
#include "MetadataManager.h"
#include "Changeling.h"
#include "Ide.h"
#include "GixDebugger.h"
#include "DebugDriverUtils.h"
#include "DataEntry.h"
#include "gix-debugger-types.h"

#include <QThread>
#include <QHostAddress>
#include <QEventLoop>
#include <QDir>
#include <QPair>

StandardDebugDriver::StandardDebugDriver(DebugManager* mgr)
{
	debug_manager = mgr;
    dbg_status_init();
}


StandardDebugDriver::~StandardDebugDriver()
{
    dbg_status_cleanup();
	if (debugger_instance)
		delete debugger_instance;
}

void StandardDebugDriver::setDebuggerInstance(GixDebugger *gd)
{
	debugger_instance = gd;
}

GixDebugger *StandardDebugDriver::debuggerInstance()
{
	return debugger_instance;
}

void StandardDebugDriver::startDriver()
{

	ib.debuggerBreak = [this](GixDebugger *gd, QString module_name, QString src_file, int line) {

        fprintf(stderr, "StandardDebugDriver is emitting DebuggerBreak\n");
        emit this->DebuggerBreak(module_name, src_file, line);

        while (true) {
            cmd_line = "";

            cmd_mutex.lock();
            Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, ">> StandardDebugDriver is waiting for commands", QLogger::LogLevel::Trace);
            cmd_available.wait(&cmd_mutex);
            cmd_mutex.unlock();

            Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, ">> StandardDebugDriver received: " + cmd_line, QLogger::LogLevel::Trace);
            bool stop_processing = processCommand(cmd_line);
            if (stop_processing) {
                Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, ">> StandardDebugDriver will stop processing commands", QLogger::LogLevel::Trace);
                break;
            }
        }

		return true;
	};

	ib.getBreakpoints = [this](GixDebugger *gd, QList<QPair<QString, int>> &bps) {

		bps.clear();
		for (auto bkp : debug_manager->getTranslatedBreakpoints()) {
			QString src_file = QDir::toNativeSeparators(bkp.mid(bkp.indexOf("@") + 1));
			int ln = bkp.mid(0, bkp.indexOf("@")).toInt();

			bps.append(QPair<QString, int>(src_file, ln));
		}
		return true;
	};

	ib.debuggerMessage = [](GixDebugger *gd, QString msg, int l) {
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, msg, (QLogger::LogLevel) l);
		return true;
	};

	ib.debuggerError = [this](GixDebugger *gd, int err_code, QString msg) {
		QString m1 = msg;
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, msg, (QLogger::LogLevel) 0);
        emit DebuggerProcessError(m1, err_code);
		return true;
	};

	ib.debuggerProcessStarted = [this](GixDebugger *gd, QString msg) {
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("Program %1 started").arg(msg), (QLogger::LogLevel) 0);
		emit DebuggerProcessStarted(msg);
		return true;
	};

	ib.debuggerReady = [this](GixDebugger *gd, QString msg) {
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("Debugger has loaded symbols and is ready to run %1").arg(msg), (QLogger::LogLevel) 0);
		emit DebuggerReady(msg);
		return true;
	};

	ib.debuggerProcessExit = [this](GixDebugger *gd, int rc, QString msg) {
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("Program %1 terminated with result code: %2").arg(msg).arg(rc), (QLogger::LogLevel) 0);
		emit DebuggerProcessFinished(msg, rc);
		return true;
	};

	ib.debuggerStdOutAvailable = [this](GixDebugger *gd, QString msg) {
        emit DebuggerStdOutAvailable(msg);
		return true;
	};

	ib.debuggerStdErrAvailable = [this](GixDebugger *gd, QString msg) {
        emit DebuggerStdErrAvailable(msg);
		return true;
	};

	GixDebugger* gd = GixDebugger::get();
	gd->setDriver(this);
	gd->setConfiguration(*debug_session_config);
	this->debugger_instance = gd;

	int rc = debugger_instance->start();
	
}

bool StandardDebugDriver::stop()
{
	bool b = debugger_instance->stop();
	return b;
}

bool StandardDebugDriver::isStarted()
{
    return is_running;
}

bool StandardDebugDriver::getVariables(const std::vector<std::string>& var_names, std::map<std::string, struct VariableDisplayData>& var_data_req)
{
	return debugger_instance->getVariables(var_names, var_data_req);
}

QStringList StandardDebugDriver::extract_args(QStringList l)
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

bool StandardDebugDriver::is_response(QString s)
{
	return s.startsWith(RESP_OK) || s.startsWith(RESP_KO) || s.startsWith(RESP_OK_WITH_RESULT) || s.startsWith(RESP_KO_WITH_RESULT);
}

bool StandardDebugDriver::processCommand(const QString &cmd_line)
{
	QStringList cmd = cmd_line.split(' ');

    fprintf(stderr, ">>> StandardDebugDriver received \"%s\"\n", cmd_line.toLocal8Bit().data());

	if (cmd.size() == 0 || cmd.at(0).trimmed() == "")
		return false;

	if (cmd.at(0) == IDebugDriver::CMD_STEP) {
		debugger_instance->step();
		return true;
	}

	if (cmd.at(0) == IDebugDriver::CMD_CONTINUE) {
		debugger_instance->continue_running();
		return true;
	}

	return false;
}

QString StandardDebugDriver::getLastResponse()
{
	return last_response;
}

void StandardDebugDriver::write(QString payload)
{
	cmd_line = payload;
    cmd_available.wakeAll();
}

void StandardDebugDriver::writeToProcess(QString s)
{
    debugger_instance->writeToProcess(s.toStdString());
}

bool StandardDebugDriver::is_ok_response(QString s)
{
	return s == "OK" || s.startsWith("OK:");
}


// Debugger client interface

bool StandardDebugDriver::dbgr_client_getBreakpoints(GixDebugger* gd, std::vector<std::pair<std::string, int>>& user_breakpoints)
{
	QList<QPair<QString, int>> v;
	bool b = ib.getBreakpoints(gd, v);

	for (auto e : v) {
		user_breakpoints.push_back(std::pair<std::string, int>(e.first.toStdString(), e.second));
	}
	return b;
}

bool StandardDebugDriver::dbgr_client_debuggerBreak(GixDebugger* gd, std::string module_name, std::string source_file, int line)
{
	return ib.debuggerBreak(gd, QString::fromStdString(module_name), QString::fromStdString(source_file), line);
}

bool StandardDebugDriver::dbgr_client_debuggerError(GixDebugger* gd, int err_code, std::string msg)
{
	return ib.debuggerError(gd, err_code, QString::fromStdString(msg));
}

bool StandardDebugDriver::dbgr_client_debuggerProcessExit(GixDebugger* gd, int exit_code, std::string exe_path)
{
	return ib.debuggerProcessExit(gd, exit_code, QString::fromStdString(exe_path));
}

bool StandardDebugDriver::dbgr_client_debuggerProcessStarted(GixDebugger* gd, std::string exe_path)
{
	return ib.debuggerProcessStarted(gd, QString::fromStdString(exe_path));
}

bool StandardDebugDriver::dbgr_client_debuggerReady(GixDebugger* gd, std::string exe_path)
{
	return ib.debuggerReady(gd, QString::fromStdString(exe_path));
}

bool StandardDebugDriver::dbgr_client_debuggerStdOutAvailable(GixDebugger* gd, std::string data)
{
	return ib.debuggerStdOutAvailable(gd, QString::fromStdString(data));
}

bool StandardDebugDriver::dbgr_client_debuggerStdErrAvailable(GixDebugger* gd, std::string data)
{
	return ib.debuggerStdErrAvailable(gd, QString::fromStdString(data));
}
