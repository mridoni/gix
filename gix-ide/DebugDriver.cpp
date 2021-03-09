/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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

#include "DebugDriver.h"
#include "DebugManager.h"
#include "MetadataManager.h"
#include "Changeling.h"
#include "Ide.h"
#include "DebugDriverUtils.h"
#include "GixDebugger.h"

#include <QThread>
#include <QHostAddress>
#include <QEventLoop>
#include <QDir>
#include <QPair>

const QString DebugDriver::CMD_DEBUGGER_STARTING = "DBGRSTARTING";
const QString DebugDriver::CMD_GET_BREAKPOINTS = "GETBRKPS";
const QString DebugDriver::CMD_DBGRBREAK = "DBGRBREAK";
const QString DebugDriver::CMD_DBGR_MOD_CHANGED = "DBGRMODCHGD";
const QString DebugDriver::CMD_DBGR_MOD_EXIT = "DBGRMODEXIT";
const QString DebugDriver::CMD_DBGR_PRG_EXIT = "DBGRPRGEXIT";
const QString DebugDriver::CMD_GET_CURPOS = "GETCURPOS";
const QString DebugDriver::CMD_GET_VAR = "GETVAR:";
const QString DebugDriver::CMD_GET_VARS = "GETVARS:";
const QString DebugDriver::CMD_STEP = "STEP";
const QString DebugDriver::CMD_STEP_INTO = "STEPINTO";
const QString DebugDriver::CMD_CONTINUE = "CONTINUE";
const QString DebugDriver::CMD_ABORT = "ABORT";
const QString DebugDriver::RESP_OK = "OK";
const QString DebugDriver::RESP_OK_WITH_RESULT = "OK:";
const QString DebugDriver::RESP_KO = "KO";
const QString DebugDriver::RESP_KO_WITH_RESULT = "KO:";

DebugDriver::DebugDriver(DebugManager* mgr)
{
	debug_manager = mgr;
    dbg_status_init();

}


DebugDriver::~DebugDriver()
{
    dbg_status_cleanup();
	if (debugger_instance)
		delete debugger_instance;
}

void DebugDriver::setDebuggerInstance(GixDebugger *gd)
{
	debugger_instance = gd;
}

GixDebugger *DebugDriver::debuggerInstance()
{
	return debugger_instance;
}

void DebugDriver::run()
{
	GixDebuggerInterfaceBlock ib;

	//local_server = new QLocalServer();

	//connect(local_server, &QLocalServer::newConnection, this, [this] { 
	//	local_socket = this->local_server->nextPendingConnection(); 
	//});




	ib.debuggerBreak = [this](GixDebugger *gd, QString module_name, QString src_file, int line) {

		emit DebuggerBreak(module_name, src_file, line);

		while (true) {
			cmd_line = "";

			cmd_mutex.lock();
			Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, ">> DebugDriver is waiting for commands", QLogger::LogLevel::Trace);
			cmd_available.wait(&cmd_mutex);
			cmd_mutex.unlock();

			Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, ">> DebugDriver received: " + cmd_line, QLogger::LogLevel::Trace);
			bool stop_processing = processCommand(cmd_line);
			if (stop_processing) {
				Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, ">> DebugDriver will stop processing commands", QLogger::LogLevel::Trace);
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

	ib.resolveVariable = [](GixDebugger *gd, const QString &module_name, const QString &var_name, VariableResolverData *res) {

		CobolModuleMetadata *cmm = GixGlobals::getMetadataManager()->getModuleMetadata(module_name);
		if (!cmm)
			return false;

		// TODO: fix hierarchy and search
		DataEntry *e = cmm->findDefinition(var_name, false);
		if (!e)
			return false;

		QString topmost_parent_name = e->getTopMostParent()->name;
		QString sym_name = cmm->getDebugLocalSymbolName(topmost_parent_name);
		if (sym_name.isEmpty())
			return false;

		res->storage_len = e->storage_size;
		res->local_sym_name = sym_name;
		res->local_addr = e->offset_local;
		res->module_name = module_name;
		res->var_name = var_name;

		return true;
	};

	debugger_instance->setInterfaceBlock(&ib);

	//local_server->listen("gix-debugger");

	emit DebuggerReady("gix-debugger");

	int rc = debugger_instance->start();

	//QEventLoop loop;
	//loop.exec();
	
}

bool DebugDriver::stop()
{
	bool b = debugger_instance->stop();
	return b;
}

bool DebugDriver::isRunning()
{
    return is_running;
}

QStringList DebugDriver::extract_args(QStringList l)
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

bool DebugDriver::is_response(QString s)
{
	return s.startsWith(RESP_OK) || s.startsWith(RESP_KO) || s.startsWith(RESP_OK_WITH_RESULT) || s.startsWith(RESP_KO_WITH_RESULT);
}

bool DebugDriver::processCommand(const QString &cmd_line)
{
	QStringList cmd = cmd_line.split(' ');
	if (cmd.size() == 0 || cmd.at(0).trimmed() == "")
		return false;

	if (cmd.at(0) == CMD_STEP) {
		debugger_instance->step();
		return true;
	}

	if (cmd.at(0) == CMD_CONTINUE) {
		debugger_instance->continue_running();
		return true;
	}

	return false;
}

QString DebugDriver::getLastResponse()
{
	return last_response;
}

void DebugDriver::write(QString payload)
{
	cmd_line = payload;
	cmd_available.wakeAll();
}

bool DebugDriver::is_ok_response(QString s)
{
	return s == "OK" || s.startsWith("OK:");
}
//
//QString DebugDriver::_raw_receive_data(bool* brk)
//{
//	*brk = false;
//
//	int to_be_read = get_network_msg_len();
//	if (to_be_read == SOCKET_ERROR) {
//		*brk = true;
//		return QString();
//	}
//
//	char* buffer = (char*)calloc(to_be_read + 1, 1);
//	char* cur_bfr_ptr = buffer;
//
//	while (true)
//	{
//		int max_cur_bfr_len = ((buffer + to_be_read) - cur_bfr_ptr);
//		int n = recv(client, cur_bfr_ptr, max_cur_bfr_len, 0);
//		if (n == SOCKET_ERROR) {
//			*brk = true;
//			return QString();
//		}
//
//		if (n == 0)
//			break;
//
//		cur_bfr_ptr += n;
//		int nrcvd = (cur_bfr_ptr - buffer);
//		if (nrcvd == to_be_read)
//			break;
//	}
//
//	buffer[to_be_read] = '\0';
//	QString res(buffer);
//	free(buffer);
//
//	//emit DebuggerCommandReceived(res);
//	return res;
//}
