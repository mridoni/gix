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

/*
	This module includes support unctions for automated tests.
	Normally, it should NOT be included with a binary distribution
*/

#include "testhlpr.h"

#include "libcpputils.h"

#include "MainWindow.h"
#include "PropertyWindow.h"
#include "Ide.h"
#include "GixGlobals.h"

#include <iostream>
#include <string>
#include <vector>

static bool split_in_args(std::vector<std::string> &qargs, std::string command);

TestHelper::TestHelper(TestHelperInterface *i)
{
	testhlpr_interface = i;
}

TestHelper::~TestHelper()
{
	if (server)
		delete server;
}

bool TestHelper::start()
{
	server = new QTcpServer();
	connect(server, &QTcpServer::newConnection, this, [this]() {
		server_conn = server->nextPendingConnection();
		connect(server_conn, &QTcpSocket::readyRead, this, [this]() {
			QByteArray qba = server_conn->readLine();
			QString cmd = QString::fromUtf8(qba);
			std::cerr << cmd.toStdString() << std::endl;
			if (!process(cmd))
				;
		});
	});
	return server->listen(QHostAddress::LocalHost, 19797);
}

int TestHelper::writeMessage(QString msg)
{
	msg += "\n";
	QByteArray bfr = msg.toUtf8();
	uint32_t len = bfr.size();
	server_conn->write((const char *)&len, sizeof(uint32_t));
	QString mmsg = msg.replace("\r", "").replace("\n", "");
	testhlpr_interface->logMessage(QString("Writing %1 bytes (message: [%2]").arg(len).arg(mmsg));
	server_conn->write(bfr);
	return len;
}

bool TestHelper::process(QString cmd)
{
	std::vector<std::string> items;
	split_in_args(items, cmd.toStdString());
	QString ccmd = QString::fromStdString(items.at(0)).replace("\r", "").replace("\n", "");

	testhlpr_interface->logMessage(QString("Received message: [%1]").arg(cmd.replace("\r", "").replace("\n", "")));

	if (ccmd == "hello") {
		writeMessage("OK");
	}

	if (ccmd == "setprop") {
		QString prop_name = QString::fromStdString(items.at(1)).replace("\r", "").replace("\n", "");
		QString sprop_val = QString::fromStdString(items.at(2)).replace("\r", "").replace("\n", "");

		PropertySource *ps = dynamic_cast<PropertySource *>(testhlpr_interface->getPropertyWindowItem());

		if (sprop_val == "true" || sprop_val == "false") {
			ps->PropertySetValue(prop_name, (bool)(sprop_val == "true"));
		}
		else {
			ps->PropertySetValue(prop_name, sprop_val);
		}
		testhlpr_interface->refreshPropertyWindowContent();
		writeMessage("OK");
	}

	if (ccmd == "setprjprop") {
		testhlpr_interface->logMessage("Received setprjprop");

		QString prop_name = QString::fromStdString(items.at(1)).replace("\r", "").replace("\n", "");
		QString sprop_val = QString::fromStdString(items.at(2)).replace("\r", "").replace("\n", "");

		PropertySource *ps = dynamic_cast<PropertySource *>(testhlpr_interface->getFirstProjectInCurrentProjectCollection());

		if (!prop_name.startsWith("[]")) {
			if (sprop_val == "true" || sprop_val == "false") {
				ps->PropertySetValue(prop_name, (bool)(sprop_val == "true"));
			}
			else {
				ps->PropertySetValue(prop_name, sprop_val);
			}
		}
		else {
			prop_name = prop_name.mid(2);
			QStringList l = sprop_val.split("|");
			ps->PropertySetValue(prop_name, l);
		}

		testhlpr_interface->refreshPropertyWindowContent();

		testhlpr_interface->logMessage(QString("Set project property: [%1] = [%2]").arg(prop_name).arg(sprop_val));

		writeMessage("OK");
	}

	if (ccmd == "dupconsole") {
		QString file_out = QString::fromStdString(items.at(1)).replace("\r", "").replace("\n", "");
		QString file_err = QString::fromStdString(items.at(2)).replace("\r", "").replace("\n", "");
		testhlpr_interface->duplicateConsole(file_out, file_err);

		testhlpr_interface->logMessage(QString("Duplicating stdout to [%1]").arg(file_out));
		testhlpr_interface->logMessage(QString("Duplicating stderr to [%1]").arg(file_err));

		writeMessage("OK");
	}

	if (ccmd == "dupideout") {
		QString file_out = QString::fromStdString(items.at(1)).replace("\r", "").replace("\n", "");
		testhlpr_interface->duplicateIdeOutput(file_out);
		testhlpr_interface->logMessage(QString("Duplicating IDE output to [%1]").arg(file_out));

		writeMessage("OK");
	}

	if (ccmd == "getoutlog") {
		QString text = testhlpr_interface->getOutputWindowContent();
		testhlpr_interface->logMessage(QString("Sending output window content (%1 bytes").arg(text.size()));
		writeMessage(text);
	}

	if (ccmd == "setcfgplatforms") {
		testhlpr_interface->setAvailablePlatformsForConfiguration();
		testhlpr_interface->logMessage(QString("Setting available platforms for current configuration"));

		writeMessage("OK");
	}

	if (ccmd == "addbrkps") {
		QString s = QString::fromStdString(items.at(1)).replace("\r", "").replace("\n", "");
		if (!s.trimmed().isEmpty()) {
			QStringList brkps = s.split("||");
			testhlpr_interface->addBreakpoints(brkps);
			testhlpr_interface->logMessage(QString("Adding %1 breakpoints").arg(brkps.size()));
		}
		writeMessage("OK");
	}

	if (ccmd == "addwvars") {
		QString s = QString::fromStdString(items.at(1)).replace("\r", "").replace("\n", "");
		if (!s.trimmed().isEmpty()) {
			QStringList wvars = s.split("||");
			testhlpr_interface->addWatchedVars(wvars);
			testhlpr_interface->logMessage(QString("Adding %1 watched vars").arg(wvars.size()));
		}
		writeMessage("OK");
	}

	if (ccmd == "getwvars") {
		QMap<QString,QString> wvars = testhlpr_interface->getWatchedVarsValues();
		QStringList l;
		for (auto it = wvars.begin(); it != wvars.end(); ++it) {
			l.append(it.key() + "=" + it.value());
		}
		QString s = l.join("||");
		testhlpr_interface->logMessage(QString("Sending %1 watched vars").arg(l.size()));
		writeMessage(s);
	}

	if (ccmd == "getdbgstatus") {
		int st = 0;
		QString dbg_src_file;
		int dbg_ln = 0;
		testhlpr_interface->getDbgStatus(&st, dbg_src_file, &dbg_ln);
		QString s = QString("%1||%2@%3").arg(st).arg(dbg_ln).arg(dbg_src_file);
		writeMessage(s);
	}
	return true;
}

TestHelperInterface *test_helper_interface()
{
	return new TestHelperInterface();
}

TestHelper *test_helper_init(TestHelperInterface *i)
{
	if (!i)
		return nullptr;

	TestHelper *th = new TestHelper(i);
	if (!th->start()) {
		delete th;
		delete i;
		th = nullptr;
	}
	return th;
}

static bool split_in_args(std::vector<std::string> &qargs, std::string command)
{
	int len = command.length();
	bool qot = false, sqot = false;
	int arglen;
	for (int i = 0; i < len; i++) {
		int start = i;
		if (command[i] == '\"') {
			qot = true;
		}
		else if (command[i] == '\'') sqot = true;

		if (qot) {
			i++;
			start++;
			while (i < len && command[i] != '\"')
				i++;
			if (i < len)
				qot = false;
			arglen = i - start;
			i++;
		}
		else if (sqot) {
			i++;
			while (i < len && command[i] != '\'')
				i++;
			if (i < len)
				sqot = false;
			arglen = i - start;
			i++;
		}
		else {
			while (i < len && command[i] != ' ')
				i++;
			arglen = i - start;
		}
		qargs.push_back(command.substr(start, arglen));
	}
	for (int i = 0; i < qargs.size(); i++) {
		std::cout << qargs[i] << std::endl;
	}
	std::cout << qargs.size();
	return (qot == sqot);
}
