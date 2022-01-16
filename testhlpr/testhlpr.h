#pragma once

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

#include "testhlpr_global.h"

#include <functional>
#include <QTcpServer>
#include <QTcpSocket>

class MainWindow;
class ProjectCollection;
class ProjectItem;

struct TESTHLPR_EXPORT TestHelperInterface
{
	//TestHelperInterface() {};
	//~TestHelperInterface() {};

	std::function<void(QString msg)> logMessage;
	std::function<void()> refreshPropertyWindowContent;
	std::function<void()> setAvailablePlatformsForConfiguration;
	std::function <ProjectCollection *()> getCurrentProjectCollection;
	std::function <ProjectItem *()> getFirstProjectInCurrentProjectCollection;
	std::function<void(QString out, QString err)> duplicateConsole;
	std::function<void(QString out)> duplicateIdeOutput;
	std::function <QString()> getOutputWindowContent;
	std::function <QString()> getConsoleWindowContent;
	std::function <ProjectItem *()> getPropertyWindowItem;
	std::function <void(QStringList)> addBreakpoints;
	std::function <void(QStringList)> addWatchedVars;
	std::function <QMap<QString,QString>()> getWatchedVarsValues;
	std::function <void(int *, QString&, int*)> getDbgStatus;
};

class TestHelper : public QObject
{
	Q_OBJECT

public:
	TestHelper(TestHelperInterface *i);
	~TestHelper();

	bool start();

private:
	QTcpServer *server = nullptr;
	QTcpSocket *server_conn = nullptr;

	bool process(QString cmd);
	int writeMessage(QString cmd);

	TestHelperInterface *testhlpr_interface = nullptr;

};

extern "C" {
	TESTHLPR_EXPORT TestHelperInterface *test_helper_interface();
	TESTHLPR_EXPORT TestHelper *test_helper_init(TestHelperInterface *i);
}
