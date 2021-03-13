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

#include "DebugDriver.h"

#include <QString>
#include <QStringList>
#include <QProcess>
#include <QProcessEnvironment>

#include "Project.h"
#include "IdeTaskManager.h"

class IdeTaskManager;

class DebugManager : public QObject
{
	Q_OBJECT

public:
	DebugManager(IdeTaskManager *);
	~DebugManager();

	void setDebuggingEnabled(bool);

	bool start(Project *prj, QString configuration, QString platform);
	//bool startHttpService(Project* prj, QString build_configuration, QString target_platform);
	void step();
	void stop();
	void continue_running();
	QString getCurrentSourceFile();
	QString getCurrentCobolModuleName();
	int getCurrentLine();
	QString getPrintableVarContent(QString n);
	QMap<QString, QString> getPrintableVarListContent(QStringList vlist);
	int getWatchedVarCount();
	QString getWatchedVarName(int);
	void addWatchedVar(QString);
	void removeWatchedVar(QString);
	QStringList getWatchedVarList();
	QStringList getTranslatedBreakpoints();
	void setUserInititatedStop(bool b);

signals:
	void debugStopped();
	void debugStarted();
	void debugProgramExit();
	void debugError();

private slots:
	void debuggedProcessFinished(int rc, QString s);
	void debuggedProcessError(int errcode, QString errmsg);
	void debuggedProcessStarted();

private:
	bool is_debugging_enabled;
	bool is_user_initiated_stop;

	DebugDriver *debug_driver;
	IdeTaskManager *ide_task_manager;

	void readStdErr();
	void readStdOut();

    void debug_break(QString module_name, QString m, int l);
	void debug_module_changed(QString m, int l);
	void debug_program_exit(QString m, int l);

	bool parsePosition(QString pos, QString& module, int *line);
	void loadDebugManagerState();
	void saveDebugManagerState();

	QString tmp_cfg_path;

	QStringList parseArguments(QString);

	QString cur_src_file;
	QString cur_module;
	int cur_line;

	QStringList watched_vars;

	Project *debugged_prj;

	QMap<QString, ModuleDebugInfo *> modules;

	QString driverWrite(const QString& msg);

	QStringList translateBreakpoints(CobolModuleMetadata *cmm, const QStringList &orig_bkps);
	bool translateBreakpointReverse(CobolModuleMetadata *cmm, const QString &running_file, int running_ln, QString &orig_file, int *orig_ln);

};

