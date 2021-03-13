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

#include <QObject>
#include <QMap>
#include <QString>
#include <QVariant>
#include <QStringList>
#include <QMap>

#include "BuildResult.h"
#include "BuildTarget.h"
#include "LogOutputType.h"
#include "CompilerConfiguration.h"
#include "CopyResolver.h"
#include "QLogger.h"
#include "gixcommon_global.h"

class ProjectItem;
class CompilerConfiguration;

enum class BuildOperation
{
	Build,
	Clean,
	Rebuild
};

class GIXCOMMON_EXPORT BuildDriver : public QObject {

	Q_OBJECT

public:
	BuildDriver();
	
	void execute(BuildTarget *target, BuildOperation op);
	
	bool handle_single_target(BuildTarget *target, QString &provides_item, ProjectItem *pi);
	
	bool stopRequested();
	
	void addToBuiltTargetList(QString f, QString t);
	QList<QPair<QString, QString>> getBuiltTargetList();
	
	CopyResolver *getCopyResolver() const;
	
	void setBuildEnvironment(QMap<QString, QVariant>&);
	QMap<QString, QVariant>& getBuildEnvironment();
	
	void log_build_message(QString msg, QLogger::LogLevel, int status = 0);
	void log_build_clear();
	
	BuildResult getBuildResult();
	
	ProjectItem *getItem();

public slots:
	void stopBuild();

signals:
	void log_output(QString msg, QLogger::LogLevel);
	void log_clear();

protected:
	QMap<QString, QVariant> build_environment;
	BuildResult build_result;

	ProjectItem *item;

	bool stop_requested;

	CopyResolver copy_resolver;

	QList<QPair<QString, QString>> built_target_list;

private:
	void execute_clean(BuildTarget *target);
	void execute_build(BuildTarget *target);

	void extract_project_base_dirs(BuildTarget *target, QMap<QString, QString> &build_dirs);

};

