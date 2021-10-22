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

#include "BuildDriver.h"
#include "MacroManager.h"
#include "ProjectItem.h"
#include "PropertySource.h"
#include "PathUtils.h"
#include "SysUtils.h"
#include "BuildTarget.h"
#include "IBuildableItem.h"
#include "BuildActionHandler.h"
#include "Project.h"

#include <QSettings>
#include <QList>

BuildDriver::BuildDriver()
{

}

void BuildDriver::setBuildEnvironment(QMap<QString, QVariant> &env)
{
	build_environment = env;
	build_result.status = -1;
	item = nullptr;
	stop_requested = false;
}

QMap<QString, QVariant> &BuildDriver::getBuildEnvironment()
{
	return build_environment;
}

void BuildDriver::log_build_message(QString msg, QLogger::LogLevel log_output_type, int status)
{
	QSettings settings;

	build_result.build_log.append(msg);
	emit log_output(msg, log_output_type);
	build_result.status = status;

}

void BuildDriver::log_build_clear()
{
	build_result.build_log.clear();
	emit log_clear();
}

BuildResult BuildDriver::getBuildResult()
{
	return build_result;
}

ProjectItem *BuildDriver::getItem()
{
	return item;
}


void BuildDriver::execute(BuildTarget *target, BuildOperation op)
{
	if (target == nullptr) {
		log_build_message(tr("Invalid target"), QLogger::LogLevel::Error, 1);
		return;
	}

	switch (op) {
		case BuildOperation::Build:
			execute_build(target);
			break;

		case BuildOperation::Rebuild:
			execute_clean(target);
			if (!build_result.status)
				execute_build(target);
			break;

		case BuildOperation::Clean:
			execute_clean(target);
			break;
	}
}

void BuildDriver::execute_clean(BuildTarget *target)
{
	QString msg;

	build_result.status = 1;

	if (!build_environment.contains("configuration") || !build_environment.contains("platform") || !build_environment.contains("prj.build_dir")) {
		msg = QString(tr("Invalid build environment for target %1")).arg(target->filename());
		log_build_message(msg, QLogger::LogLevel::Error);
		return;
	}

	MacroManager mm(build_environment);
	log_build_clear();

	QMap<QString, QString> base_dirs;
	extract_project_base_dirs(target, base_dirs);

	QString build_dir_tpl = build_environment["prj.build_dir"].toString();

	for (QMap<QString, QString>::iterator it = base_dirs.begin(); it != base_dirs.end(); ++it) {
		QString prj_name = it.key();
		QString base_dir = it.value();
		mm.add("prj.basedir", base_dir);
		QString build_dir = mm.translate(build_dir_tpl);
		if (build_dir != base_dir && build_dir.startsWith(base_dir)) {
			QDir dir(build_dir);
			if (dir.exists()) {
				if (!dir.removeRecursively()) {
					msg = QString(tr("Cannot remove directory %1")).arg(build_dir);
					log_build_message(msg, QLogger::LogLevel::Error);
				}
			}
			msg = QString(tr("Successfully cleaned project %1 (removed %2)")).arg(prj_name).arg(build_dir);
			log_build_message(msg, QLogger::LogLevel::Trace, 1);
		}
	}

	build_result.status = 0;
}

void BuildDriver::execute_build(BuildTarget *target)
{
	ProjectItem *pi = dynamic_cast<ProjectItem *>(target->getItem());

	if (!this->getBuildEnvironment().contains("configuration") || !this->getBuildEnvironment().contains("platform")) {
		log_build_message(tr("Invalid target"), QLogger::LogLevel::Error, 1);
		return;
	}

	QString target_type = this->getBuildEnvironment()["configuration"].toString() + "/" + this->getBuildEnvironment()["platform"].toString();

	log_build_clear();
	log_build_message(QString(tr("Starting to build %1 for target %2")).arg(pi->GetDisplayName()).arg(target_type), QLogger::LogLevel::Info);

	for (QString provides_item : target->provides()) {

		if (target->isVirtual() || !target->isUpToDate()) {
			if (!handle_single_target(target, provides_item, pi))
				build_result.status = 1;
			return;
		}
	}

}

void BuildDriver::extract_project_base_dirs(BuildTarget *target, QMap<QString, QString> &base_dirs)
{
	ProjectItem *pi = dynamic_cast<ProjectItem *>(target->getItem());
	if (pi->GetItemType() == ProjectItemType::TProject)
		base_dirs.insert(pi->GetDisplayName(), pi->GetBaseDir());

	for (BuildTarget *bt : *target->dependencies())
		extract_project_base_dirs(bt, base_dirs);
}

bool BuildDriver::handle_single_target(BuildTarget *target, QString &provides_item, ProjectItem *pi)
{
	if (pi->GetItemType() == ProjectItemType::TProject) {
		PropertySource *ps = dynamic_cast<PropertySource *>(pi);
		Project *prj = dynamic_cast<Project *>(pi);
		
		QStringList copy_dirs = ps->PropertyGetValue("copy_include_path").toStringList();
		if (prj->isEsql() && !qgetenv("GIXSQL_DATA_DIR").isEmpty()) {
			copy_dirs.append(QString::fromLocal8Bit(qgetenv("GIXSQL_DATA_DIR")));
		}
		this->copy_resolver.setCopyDirs(SysUtils::to_std_string_vector(copy_dirs));

		this->copy_resolver.setExtensions(SysUtils::to_std_string_vector(prj->getCopyExtList()));
		this->copy_resolver.setBaseDir(prj->GetBaseDir().toStdString());
	}

	QList<BuildTarget *> *deps = target->dependencies();
	for (auto dep : *deps) {
		if (dep->isVirtual() || !dep->isUpToDate()) {
			ProjectItem *dpi = dynamic_cast<ProjectItem *>(dep->getItem());
			if (!handle_single_target(dep, provides_item, dpi) && !dep->isOptional()) {
				log_build_message(QString(tr("An error occurred while handling target %1").arg(dep->filename())), QLogger::LogLevel::Error);
				build_result.status = 1;
				return false;
			}
		}
	}

	QScopedPointer<BuildActionHandler> bah(BuildActionHandler::get(target));
	if (!bah) {
		log_build_message(QString(tr("Cannot instantiate builder for %1").arg(pi->GetFilename())), QLogger::LogLevel::Error);
		build_result.status = 1;
		return false;
	}

	log_build_message(QString(tr("Building %1").arg(target->filename())), QLogger::LogLevel::Trace);

	bah->setMainBuilder(this);
	bah->addEnvironment(this->build_environment);
	bool res = bah->startBuild();
	build_result.status = res ? 0 : 1;
	return res;
}


bool BuildDriver::stopRequested()
{
	return stop_requested;
}

void BuildDriver::addToBuiltTargetList(QString f, QString t)
{
	built_target_list.append({ f, t });
}

QList<QPair<QString, QString>>  BuildDriver::getBuiltTargetList()
{
	return built_target_list;
}

void BuildDriver::stopBuild()
{
	stop_requested = true;
}


CopyResolver *BuildDriver::getCopyResolver() const
{
	return const_cast<CopyResolver *>(&copy_resolver);
}


