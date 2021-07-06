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

#include "BuildActionPreprocessESQLHandler.h"

#include <QEventLoop>

#include <chrono>

#include "ProjectFile.h"
#include "PathUtils.h"
//#include "Ide.h"
#include "SysUtils.h"
#include "GixPreProcessor.h"
#include "TPESQLProcessing.h"
#include "BuildDriver.h"
#include "ESQLConfiguration.h"

BuildActionPreprocessESQLHandler::BuildActionPreprocessESQLHandler()
{
}


BuildActionPreprocessESQLHandler::~BuildActionPreprocessESQLHandler()
{
}

bool BuildActionPreprocessESQLHandler::startBuild()
{
	QSettings settings;

	uint64_t t_start = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
	
	importProjectEnvironment();

	QString outfile_path;
	QString build_dir = getBuildDirectory();
	QDir(".").mkpath(build_dir);

	if (!target->getItem() || target->dependencies()->size() != 1)
		return false;

	QString input_file = target->dependencies()->at(0)->filename();
	QString input_file_dir = PathUtils::getDirectory(input_file);

	if (!QDir::isAbsolutePath(target->filename()))
		outfile_path = PathUtils::combine(build_dir, target->filename());
	else
		outfile_path = target->filename();

	QString esql_cfg_id = settings.value("esql_preprocessor_id", ESQLConfigurationType::GixInternal).toString();

	QString build_configuration = build_driver->getBuildEnvironment()["configuration"].toString();
	QString target_platform = build_driver->getBuildEnvironment()["platform"].toString();
	QString target_type = build_configuration + "/" + target_platform;

	QScopedPointer<CompilerConfiguration> ccfg(CompilerConfiguration::get(build_configuration, target_platform, environment));
	CompilerConfiguration *compiler_cfg = ccfg.data();
	if (compiler_cfg == nullptr) {
		build_driver->log_build_message(QString(tr("Invalid compiler configuration for target ")).arg(target_type), QLogger::LogLevel::Error, 1);
		return false;
	}

	CompilerEnvironment esql_cfg_env = compiler_cfg->getCompilerEnvironment();
	QScopedPointer<ESQLConfiguration> esql_cfg(ESQLConfiguration::get(esql_cfg_id, esql_cfg_env, build_configuration, target_platform));
	if (esql_cfg.isNull()) {
		build_driver->log_build_message(QString(tr("Invalid ESQL precompiler configuration for target ")).arg(target_type), QLogger::LogLevel::Error, 1);
		return false;
	}

	bool res = esql_cfg->run(build_driver, input_file, outfile_path, environment);

	uint64_t elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count() - t_start;
	build_driver->log_build_message(QString("Build time for BuildActionPreprocessESQLHandler: %1ms").arg(elapsed), QLogger::LogLevel::Trace);

	return res;
}
