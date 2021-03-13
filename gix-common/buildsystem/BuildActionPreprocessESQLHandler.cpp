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

BuildActionPreprocessESQLHandler::BuildActionPreprocessESQLHandler()
{
}


BuildActionPreprocessESQLHandler::~BuildActionPreprocessESQLHandler()
{
}

bool BuildActionPreprocessESQLHandler::startBuild()
{
	uint64_t t_start = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
	
	importProjectEnvironment();

	QString outfile_path;
	QString build_dir = getBuildDirectory();
	QDir(".").mkpath(build_dir);

	if (!target->getItem() || target->dependencies()->size() != 1)
		return false;

	QString input_file = target->dependencies()->at(0)->filename();
	QString input_file_dir = PathUtils::getDirectory(input_file);

	QSettings settings;
	QProcessEnvironment env = QProcessEnvironment::systemEnvironment();

	QList<ProjectFile*> output;

	GixPreProcessor gp;

	if (!QDir::isAbsolutePath(target->filename()))
		outfile_path = PathUtils::combine(build_dir, target->filename());
	else
		outfile_path = target->filename();

	bool esql_preprocess_copy_files = this->environment["esql_preprocess_copy_files"].toBool();

	gp.verbose = true;
	gp.verbose_debug = true;

	gp.setCopyResolver(build_driver->getCopyResolver());
	
	gp.setOpt("emit_debug_info", true);
	gp.setOpt("emit_static_calls", true);

	gp.setOpt("anonymous_params", true);
	gp.setOpt("consolidated_map", true);	// we need this to generate a map against the full program listing
	gp.setOpt("preprocess_copy_files", esql_preprocess_copy_files);

	gp.addStep(new TPESQLProcessing(&gp));

	gp.setInputFile(input_file);
	gp.setOutputFile(outfile_path);	
	
	bool b = gp.process();

	if (!b || !QFile(outfile_path).exists()) {
		for (QString m : gp.err_messages)
			build_driver->log_build_message("ERROR: " + m, QLogger::LogLevel::Error);
		return false;
	}

	uint64_t elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count() - t_start;
	build_driver->log_build_message(QString("Build time for BuildActionPreprocessESQLHandler: %1ms").arg(elapsed), QLogger::LogLevel::Trace);

	return true;
}
