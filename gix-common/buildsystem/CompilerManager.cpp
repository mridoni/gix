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

#include "CompilerManager.h"
#include "PathUtils.h"
#include "IGixLogManager.h"
#include "GixGlobals.h"

#include <QDir>
#include <QFile>


CompilerManager::CompilerManager()
{
	init();
}

CompilerManager::~CompilerManager()
{
	cleanup();
}

void CompilerManager::init()
{
	IGixLogManager *logger = GixGlobals::getLogManager();

	logger->logMessage(GIX_CONSOLE_LOG, "Initializing compiler manager", QLogger::LogLevel::Info);

	if (compiler_defs.size() > 0)
		cleanup();

#if defined(__linux__)
	QString compiler_info;
	CompilerDefinition* cd_default = CompilerManager::tryGetDefault(compiler_info);
	if (cd_default != nullptr) {
		logger->logMessage(GIX_CONSOLE_LOG, QString("Adding distribution-provided compiler (%1)").arg(compiler_info) + def_file.absoluteFilePath()", QLogger::LogLevel::Info);
		compiler_defs[cd_default->getId()] = cd;
	}
#endif

	QString cdir = GixGlobals::getCompilerDefsDir();
	if (cdir.isEmpty()) {
		logger->logMessage(GIX_CONSOLE_LOG, "Compiler definitions directory is not set", QLogger::LogLevel::Error);
		return;
	}

	QDir compiler_defs_dir(QDir::fromNativeSeparators(cdir));
	if (!compiler_defs_dir.exists()) {
		logger->logMessage(GIX_CONSOLE_LOG, "Cannot locate compiler definitions directory: " + cdir, QLogger::LogLevel::Error);
		return;
	}

	logger->logMessage(GIX_CONSOLE_LOG, "Compiler definitions directory is " + cdir, QLogger::LogLevel::Debug);

	compiler_defs_dir.setNameFilters(QStringList("*.def"));

	for (QFileInfo def_file : compiler_defs_dir.entryInfoList(QDir::NoDotAndDotDot | QDir::Files)) {
		CompilerDefinition* cd = CompilerDefinition::load(def_file.absoluteFilePath());
		if (cd == nullptr) {
			logger->logMessage(GIX_CONSOLE_LOG, "Cannot parse compiler definition " + def_file.absoluteFilePath(), QLogger::LogLevel::Error);
			continue;
		}


		compiler_defs[cd->getId()] = cd;
	}

}

QMap<QString, CompilerDefinition*> CompilerManager::getCompilers()
{
	return compiler_defs;
}

GIXCOMMON_EXPORT CompilerDefinition* CompilerManager::tryGetDefault(QString& compiler_info)
{
#ifdef __linux__
	QProcess findProcess;
	QStringList arguments;
	arguments << "cobc";
	findProcess.start("which", arguments);
	findProcess.setReadChannel(QProcess::ProcessChannel::StandardOutput);

	if (!findProcess.waitForFinished())
		return false; // Not found or which does not work

	QString retStr(findProcess.readAll());

	retStr = retStr.trimmed();

	QFile file(retStr);
	QFileInfo check_file(file);
	if (check_file.exists() && check_file.isFile()) {

		// TODO: try to launch and retrieve version info

	}
#endif
	return nullptr;
}

void CompilerManager::cleanup()
{
	for (CompilerDefinition *cd : compiler_defs) {
		delete cd;
	}

	compiler_defs.clear();
}
