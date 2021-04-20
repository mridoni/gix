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

#if defined(__MINGW32__)
typedef unsigned char byte;
#include <cstddef>
#include <windows.h>
#endif

#include <QProcess>
#include <QString>

#include "BuildDriver.h"
#include "CompilerEnvironment.h"

class BuildDriver;

class GIXCOMMON_EXPORT CompilerConfiguration {

public:

	int id;
	QString homeDir;
	QString executablePath;
	QString binDirPath;
	QString libDirPath;
	QString configDirPath;
	QString copyDirPath;
	QString includeDirPath;
	QString runnerPath;
	QString host_platform;
	QString target_platform;
	bool isVsBased;

	CompilerEnvironment getCompilerEnvironment();
	QProcessEnvironment getEnvironment(BuildDriver *);
	
	static CompilerConfiguration *get(QString build_configuration, QString target_platform);

private:
	static CompilerConfiguration* getCompilerById(QString compiler_id, QString target_platform = "x64");

#if !defined(__MINGW32__) && (defined(_WIN32) || defined(_WIN64))
	bool add_vs_environment(BuildDriver *builder, QProcessEnvironment& env);
	bool add_libpath(QProcessEnvironment & env, QString host, QString target, QString install_path, QString version, BuildDriver * builder);
	bool add_lib(QProcessEnvironment & env, QString host, QString target, QString install_path, QString version, BuildDriver * builder);
	bool add_include(QProcessEnvironment & env, QString host, QString target, QString install_path, QString version, BuildDriver * builder);
	bool add_bin(QProcessEnvironment & env, QString host, QString target, QString install_path, QString version, BuildDriver * builder);
	QString get_win_sdk_path(QString p, QString& version);
#endif
};
