#pragma once

#if defined(__MINGW32__)
typedef unsigned char byte;
#include <cstddef>
#include <windows.h>
#endif

#include <QProcess>
#include <QString>

#include "BuildDriver.h"

class BuildDriver;

class CompilerConfiguration {

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

	QProcessEnvironment getEnvironment(BuildDriver *);
	GIXCOMMON_EXPORT static CompilerConfiguration *get(QString build_configuration, QString target_platform);

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
