#pragma once

#include <QObject>
#include <QString>
#include <QStringList>
#include <QMap>
#include <QVariant>
#include <QProcess>

#include "CompilerEnvironment.h"
#include "gixcommon_global.h"

class GIXCOMMON_EXPORT ESQLConfigurationType
{
public:
	inline const static QString GixInternal = "esql_driver_gix_internal";
	inline const static QString GixExternal = "esql_driver_gix_external";
};

class BuildDriver;

class GIXCOMMON_EXPORT ESQLConfiguration : public QObject
{
	Q_OBJECT

public:
	static ESQLConfiguration *get(QString id, CompilerEnvironment e, QString c, QString p);

	bool run(BuildDriver *build_driver, QString input_file, QString output_file, QMap<QString, QVariant> opts);

	CompilerEnvironment env;
	QString configuration;
	QString platform;

	QString getBinPath();
	QString getExe();
	QString getCmdLine();
	QMap<QString, QString> getEnvironment(QString driver_type);
	QStringList getCopyPathList();
	QStringList getLinkLibPathList();
	QStringList getLinkLibNameList();
	QStringList getRuntimeLibPathList(QString driver_type);


private:
	QString id;
	QString name;

	QString bin_path;
	QString pp_exe;
	QString pp_cmdline;
	QMap<QString, QString> pp_env;
	QStringList copy_path;
	QStringList link_lib_path;
	QStringList link_lib_name;	
	QStringList rt_lib_path;


	bool runGixSqlInternal(BuildDriver *build_driver, QString input_file, QString output_file, QMap<QString, QVariant> opts);
	void readStdOut(BuildDriver *build_driver, QProcess *p);
	void readStdErr(BuildDriver *build_driver, QProcess *p);
};

