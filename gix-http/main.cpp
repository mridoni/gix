/*
Copyright (C) 2010 Stefan Frings
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

/**
  @file
  @author Stefan Frings
*/

#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include "httplistener.h"
#include "templatecache.h"
#include "httpsessionstore.h"
#include "staticfilecontroller.h"
#include "filelogger.h"
#include "requestmapper.h"
#include "global.h"
#include "ServerConfig.h"
#include "ServiceManager.h"
#include "main.h"
#include "SysUtils.h"
#include "utils.h"
#include "RuntimeHelper.h"

using namespace stefanfrings;

ServerConfig *config = NULL;
RuntimeHelper *runtime = NULL;

bool init_server_environment();

/** Search the configuration file */
QString searchConfigFile()
{
	QString binDir = QCoreApplication::applicationDirPath();
	//QString appName=QCoreApplication::applicationName();
	QString fileName("gix-web-config.ini");

	QStringList searchList;
	searchList.append(binDir);
	searchList.append(QDir::rootPath() + "etc/opt");
	searchList.append(QDir::rootPath() + "etc");

	foreach(QString dir, searchList)
	{
		QFile file(dir + "/" + fileName);
		if (file.exists())
		{
			// found
			fileName = QDir(file.fileName()).canonicalPath();
			qDebug("Using config file %s", qPrintable(fileName));
			return fileName;
		}
	}

	// not found
	foreach(QString dir, searchList)
	{
		qWarning("%s/%s not found", qPrintable(dir), qPrintable(fileName));
	}
	//qFatal("Cannot find config file %s",qPrintable(fileName));
	return QString();
}


/**
  Entry point of the program.
*/
int main(int argc, char* argv[])
{
	QCoreApplication app(argc, argv);

	qsrand(QDateTime::currentMSecsSinceEpoch() % UINT_MAX);
	
	// Find the configuration file
	QString configFile;
	
	if (argc < 2) {
		configFile = searchConfigFile();
		if (configFile.isEmpty()) {
			fprintf(stderr, QString("Cannot find config file").toUtf8().constData());
			return 1;
		}
	}
	else
		configFile = argv[1];


	setbuf(stdout, NULL);
	setbuf(stderr, NULL);

	config = ServerConfig::read(configFile);
	if (!config) {
		fprintf(stderr, "Error in configuration");
		return 1;
	}

	SERVER_LOG = config->getServerId();

	auto dtFormat = QDateTime::currentDateTime().toString("dd-MM-yyyy hh:mm:ss.zzz");
	auto logLevel = QLogger::QLoggerManager::levelToText(QLogger::LogLevel::Info);
	auto text = QString("[%1] [%2] {%3} %4").arg(dtFormat).arg(logLevel).arg(SERVER_LOG).arg(QString("Starting from file %1").arg(configFile));
	fprintf(stdout, qPrintable(text));

	if (!is_valid_log_file(config->getLog())) {
		dtFormat = QDateTime::currentDateTime().toString("dd-MM-yyyy hh:mm:ss.zzz");
		logLevel = QLogger::QLoggerManager::levelToText(QLogger::LogLevel::Error);
		text = QString("[%1] [%2] {%3} %4").arg(dtFormat).arg(logLevel).arg(SERVER_LOG).arg(QString("Cannot write server log %1, server will not be started").arg(config->getLog()));
		fprintf(stderr, qPrintable(text));
		return 1;
	}

	QLogger::QLoggerManager::setConsoleEchoEnabled(true);

	log_manager = QLogger::QLoggerManager::getInstance();
	log_manager->addDestination(config->getLog(), SERVER_LOG, decode_log_level(config->getLogLevel()));

	QLogger::QLog_Info(SERVER_LOG, QString("%1 service(s) found").arg(config->getServices().size()));

	if (!init_server_environment()) {
		QLogger::QLog_Error(SERVER_LOG, QString("Server environment could not be initialized"));
		return 1;
	}

	int nstarted = 0;
	QMap<QString, ServiceConfig*> services = config->getServices();
	QMap<QString, ServiceConfig*>::iterator it;
	for (it = services.begin(); it != services.end(); ++it) {
		QLogger::QLog_Info(SERVER_LOG, QString("Starting service [%1]").arg(it.key()));
		if (!ServiceManager::start(it.value())) {
			QLogger::QLog_Error(SERVER_LOG, QString("Service [%1] could not be started").arg(it.key()));
		}
		else
			nstarted++;
	}

	/*
	// Configure logging into a file

	QSettings* logSettings=new QSettings(configFileName,QSettings::IniFormat,&app);
	logSettings->beginGroup("logging");
	FileLogger* logger=new FileLogger(logSettings,10000,&app);
	logger->installMsgHandler();


	// Configure template loader and cache
	QSettings* templateSettings=new QSettings(configFileName,QSettings::IniFormat,&app);
	templateSettings->beginGroup("templates");
	templateCache=new TemplateCache(templateSettings,&app);

	// Configure session store
	QSettings* sessionSettings=new QSettings(configFileName,QSettings::IniFormat,&app);
	sessionSettings->beginGroup("sessions");
	sessionStore=new HttpSessionStore(sessionSettings,&app);

	
	QSettings* fileSettings=new QSettings(configFileName,QSettings::IniFormat,&app);
	fileSettings->beginGroup("docroot");
	

*/
	
	// Configure static file controller
	staticFileController = new StaticFileController(config, &app);

	// Configure and start the TCP listener
	HttpListener *listener = new HttpListener(config, new RequestMapper(config, &app),&app);
	if (!listener->listen())
		exit(1);

	if (nstarted) {
		QLogger::QLog_Info(SERVER_LOG, "gix-http has started");
		app.exec();
	}
	else
		QLogger::QLog_Error(SERVER_LOG, "No services started, gix-http will now stop");

	if (runtime)
		runtime->cleanup();

	QLogger::QLog_Info(SERVER_LOG, "gix-http has stopped");
}

bool init_server_environment()
{
	QString libcob_path = PathUtils::combine(config->getRuntimePath(), "bin_x64");
	QString env_path = SysUtils::mergeEnvironmentVariable(QString("PATH"), libcob_path);

	QString search_path = config->getSearchPath();

	if (!search_path.isEmpty()) {
		if (!QDir(config->getSearchPath()).exists()) {
			QLogger::QLog_Error(SERVER_LOG, "Search path [" + search_path + "] not found, server will not be started");
			return false;
		}
		else {
			env_path = SysUtils::mergeEnvironmentVariableValue(env_path, search_path);
			//qputenv("COB_LIBRARY_PATH", search_path.toUtf8());
		}

	}

	QLogger::QLog_Debug(SERVER_LOG, "PATH for server is: " + env_path);

	qputenv("PATH", env_path.toUtf8());

	runtime = new RuntimeHelper();
	if (!runtime->loadRuntime(config)) {
		QLogger::QLog_Error(SERVER_LOG, "Cannot initialize GnuCOBOL runtime");
		return false;
	}

	runtime->cob_setenv("COB_LIBRARY_PATH", search_path.toUtf8().constData(), 1);

	return true;
}
