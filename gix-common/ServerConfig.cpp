/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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

#include "ServerConfig.h"
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

#include "linq/linq.hpp"

ServerConfig::ServerConfig()
{
	cleanup_interval = 1000;
	min_threads = 1; 
	max_threads = 100;
	ssl_enabled = false;
	ssl_cert_file = "";
	ssl_key_file = "";
	read_timeout = 10000;
	max_request_size = 16000;
	max_multipart_size = 1000000;
	base_path = ".";
	max_age = 60000;
	encoding = "UTF-8";
	max_cached_file_size = 65536;
	cache_size = 1000000;
	cache_time = 60000;
	log_console_echo = false;
}

ServerConfig::~ServerConfig()
{
	QMap<QString, ServiceConfig*>::iterator it_svcs;
	for (it_svcs = services.begin(); it_svcs != services.end(); ++it_svcs) {
		ServiceConfig *svc = it_svcs.value();
		if (svc)
			delete svc;
	}
}

ServerConfig* ServerConfig::read(QString path)
{
	// TODO: validation + error checking
	try {
		QJsonParseError error;

		QFile file(path);
		file.open(QIODevice::ReadOnly);
		QByteArray content = file.readAll();
		file.close();

		QJsonDocument jr = QJsonDocument::fromJson(content, &error);
		if (jr.isNull()) {
			QString msg = QString("Parse error at %1: %2").arg(error.offset).arg(error.errorString());
			qWarning(msg.toUtf8().constData());
			return nullptr;
		}

		QJsonObject jcfg = jr.object();
		QJsonObject jserver = jr["server"].toObject();

		ServerConfig* cfg = new ServerConfig();

		cfg->address = jserver["address"].toString();
		cfg->port = jserver["port"].toInt();
		cfg->debug = jserver["debug"].toBool();
		cfg->log_level = jserver.value("log_level").toString();
		cfg->log = jserver.value("log").toString();
		cfg->runtime_path = jserver["runtime_path"].toString();
		cfg->runtime_path_debug = jserver["runtime_path_debug"].toString();
		cfg->search_path = jserver["search_path"].toString();
		cfg->log_console_echo = jserver["log_console_echo"].toBool();
		if (jserver.keys().contains("environment")) {
			QJsonArray a = jserver["environment"].toArray();
			for (int i = 0; i < a.size(); i++) {
				QJsonObject o = a[i].toObject();
				QString k = o.keys().at(0);
				QString v = o.value(k).toString();
				cfg->environment.insert(k, v);
			}
		}

		if (jserver.contains("cleanup_interval"))
			cfg->cleanup_interval = jserver["cleanup_interval"].toInt();

		if (jserver.contains("min_threads"))
			cfg->min_threads = jserver["min_threads"].toInt();

		if (jserver.contains("max_threads"))
			cfg->max_threads = jserver["max_threads"].toInt();

		if (jserver.contains("ssl_enabled"))
			cfg->ssl_enabled = jserver["ssl_enabled"].toBool();

		if (jserver.contains("ssl_cert_file"))
			cfg->ssl_cert_file = jserver["ssl_cert_file"].toString();

		if (jserver.contains("ssl_key_file"))
			cfg->ssl_key_file = jserver["ssl_key_file"].toString();

		if (jserver.contains("read_timeout"))
			cfg->read_timeout = jserver["read_timeout"].toInt();

		if (jserver.contains("max_request_size"))
			cfg->max_request_size = jserver["max_request_size"].toInt();

		if (jserver.contains("max_multipart_size"))
			cfg->max_multipart_size = jserver["max_multipart_size"].toInt();

		if (jserver.contains("base_path"))
			cfg->base_path = jserver["base_path"].toString();

		if (jserver.contains("max_age"))
			cfg->max_age = jserver["max_age"].toInt();

		if (jserver.contains("encoding"))
			cfg->encoding = jserver["encoding"].toString();

		if (jserver.contains("max_cached_file_size"))
			cfg->max_cached_file_size = jserver["max_cached_file_size"].toInt();

		if (jserver.contains("cache_size"))
			cfg->cache_size = jserver["cache_size"].toInt();

		if (jserver.contains("cache_time"))
			cfg->cache_time = jserver["cache_time"].toInt();

		QJsonArray jservices = jr["services"].toArray();
		for (int i = 0; i < jservices.size(); i++) {
			QJsonObject jservice = jservices[i].toObject();
			ServiceConfig *sc = ServiceConfig::ofType(jservice.value("type").toString());
			if (!sc) {
				QString msg = QString("Invalid configuration for service %1").arg(sc->getName());
				qErrnoWarning(msg.toUtf8().constData());
				delete cfg;
				return nullptr;
			}

			sc->owner = cfg;
			sc->name = jservice.value("name").toString();
			sc->description = jservice.value("description").toString();
			sc->program = jservice.value("program").toString();
			sc->url = jservice.value("url").toString();
			sc->log_level = jservice.value("log_level").toString();
			sc->log = jservice.value("log").toString();
			sc->type = jservice.value("type").toString();
			sc->method = jservice.value("method").toString();
			sc->interface_in_field_name = jservice.value("interface_in_field").toString();
			sc->interface_out_field_name = jservice.value("interface_out_field").toString();
			sc->debug_port = jservice.value("debug_port").toInt();
			sc->enabled = jservice["enabled"].toBool();
			QJsonArray a = jservice["environment"].toArray();
			for (int i = 0; i < a.size(); i++) {
				QJsonObject o = a[i].toObject();
				QString k = o.keys().at(0);
				QString v = o.value(k).toString();
				sc->environment.insert(k, v);
			}

			//if (jservice.contains("base_path"))
			//	sc->base_path = jservice["base_path"].toString();

			if (jservice.contains("shared_module"))
				sc->shared_module = jservice["shared_module"].toString();

			sc->readAdditionalConfig(jservice.toVariantMap());

			if (!sc->validateConfig()) {
				QString msg = QString("Invalid configuration for service %1").arg(sc->getName());
				qErrnoWarning(msg.toUtf8().constData());
				delete cfg;
				return nullptr;
			}
			
			cfg->services[sc->name] = sc;
		}

		return cfg;
	}
	catch (...)
	{
		return nullptr;
	}
}


QMap<QString, ServiceConfig*> ServerConfig::getServices()
{
	return services;
}

void ServerConfig::addService(QString name, ServiceConfig *sc)
{
	services.insert(name, sc);
}

bool ServerConfig::write(QString file_path)
{
	QMap<QString, QString>::iterator it;

	QFile http_cfg(file_path);
	if (!http_cfg.open(QIODevice::OpenModeFlag::WriteOnly)) {
		return false;
	}
		
	QJsonDocument jr;
	QJsonObject jcfg;
	QJsonObject jserver;

	jserver.insert("address", getAddressString());
	jserver.insert("port", port);
	jserver.insert("debug", debug);
	jserver.insert("log_level", log_level);
	jserver.insert("log", log);
	jserver.insert("runtime_path", runtime_path);
	jserver.insert("runtime_path_debug", runtime_path_debug);
	jserver.insert("search_path", search_path);
	QJsonArray svr_env;

	for (it = environment.begin(); it != environment.end(); ++it) {
		QJsonObject jprop;
		jprop.insert(it.key(), it.value());
		svr_env.append(jprop);
	}

	jserver.insert("environment", svr_env);
	jserver.insert("log_console_echo", log_console_echo);

	jserver.insert("cleanup_interval", cleanup_interval);
	jserver.insert("min_threads", min_threads);
	jserver.insert("max_threads", max_threads);
	jserver.insert("ssl_enabled", ssl_enabled);
	jserver.insert("ssl_cert_file", ssl_cert_file);
	jserver.insert("ssl_key_file", ssl_key_file);
	jserver.insert("read_timeout", read_timeout);
	jserver.insert("max_request_size", max_request_size);
	jserver.insert("max_multipart_size", max_multipart_size);
	jserver.insert("base_path", base_path);
	jserver.insert("max_age", max_age);
	jserver.insert("encoding", encoding);
	jserver.insert("max_cached_file_size", max_cached_file_size);
	jserver.insert("cache_size", cache_size);
	jserver.insert("cache_time", cache_time);


	jcfg.insert("server", jserver);

	QJsonArray jservices;
	QMap<QString, ServiceConfig *>::iterator it_svcs;
	for (it_svcs = services.begin(); it_svcs != services.end(); ++it_svcs) {
		QJsonObject jservice;
		ServiceConfig* sc = it_svcs.value();

		jservice.insert("name", sc->name);
		jservice.insert("description", sc->description);
		jservice.insert("program", sc->program);
		jservice.insert("url", sc->url);
		jservice.insert("log_level", sc->log_level);
		jservice.insert("log", sc->log);
		jservice.insert("type", sc->type);
		jservice.insert("method", sc->method);
		jservice.insert("interface_in_field", sc->interface_in_field_name);
		jservice.insert("interface_out_field", sc->interface_out_field_name);
		jservice.insert("debug_port", sc->debug_port);
		jservice.insert("enabled", sc->enabled);

		QJsonArray svc_env;

		for (it = sc->environment.begin(); it != sc->environment.end(); ++it) {
			QJsonObject jprop;
			jprop.insert(it.key(), it.value());
			svc_env.append(jprop);
		}

		jservice.insert("environment", svc_env);

		jservices.append(jservice);
	}

	jcfg.insert("services", jservices);

	jr.setObject(jcfg);

	int rc = http_cfg.write(jr.toJson());
	http_cfg.close();

	return (rc != -1);
}

QString ServerConfig::getLog()
{
	return log;
}

QString ServerConfig::getLogLevel()
{
	return log_level;
}

QString ServerConfig::getServerId()
{
	return address.toString() + ":" + QString::number(port);
}

QString GIXCOMMON_EXPORT ServerConfig::getAddressString()
{
	return address.toString();
}

uint16_t GIXCOMMON_EXPORT ServerConfig::getPort()
{
	return port;
}

bool GIXCOMMON_EXPORT ServerConfig::getDebugEnabled()
{
	return debug;
}

int GIXCOMMON_EXPORT ServerConfig::getCleanupInterval()
{
	return cleanup_interval;
}

int GIXCOMMON_EXPORT ServerConfig::getMinThreads()
{
	return min_threads;
}

int GIXCOMMON_EXPORT ServerConfig::getMaxThreads()
{
	return max_threads;
}

bool GIXCOMMON_EXPORT ServerConfig::getSslEnabled()
{
	return ssl_enabled;
}

QString GIXCOMMON_EXPORT ServerConfig::getSslKeyFile()
{
	return ssl_key_file;
}

QString GIXCOMMON_EXPORT ServerConfig::getSslCertFile()
{
	return ssl_cert_file;
}

int GIXCOMMON_EXPORT ServerConfig::getReadTimeout()
{
	return read_timeout;
}

int GIXCOMMON_EXPORT ServerConfig::getMaxRequestSize()
{
	return max_request_size;
}

int GIXCOMMON_EXPORT ServerConfig::getMaxMultiPartSize()
{
	return max_multipart_size;
}

QString GIXCOMMON_EXPORT ServerConfig::getBasePath()
{
	return base_path;
}

int GIXCOMMON_EXPORT ServerConfig::getMaxAge()
{
	return max_age;
}

QString GIXCOMMON_EXPORT ServerConfig::getEncoding()
{
	return encoding;
}

int GIXCOMMON_EXPORT ServerConfig::getMaxCachedFileSize()
{
	return max_cached_file_size;
}

int GIXCOMMON_EXPORT ServerConfig::getCacheTime()
{
	return cache_time;
}

int GIXCOMMON_EXPORT ServerConfig::getCacheSize()
{
	return cache_size;
}

QString GIXCOMMON_EXPORT ServerConfig::getRuntimePath()
{
	return debug ? runtime_path_debug : runtime_path;
}

bool GIXCOMMON_EXPORT ServerConfig::isLogConsoleEchoEnabled()
{
	return log_console_echo;
}

QString GIXCOMMON_EXPORT ServerConfig::getSearchPath()
{
	return search_path;
}

void GIXCOMMON_EXPORT ServerConfig::setLog(QString v)
{
	log = v;
}

void GIXCOMMON_EXPORT ServerConfig::setLogLevel(QString v)
{
	log_level = v;
}

void GIXCOMMON_EXPORT ServerConfig::setAddress(QString v)
{
	address = QHostAddress(v);
}

void GIXCOMMON_EXPORT ServerConfig::setPort(uint16_t v)
{
	port = v;
}

void GIXCOMMON_EXPORT ServerConfig::setDebugEnabled(bool v)
{
	debug = v;
}

void GIXCOMMON_EXPORT ServerConfig::setCleanupInterval(int v)
{
	cleanup_interval = v;
}

void GIXCOMMON_EXPORT ServerConfig::setMinThreads(int v)
{
	min_threads = v;
}

void GIXCOMMON_EXPORT ServerConfig::setMaxThreads(int v)
{
	max_threads = v;
}

void GIXCOMMON_EXPORT ServerConfig::setSslEnabled(bool v)
{
	ssl_enabled = v;
}

void GIXCOMMON_EXPORT ServerConfig::setSslKeyFile(QString v)
{
	ssl_key_file = v;
}

void GIXCOMMON_EXPORT ServerConfig::setSslCertFile(QString v)
{
	ssl_cert_file = v;
}

void GIXCOMMON_EXPORT ServerConfig::setReadTimeout(int v)
{
	read_timeout = v;
}

void GIXCOMMON_EXPORT ServerConfig::setMaxRequestSize(int v)
{
	max_request_size = v;
}

void GIXCOMMON_EXPORT ServerConfig::setMaxMultiPartSize(int v)
{
	max_multipart_size = v;
}

void GIXCOMMON_EXPORT ServerConfig::setBasePath(QString v)
{
	base_path = v;
}

void GIXCOMMON_EXPORT ServerConfig::setMaxAge(int v)
{
	max_age = v;
}

void GIXCOMMON_EXPORT ServerConfig::setEncoding(QString v)
{
	encoding = v;
}

void GIXCOMMON_EXPORT ServerConfig::setMaxCachedFileSize(int v)
{
	max_cached_file_size = v;
}

void GIXCOMMON_EXPORT ServerConfig::setCacheTime(int v)
{
	cache_time = v;
}

void GIXCOMMON_EXPORT ServerConfig::setCacheSize(int v)
{
	cache_size = v;
}

void GIXCOMMON_EXPORT ServerConfig::setRuntimePath(QString v)
{
	runtime_path = v;
}

void GIXCOMMON_EXPORT ServerConfig::setSearchPath(QString v)
{
	search_path = v;
}

bool GIXCOMMON_EXPORT ServerConfig::setLogConsoleEchoEnabled(bool b)
{
	return log_console_echo = b;
}

ServiceConfig::ServiceConfig()
{
	debug_port = 13001;
	enabled = true;
	owner = nullptr;
}

ServiceConfig::~ServiceConfig()
{
}

GIXCOMMON_EXPORT ServiceConfig* ServiceConfig::ofType(QString type)
{
	if (type.toLower() == "rest")
		return new RestServiceConfig();

	if (type.toLower() == "soap")
		return new SoapServiceConfig();

	return nullptr;
}


QString GIXCOMMON_EXPORT ServiceConfig::getSchemaUrl()
{
	QString u = this->url;
	if (u.endsWith("/"))
		u = u.chopped(1);

	return QString(u + "/schema");
}

void *ServiceConfig::getPrivateData()
{
	return private_data;
}

void ServiceConfig::setPrivateData(void *pd)
{
	private_data = pd;
}


RestServiceConfig::RestServiceConfig() : ServiceConfig()
{
	type = "rest";
}

QMap<QString, QByteArray> RestServiceConfig::getRequestParameters(QByteArray body, QMap<QByteArray, QByteArray> params)
{
	// TODO: check/use JSON input, for now we use HTTP parameters
	QMap<QByteArray, QByteArray>::iterator it;
	QMap<QString, QByteArray> res;

	for (it = params.begin(); it != params.end(); ++it) {
		QString k = QString::fromUtf8(it.key());
		QByteArray v = it.value();
		res[k] = v;
	}

	return res;
}

void RestServiceConfig::setResponseParameters(QMap <QString, QByteArray>, QByteArray& body)
{

}

bool RestServiceConfig::validateConfig()
{
	return true;
}

void RestServiceConfig::readAdditionalConfig(QVariantMap acfg)
{

}

SoapServiceConfig::SoapServiceConfig() : ServiceConfig()
{
	type = "soap";
}

QMap<QString, QByteArray> SoapServiceConfig::getRequestParameters(QByteArray body, QMap<QByteArray, QByteArray> params)
{
	return QMap<QString, QByteArray>();
}

void SoapServiceConfig::setResponseParameters(QMap <QString, QByteArray>, QByteArray& body)
{

}

bool SoapServiceConfig::validateConfig()
{
	return true;
}

void SoapServiceConfig::readAdditionalConfig(QVariantMap acfg)
{

}
