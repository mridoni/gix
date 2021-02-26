#pragma once

#include <QHostAddress>
#include <QFile>
#include <QMap>
#include <QList>
#include <QString>

#include "gixcommon_global.h"

#define _prop_get(_v,_t,_m) _t _m() { return _v; }
#define _prop_set(_v,_t,_m) void _m(_t v) { _v = v; }

class ServerConfig;
class ExternalInterfaceData;
class DataEntry;
class ServiceModuleInfo;

class ServiceConfig {

	friend class ServerConfig;
	friend class ServiceManager;
	friend class JsonManager;
	/* Ex.
		  {
			"name": "TESTSVC1",
			"description": "Test service 1",
			"program": "TESTSVC1",
			"path": "c:/testsvcs",
			"url": "/testsvc1",
			"search_path": "c:/testsvcs",
			"environment": [
			  { "DBNAME": "testdb" },
			  { "DBAUTH": "test.test" }
			],
			"log_level": "info",
			"log": "c:/testsvcs/log/testsvc1.log",
			"type": "rest",
			"method": "post",
			"interface_in_field": "",
			"interface_out_field": "",
			"interface_force_rebuild": false,
			"debug": "on",
			"debug_port": "auto",
			"enabled": true
		  },
	*/

public:
	GIXCOMMON_EXPORT virtual ~ServiceConfig();

	static GIXCOMMON_EXPORT ServiceConfig* ofType(QString type);

	QString GIXCOMMON_EXPORT getSchemaUrl();

	GIXCOMMON_EXPORT void *getPrivateData();
	GIXCOMMON_EXPORT void setPrivateData(void *pd);

	virtual bool validateConfig() = 0;
	virtual void readAdditionalConfig(QVariantMap acfg) = 0;
	virtual QMap<QString, QByteArray> GIXCOMMON_EXPORT getRequestParameters(QByteArray body, QMap<QByteArray, QByteArray> params) = 0;
	virtual void GIXCOMMON_EXPORT setResponseParameters(QMap <QString, QByteArray>,  QByteArray& body) = 0;

	_prop_get(name, QString, getName);
	_prop_get(description, QString, getDescription);
	_prop_get(program, QString, getProgram);
	_prop_get(shared_module, QString, getSharedModule);
	//_prop_get(base_path, QString, getBasePath);
	_prop_get(url, QString, getUrl);
	_prop_get(interface_in_field_name, QString, getInterfaceInFieldName);
	_prop_get(interface_out_field_name, QString, getInterfaceOutFieldName);
	_prop_get(enabled, bool, isEnabled);
	_prop_get(log, QString, getLog);
	_prop_get(log_module, QString, getLogModule);
	_prop_get(type, QString, getType);
	_prop_get(log_level, QString, getLogLevel);
	_prop_get(owner, ServerConfig *, getServerConfig);

	_prop_set(name, QString, setName);
	_prop_set(description, QString, setDescription);
	_prop_set(program, QString, setProgram);
	_prop_set(shared_module, QString, setSharedModule);
	//_prop_set(base_path, QString, setBasePath);
	_prop_set(url, QString, setUrl);
	_prop_set(interface_in_field_name, QString, setInterfaceInFieldName);
	_prop_set(interface_out_field_name, QString, setInterfaceOutFieldName);
	_prop_set(interface_force_rebuild, bool, setInterfaceForceRebuild);
	_prop_set(enabled, bool, setEnabled);
	_prop_set(log, QString, setLog);
	_prop_set(log_level, QString, setLogLevel);

	_prop_get(itf_data_in, ExternalInterfaceData *, getInterfaceIn);
	_prop_get(itf_data_out, ExternalInterfaceData *, getInterfaceOut);

protected:

	ServiceConfig();

	ServerConfig* owner;

	QString name;
	QString description;
	QString program;
	QString shared_module;
	//QString base_path;
	QString url;
	QMap<QString, QString> environment;
	QString log_level;
	QString log;
	QString type;
	QString method;
	QString interface_in_field_name;
	QString interface_out_field_name;
	bool interface_force_rebuild = false;
	uint16_t debug_port;
	bool enabled;

	QString log_module;


private:
	ExternalInterfaceData *itf_data_in = nullptr;
	ExternalInterfaceData *itf_data_out = nullptr;
	void *private_data = nullptr;

	void add_tree_children(DataEntry *e);
};

class ServerConfig
{
	/* Ex.
		"address": "127.0.0.1",
			"port" : 9090,
			"log_level": "info",
			"log": "c:/testsvcs/log/testsvr.log",
			"debug" : "on",
			"runtime_path" : "C:/GnuCOBOL-2.2",
			"runtime_path_debug" : "C:/GnuCOBOL-2.2-dbg",
			"environment" : [
		{ "VAR1": "test1" },
		{ "VAR2": "test2" }
			] ,
	*/
public:

	GIXCOMMON_EXPORT ServerConfig();
	GIXCOMMON_EXPORT ~ServerConfig();

	static GIXCOMMON_EXPORT ServerConfig* read(QString path);

	QMap<QString, ServiceConfig*> GIXCOMMON_EXPORT getServices();
	void GIXCOMMON_EXPORT addService(QString name, ServiceConfig*);

	bool GIXCOMMON_EXPORT write(QString file_path);

	QString GIXCOMMON_EXPORT getLog();
	QString GIXCOMMON_EXPORT getLogLevel();
	QString GIXCOMMON_EXPORT getServerId();
	QString GIXCOMMON_EXPORT getAddressString();
	uint16_t GIXCOMMON_EXPORT getPort();
	bool GIXCOMMON_EXPORT getDebugEnabled();
	int GIXCOMMON_EXPORT getCleanupInterval();
	int GIXCOMMON_EXPORT getMinThreads();
	int GIXCOMMON_EXPORT getMaxThreads();
	bool GIXCOMMON_EXPORT getSslEnabled();
	QString GIXCOMMON_EXPORT getSslKeyFile();
	QString GIXCOMMON_EXPORT getSslCertFile();
	int GIXCOMMON_EXPORT getReadTimeout();
	int GIXCOMMON_EXPORT getMaxRequestSize();
	int GIXCOMMON_EXPORT getMaxMultiPartSize();
	QString GIXCOMMON_EXPORT getBasePath();
	int GIXCOMMON_EXPORT getMaxAge();
	QString GIXCOMMON_EXPORT getEncoding();
	int GIXCOMMON_EXPORT getMaxCachedFileSize();
	int GIXCOMMON_EXPORT getCacheTime();
	int GIXCOMMON_EXPORT getCacheSize();
	QString GIXCOMMON_EXPORT getRuntimePath();
	bool GIXCOMMON_EXPORT isLogConsoleEchoEnabled();
	QString GIXCOMMON_EXPORT getSearchPath();

	void GIXCOMMON_EXPORT setLog(QString v);
	void GIXCOMMON_EXPORT setLogLevel(QString v);
	void GIXCOMMON_EXPORT setAddress(QString v);
	void GIXCOMMON_EXPORT setPort(uint16_t v);
	void GIXCOMMON_EXPORT setDebugEnabled(bool v);
	void GIXCOMMON_EXPORT setCleanupInterval(int v);
	void GIXCOMMON_EXPORT setMinThreads(int v);
	void GIXCOMMON_EXPORT setMaxThreads(int v);
	void GIXCOMMON_EXPORT setSslEnabled(bool v);
	void GIXCOMMON_EXPORT setSslKeyFile(QString v);
	void GIXCOMMON_EXPORT setSslCertFile(QString v);
	void GIXCOMMON_EXPORT setReadTimeout(int v);
	void GIXCOMMON_EXPORT setMaxRequestSize(int v);
	void GIXCOMMON_EXPORT setMaxMultiPartSize(int v);
	void GIXCOMMON_EXPORT setBasePath(QString v);
	void GIXCOMMON_EXPORT setMaxAge(int v);
	void GIXCOMMON_EXPORT setEncoding(QString v);
	void GIXCOMMON_EXPORT setMaxCachedFileSize(int v);
	void GIXCOMMON_EXPORT setCacheTime(int v);
	void GIXCOMMON_EXPORT setCacheSize(int v);
	void GIXCOMMON_EXPORT setRuntimePath(QString v);
	void GIXCOMMON_EXPORT setSearchPath(QString v);
	bool GIXCOMMON_EXPORT setLogConsoleEchoEnabled(bool b);

private:
	QHostAddress address;
	uint16_t port;
	QString log_level;
	QString log;
	bool debug;
	QString runtime_path;
	QString runtime_path_debug;
	QMap<QString, QString> environment;
	QString search_path;
	QMap<QString, ServiceConfig *> services;
	int cleanup_interval;
	int min_threads;
	int max_threads;
	bool ssl_enabled;
	QString ssl_cert_file;
	QString ssl_key_file;
	int read_timeout;
	int max_request_size;
	int max_multipart_size;
	QString base_path;
	int max_age;
	QString encoding;
	int max_cached_file_size;
	int cache_time;
	int cache_size;
	bool log_console_echo;
};

class RestServiceConfig : public ServiceConfig
{
public:
	GIXCOMMON_EXPORT RestServiceConfig();

	virtual QMap<QString, QByteArray> GIXCOMMON_EXPORT getRequestParameters(QByteArray body, QMap<QByteArray, QByteArray> params) override;
	virtual void GIXCOMMON_EXPORT setResponseParameters(QMap <QString, QByteArray>, QByteArray& body) override;

	// Inherited via ServiceConfig
	virtual bool validateConfig() override;
	virtual void readAdditionalConfig(QVariantMap acfg) override;
};

class SoapServiceConfig : public ServiceConfig
{
public:
	GIXCOMMON_EXPORT SoapServiceConfig();

	virtual QMap<QString, QByteArray> GIXCOMMON_EXPORT getRequestParameters(QByteArray body, QMap<QByteArray, QByteArray> params) override;
	virtual void GIXCOMMON_EXPORT setResponseParameters(QMap <QString, QByteArray>, QByteArray& body) override;

	// Inherited via ServiceConfig
	virtual bool validateConfig() override;
	virtual void readAdditionalConfig(QVariantMap acfg) override;
};