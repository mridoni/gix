#pragma once

#include <QString>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QJsonValue>

#include "gixcommon_global.h"
#include "ServerConfig.h"
#include "DataEntry.h"

enum class SchemaType
{
	In = 1,
	Out = 2
};

class JsonManager
{
public:

	 JsonManager(ServiceConfig *);

	QString getSchema(SchemaType schema_type);
	QString serializeData(QHash<QString, QString> &data);

private:
	ExternalInterfaceData* data;
	ServiceConfig* service;

	void add_entry(QJsonObject& jprops, QList<DataEntry*> entries);
	void add_data_entry(QJsonObject& jprops, QHash<QString, QString> &entries);

};

