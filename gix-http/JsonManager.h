/*
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

