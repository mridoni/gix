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

#include "IdeDbManager.h"
#include "DbConnection.h"
#include "IDataSourceInfo.h"
#include "DataSourceInfo.h"
#include "Ide.h"
#include "IdeTaskManager.h"
#include "DbInterfaceFactory.h"
#include "linq/linq.hpp"
#include <QSettings>
#include <unordered_set>

IdeDbManager::IdeDbManager()
{
	connect(Ide::TaskManager(), &IdeTaskManager::IdeReady, this, &IdeDbManager::init);
}

IdeDbManager::~IdeDbManager()
{
	cleanup();
}

bool IdeDbManager::init()
{
	QSettings settings;
	QSet<QString> initialized_ids;

	connections.clear();
	QString pfx = "dbmgr_conn_";
	int index = 1;

	auto std_keys = settings.allKeys().toVector().toStdVector();
	std::vector<QString> keys = cpplinq::from(std_keys)
		.where([pfx](QString a) { return a.startsWith(pfx); })
		.select([pfx](QString a) { return a.mid(0, pfx.length() + 4);  })
		.to_vector();

	for (QString id : keys) {
		if (initialized_ids.find(id) == initialized_ids.end()) {
			initialized_ids.insert(id);

			loadDbConnection(id);
		}

	}

	return true;
}

bool IdeDbManager::cleanup()
{
	return false;
}

bool IdeDbManager::test(IDataSourceInfo* conn_info)
{
	Connection conn;
	conn.setConnectionInfo(conn_info);

	IDbInterface* dbi = DbInterfaceFactory::getInterface(conn_info->getDbType(), spdlog::default_logger());
	if (!dbi)
		return false;

	conn.setDbInterface(dbi);
	dbi->set_owner(&conn);

	IConnectionOptions options;
	options.client_encoding = "UTF-8";
	int res = dbi->connect(conn_info, &options);
	if (res)
		return false;

	dbi->terminate_connection();
	return true;
}

void IdeDbManager::newConnection(IDataSourceInfo * conn_info, bool save_password)
{
	QSettings settings;

	QString conn_name = QString::fromStdString(conn_info->getName());

	DbConnection* dbc = IdeDbManager::createConnection(conn_info);
	if (!dbc)
		return;

	connections[conn_name] = dbc;

	emit connectionAdded(conn_name, dbc);
}

void IdeDbManager::saveConnection(DbConnection* conn, bool save_password)
{
	QSettings settings;
	QString pfx = "dbmgr_conn_";
	int index = 1;

	if (conn->id.isEmpty()) {	// new connection, must generate id
		auto std_keys = settings.allKeys().toVector().toStdVector();
		std::vector<QString> keys = cpplinq::from(std_keys)
					.where([pfx](QString a) { return a.startsWith(pfx); }).to_vector();
					
		if (keys.size() > 0) {
			sort(keys.begin(), keys.end());
			QString last_key = keys[keys.size() - 1];
			last_key = last_key.replace("dbmgr_conn_", "");
			last_key = last_key.mid(0, last_key.indexOf("_"));
			index = last_key.toInt() + 1;
		}

		QString s_index = QString("%1").arg(index, 3, 10, QChar('0'));
		pfx = pfx + s_index + "_";
	}

	settings.setValue(pfx + "name", QString::fromStdString(conn->conn_info->getName()));
	settings.setValue(pfx + "info", QString::fromStdString(conn->conn_info->toConnectionString(save_password, save_password ? conn->conn_info->getPassword() : "")));
	settings.setValue(pfx + "has_password", save_password);
	settings.setValue(pfx + "dbtype", QString::fromStdString(conn->conn_info->getDbType()));
	settings.setValue(pfx + "dbhost", QString::fromStdString(conn->conn_info->getHost()));
	settings.setValue(pfx + "dbport", conn->conn_info->getPort());
	settings.setValue(pfx + "dbname", QString::fromStdString(conn->conn_info->getDbName()));
	settings.setValue(pfx + "dbusername", QString::fromStdString(conn->conn_info->getUsername()));
	settings.setValue(pfx + "dbpassword", save_password ? QString::fromStdString(conn->conn_info->getPassword()) : "");
	//settings.setValue(pfx + "dbdefault_schema", QString::fromStdString(conn->conn_info->getDefaultSchema()));
}

void IdeDbManager::loadDbConnection(QString pfx)
{
	QSettings settings;

	QString conn_name = settings.value(pfx + "name").toString();
	QString conn_info_s = settings.value(pfx + "info").toString();
	bool save_password = settings.value(pfx + "has_password").toBool();
	QString dbtype = settings.value(pfx + "dbtype").toString();
	QString host = settings.value(pfx + "dbhost").toString();
	int port = settings.value(pfx + "dbport").toInt();
	QString dbname = settings.value(pfx + "dbname").toString();
	QString username = settings.value(pfx + "dbusername").toString();
	QString password = save_password ? settings.value(pfx + "dbpassword").toString() : QString();
	QString default_schema = settings.value(pfx + "dbdefault_schema").toString();

	DbConnection* dbc = new DbConnection;
	std::string ci = conn_info_s.toStdString();
	dbc->conn_info = new DataSourceInfo();
	dbc->conn_info->init(ci, dbname.toStdString(), username.toStdString(), password.toStdString());	// FIXME
	dbc->internal_conn = new Connection();
	dbc->internal_conn->setConnectionInfo(dbc->conn_info);
	dbc->internal_conn->setName(conn_name.toStdString());
	dbc->dbi = DbInterfaceFactory::getInterface(dbtype.toStdString(), spdlog::default_logger());
	dbc->dbi->set_owner(dbc->internal_conn);

	connections[conn_name] = dbc;

	emit connectionAdded(conn_name, dbc);
}

DbConnection* IdeDbManager::createConnection(IDataSourceInfo * conn_info)
{
	IDbInterface* dbi = DbInterfaceFactory::getInterface(conn_info->getDbType(), spdlog::default_logger());
	if (!dbi)
		return nullptr;

	DbConnection* dbc = new DbConnection;
	dbc->conn_info = conn_info;
	dbc->internal_conn = new Connection();
	dbc->internal_conn->setConnectionInfo(conn_info);
	dbc->internal_conn->setName(conn_info->getName());
	dbc->dbi = dbi;
	dbc->dbi->set_owner(dbc->internal_conn);

	return dbc;
}
