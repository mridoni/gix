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

#include "DbConnection.h"

DbConnection::DbConnection()
{
	internal_conn = nullptr;
	conn_info = nullptr;
	dbi = nullptr;
	dbm = nullptr;
	save_password = false;
	last_error = 0;
}

DbConnection::~DbConnection()
{}

bool DbConnection::connect()
{
	if (!dbi || !dbi->get_owner())
		return false;

	if (dbi->get_owner()->isOpen())
		return true;

	last_error = dbi->connect(conn_info, 0, "UTF-8");
	return (last_error == 0);
}

bool DbConnection::disconnect()
{
	if (!dbi || !dbi->get_owner())
		return false;

	last_error = dbi->terminate_connection();
	return (last_error == 0);
}

bool DbConnection::getSchemas(std::vector<SchemaInfo*>& schemas)
{
	if (!dbi)
		return false;

	return dbi->manager()->getSchemas(schemas);
}

bool DbConnection::getTables(QString table, std::vector<TableInfo*>& tables)
{
	if (!dbi)
		return false;

	return dbi->manager()->getTables(table.toStdString(), tables);
}

bool DbConnection::getColumns(QString schema, QString table, std::vector<ColumnInfo*>& columns)
{
	if (!dbi)
		return false;

	return dbi->manager()->getColumns(schema.toStdString(), table.toStdString(), columns);
}

bool DbConnection::getIndexes(QString schema, QString table, std::vector<IndexInfo*>& idxs)
{
	if (!dbi)
		return false;

	return dbi->manager()->getIndexes(schema.toStdString(), table.toStdString(), idxs);
}
