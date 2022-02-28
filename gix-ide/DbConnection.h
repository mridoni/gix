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

#include "IDataSourceInfo.h"
#include "IdeDbManager.h"
#include "IDbInterface.h"
#include "IDbManagerInterface.h"

class DbConnection
{
	friend class IdeDbManager;
	friend class DbManagerWindow;

public:
	DbConnection();
	~DbConnection();

	bool connect();
	bool disconnect();
	bool getSchemas(std::vector<SchemaInfo*>& res) ;
	bool getTables(QString table, std::vector<TableInfo*>& res);
	bool getColumns(QString schema, QString table, std::vector<ColumnInfo*>& columns);
	bool getIndexes(QString schema, QString table, std::vector<IndexInfo*>& idxs);

	
private:
	QString id;
	IDataSourceInfo *conn_info;
	IConnection* internal_conn;
	IDbInterface *dbi;
	IDbManagerInterface *dbm;
	int last_error;
	bool save_password;
};

