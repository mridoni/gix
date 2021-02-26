#pragma once

#include "IConnectionString.h"
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
	bool getSchemas(vector<SchemaInfo*>& res) ;
	bool getTables(QString table, vector<TableInfo*>& res);
	bool getColumns(QString schema, QString table, vector<ColumnInfo*>& columns);
	bool getIndexes(QString schema, QString table, vector<IndexInfo*>& idxs);

	
private:
	QString id;
	IConnectionString *conn_info;
	IConnection* internal_conn;
	IDbInterface *dbi;
	IDbManagerInterface *dbm;
	int last_error;
	bool save_password;
};

