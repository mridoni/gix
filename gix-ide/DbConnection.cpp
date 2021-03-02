#include "DbConnection.h"

DbConnection::DbConnection()
{
	internal_conn = nullptr;
	conn_info = nullptr;
	dbi = nullptr;
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
