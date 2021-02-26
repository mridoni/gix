#include "DbInterfaceMySQL.h"


bool DbInterfaceMySQL::getSchemas(vector<SchemaInfo*>& res)
{
	return false;
}

bool DbInterfaceMySQL::getTables(string table, vector<TableInfo*>& res)
{
	return false;
}

bool DbInterfaceMySQL::getColumns(string schema, string table, vector<ColumnInfo*>& columns)
{
	return false;
}

bool DbInterfaceMySQL::getIndexes(string schema, string tabl, vector<IndexInfo*>& idxs)
{
	return false;
}
