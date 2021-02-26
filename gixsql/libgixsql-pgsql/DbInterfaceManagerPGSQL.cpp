#include "DbInterfacePGSQL.h"


bool DbInterfacePGSQL::getSchemas(vector<SchemaInfo*>& res)
{
	return false;
}

bool DbInterfacePGSQL::getTables(string table, vector<TableInfo*>& res)
{
	return false;
}

bool DbInterfacePGSQL::getColumns(string schema, string table, vector<ColumnInfo*>& columns)
{
	return false;
}

bool DbInterfacePGSQL::getIndexes(string schema, string tabl, vector<IndexInfo*>& idxs)
{
	return false;
}