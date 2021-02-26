#pragma once

#include <string>
#include <vector>

#include "ISchemaManager.h"

using namespace std;

class IDbManagerInterface
{
public:
	virtual bool getSchemas(vector<SchemaInfo*>& res) = 0;
	virtual bool getTables(string table, vector<TableInfo*>& res) = 0;
	virtual bool getColumns(string schema, string table, vector<ColumnInfo*>& columns) = 0;
	virtual bool getIndexes(string schema, string tabl, vector<IndexInfo*>& idxs) = 0;
};