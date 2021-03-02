#pragma once

#include <string>
#include <vector>

#include "ISchemaManager.h"



class IDbManagerInterface
{
public:
	virtual bool getSchemas(std::vector<SchemaInfo*>& res) = 0;
	virtual bool getTables(std::string table, std::vector<TableInfo*>& res) = 0;
	virtual bool getColumns(std::string schema, std::string table, std::vector<ColumnInfo*>& columns) = 0;
	virtual bool getIndexes(std::string schema, std::string tabl, std::vector<IndexInfo*>& idxs) = 0;
};