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