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

#include <string>
#include <vector>
#include <map>
#include <libpq-fe.h>

#include "ILogger.h"
#include "ICursor.h"
#include "IDbInterface.h"
#include "IDbManagerInterface.h"
#include "IDataSourceInfo.h"
#include "ISchemaManager.h"

#define DECODE_BINARY_ON		1
#define DECODE_BINARY_OFF		0
#define DECODE_BINARY_DEFAULT	DECODE_BINARY_ON

using namespace std;

class DbInterfacePGSQL : public IDbInterface, public IDbManagerInterface
{
public:
	DbInterfacePGSQL();
	~DbInterfacePGSQL();

	virtual int init(ILogger *) override;
	virtual int connect(IDataSourceInfo *, int, string) override;
	virtual int reset() override;
	virtual int terminate_connection() override;
	virtual int begin_transaction() override;
	virtual int end_transaction(string) override;
	virtual int exec(string) override;
	virtual int exec_params(std::string query, int nParams, int *paramTypes, vector<string> &paramValues, int *paramLengths, int *paramFormats) override;
	virtual int close_cursor(ICursor *) override;
	virtual int cursor_declare(ICursor *, bool, int) override;
	virtual int cursor_declare_with_params(ICursor *, char **, bool, int) override;
	virtual int cursor_open(ICursor* cursor);
	virtual int fetch_one(ICursor *, int) override;
	virtual bool get_resultset_value(ICursor* c, int row, int col, char* bfr, int bfrlen, int *value_len);
	virtual int move_to_first_record() override;
	virtual int supports_num_rows() override;
	virtual int get_num_rows() override;
	virtual int get_num_fields() override;
	virtual char *get_error_message() override;
	virtual int get_error_code() override;
	virtual void set_owner(IConnection *) override;
	virtual IConnection* get_owner() override;

	virtual bool getSchemas(vector<SchemaInfo*>& res) override;
	virtual bool getTables(string table, vector<TableInfo*>& res) override;
	virtual bool getColumns(string schema, string table, vector<ColumnInfo*>& columns) override;
	virtual bool getIndexes(string schema, string tabl, vector<IndexInfo*>& idxs) override;

private:
	PGconn *connaddr;
	PGresult *resaddr;

	int last_rc;
	string last_error;

	map<string, ICursor*> _declared_cursors;

	int decode_binary = DECODE_BINARY_DEFAULT;
};

