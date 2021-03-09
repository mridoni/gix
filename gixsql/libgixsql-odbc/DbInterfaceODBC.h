/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 2.1,
* or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; see the file COPYING.LIB.  If
* not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
* Boston, MA 02110-1301 USA
*/

#pragma once

#include <string>
#include <vector>
#include <map>

#if defined(_WIN32) || defined(_WIN64)

#if defined(__MINGW32__)
typedef unsigned char byte;
#endif

#include <windows.h>
#endif
#include <sqlext.h>

#include "ILogger.h"
#include "ICursor.h"
#include "IDbInterface.h"
#include "IDbManagerInterface.h"
#include "IConnectionString.h"
#include "ISchemaManager.h"

using namespace std;

class DbInterfaceODBC : public IDbInterface, public IDbManagerInterface
{
public:
	DbInterfaceODBC();
	~DbInterfaceODBC();

	virtual int init(ILogger *) override;
	virtual int connect(IConnectionString *, int, string) override;
	virtual int reset() override;
	virtual int terminate_connection() override;
	virtual int begin_transaction() override;
	virtual int end_transaction(string) override;
	virtual int exec(string) override;
	virtual int exec_params(string, int, int *, vector<string>&, int *, int *, int) override;
	virtual int close_cursor(ICursor *) override;
	virtual int cursor_declare(ICursor *, bool, int) override;
	virtual int cursor_declare_with_params(ICursor *, char **, bool, int) override;
	virtual int cursor_open(ICursor *) override;
	virtual int fetch_one(ICursor *, int) override;
	virtual bool get_resultset_value(ICursor*, int, int, char* bfr, int bfrlen) override;
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

#if _DEBUG
	ILogger* logger;
#endif

	void retrieve_odbc_error(int, SQLHANDLE stmt = 0);
	int cobol2odbctype(int);
	int cobol2ctype(int);
	int get_data_len(SQLHANDLE hStmt, int cnum);

	SQLHANDLE env_handle;
	SQLHANDLE conn_handle;
	SQLHANDLE cur_stmt_handle;

	vector<string> odbc_errors;

	IConnection *owner;
	int last_rc;

	int driver_has_num_rows_support;
	bool dynamic_cursor_emulation;
	string rowid_col_name;
	char current_rowid_val[128];

	map<string, ICursor *> _declared_cursors;

	int _odbc_exec_params(ICursor *, const string, int, int*, vector<string>&, int*, int*, int);
	int _odbc_exec(ICursor*, const string);
};

