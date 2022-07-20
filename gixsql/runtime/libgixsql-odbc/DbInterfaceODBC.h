/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 3,
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

#define WIN32_LEAN_AND_MEAN

#include <string>
#include <vector>
#include <map>

#if defined(_WIN32) || defined(_WIN64)

#if defined(__MINGW32__)
typedef unsigned char byte;
#endif

#include <windows.h>
#endif

#include <sql.h>
#include <sqlext.h>


#include "ICursor.h"
#include "IDbInterface.h"
#include "IDbManagerInterface.h"
#include "IDataSourceInfo.h"
#include "ISchemaManager.h"

class DbInterfaceODBC : public IDbInterface, public IDbManagerInterface
{
public:
	DbInterfaceODBC();
	~DbInterfaceODBC();

	virtual int init(const std::shared_ptr<spdlog::logger>& _logger) override;
	virtual int connect(IDataSourceInfo *, int, std::string) override;
	virtual int reset() override;
	virtual int terminate_connection() override;
	virtual int begin_transaction() override;
	virtual int end_transaction(std::string) override;
	virtual int exec(std::string) override;
	virtual int exec_params(std::string query, int nParams, int *paramTypes, std::vector<std::string> &paramValues, int *paramLengths, int *paramFormats) override;
	virtual int close_cursor(ICursor *) override;
	virtual int cursor_declare(ICursor *, bool, int) override;
	virtual int cursor_declare_with_params(ICursor *, char **, bool, int) override;
	virtual int cursor_open(ICursor *) override;
	virtual int fetch_one(ICursor *, int) override;
	virtual bool get_resultset_value(ICursor*, int, int, char* bfr, int bfrlen, int *value_len) override;
	virtual int move_to_first_record() override;
	virtual int supports_num_rows() override;
	virtual int get_num_rows(ICursor* crsr) override;
	virtual int get_num_fields(ICursor* crsr) override;
	virtual char *get_error_message() override;
	virtual int get_error_code() override;
	virtual std::string get_state() override;
	virtual void set_owner(IConnection *) override;
	virtual IConnection* get_owner() override;
	virtual int prepare(std::string stmt_name, std::string sql) override;
	virtual int exec_prepared(std::string stmt_name, std::vector<std::string> &paramValues, std::vector<int> paramLengths, std::vector<int> paramFormats) override;

	virtual bool getSchemas(std::vector<SchemaInfo*>& res) override;
	virtual bool getTables(std::string table, std::vector<TableInfo*>& res) override;
	virtual bool getColumns(std::string schema, std::string table, std::vector<ColumnInfo*>& columns) override;
	virtual bool getIndexes(std::string schema, std::string tabl, std::vector<IndexInfo*>& idxs) override;

private:

	void retrieve_odbc_error(int, SQLHANDLE stmt = 0);
	int cobol2odbctype(int);
	int cobol2ctype(int);
	int get_data_len(SQLHANDLE hStmt, int cnum);

	SQLHANDLE env_handle;
	SQLHANDLE conn_handle;
	SQLHANDLE cur_stmt_handle;

	std::vector<std::string> odbc_errors;

	IConnection *owner;
	int last_rc;
	std::string last_state;

	int driver_has_num_rows_support;
	bool dynamic_cursor_emulation;
	std::string rowid_col_name;
	char current_rowid_val[128];

	std::map<std::string, ICursor *> _declared_cursors;

	int _odbc_exec_params(ICursor *, const std::string, int, int*, std::vector<std::string>&, int*, int*);
	int _odbc_exec(ICursor*, const std::string);
};

