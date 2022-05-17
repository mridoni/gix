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

#include <string>
#include <vector>
#include <map>

#if defined(_WIN32) || defined(_WIN64)

#if defined(__MINGW32__)
typedef unsigned char byte;
#endif

#endif

extern "C" {
#include <mysql.h>
}

#include "ILogger.h"
#include "ICursor.h"
#include "IDbInterface.h"
#include "IDbManagerInterface.h"
#include "IDataSourceInfo.h"
#include "ISchemaManager.h"

using namespace std;

class MySQLCursorData
{
public:
	MySQLCursorData();
	~MySQLCursorData();

	MYSQL_STMT* cursor_stmt = nullptr;
	vector<char*> data_buffers;
	vector<int> data_buffer_lengths;
	vector<unsigned long *> data_lengths;

	bool init();
	void clear();

	int getColumnCount();

private:
	void clear_buffers();
};


class DbInterfaceMySQL : public IDbInterface, public IDbManagerInterface
{
public:
	DbInterfaceMySQL();
	~DbInterfaceMySQL();

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
	virtual int cursor_open(ICursor *) override;
	virtual int fetch_one(ICursor *, int) override;
	virtual bool get_resultset_value(ICursor*, int, int, char* bfr, int bfrlen, int *value_len) override;
	virtual int move_to_first_record() override;
	virtual int supports_num_rows() override;
	virtual int get_num_rows() override;
	virtual int get_num_fields() override;
	virtual char *get_error_message() override;
	virtual int get_error_code() override;
	virtual std::string get_state() override;
	virtual void set_owner(IConnection *) override;
	virtual IConnection* get_owner() override;
	virtual int prepare(std::string stmt_name, std::string sql) override;
	virtual int exec_prepared(std::string stmt_name, std::vector<std::string> &paramValues, std::vector<int> paramLengths, std::vector<int> paramFormats) override;

	virtual bool getSchemas(vector<SchemaInfo *>& res) override;
	virtual bool getTables(string table, vector<TableInfo*>& res) override;
	virtual bool getColumns(string schema, string table, vector<ColumnInfo*>& columns) override;
	virtual bool getIndexes(string schema, string tabl, vector<IndexInfo*> &idxs) override;

private:
	MYSQL *connaddr;
	
	MySQLCursorData cur_crsr;

	IConnection *owner;
#if _DEBUG
	ILogger* logger;
#endif

	int last_rc;
	string last_error;
	string last_state;

	map<string, ICursor*> _declared_cursors;

	int _mysql_exec_params(ICursor*, const string, int, int*, vector<string>&, int*, int*);
	int _mysql_exec(ICursor*, const string);

	const char * retrieve_mysql_error_message(MYSQL_STMT* stmt = NULL);
	int retrieve_mysql_error_code(MYSQL_STMT* stmt = NULL);

};

