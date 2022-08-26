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
#include <tuple>
#include <libpq-fe.h>

#include "ICursor.h"
#include "IDbInterface.h"
#include "IDbManagerInterface.h"
#include "IDataSourceInfo.h"
#include "ISchemaManager.h"

#define DECODE_BINARY_ON		1
#define DECODE_BINARY_OFF		0
#define DECODE_BINARY_DEFAULT	DECODE_BINARY_ON

struct PGResultSetData {
	PGResultSetData();
	~PGResultSetData();

	PGresult *resultset = nullptr;
	int current_row_index = -1;
	int num_rows = 0;
};

class DbInterfacePGSQL : public IDbInterface, public IDbManagerInterface
{
public:
	DbInterfacePGSQL();
	~DbInterfacePGSQL();

	virtual int init(const std::shared_ptr<spdlog::logger>& _logger) override;
	virtual int connect(IDataSourceInfo *, IConnectionOptions *opts) override;
	virtual int reset() override;
	virtual int terminate_connection() override;
	virtual int begin_transaction() override;
	virtual int end_transaction(std::string) override;
	virtual int exec(std::string) override;
	virtual int exec_params(std::string query, int nParams, const std::vector<int>& paramTypes, const std::vector<std::string>& paramValues, const std::vector<int>& paramLengths, const std::vector<int>& paramFormats) override;
	virtual int close_cursor(ICursor *) override;
	virtual int cursor_declare(ICursor *, bool, int) override;
	virtual int cursor_declare_with_params(ICursor *, char **, bool, int) override;
	virtual int cursor_open(ICursor* cursor);
	virtual int fetch_one(ICursor *, int) override;
	virtual bool get_resultset_value(ResultSetContextType resultset_context_type, void* context, int row, int col, char* bfr, int bfrlen, int *value_len);
	virtual bool move_to_first_record(std::string stmt_name = "") override;
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
	PGconn *connaddr = nullptr;

	PGResultSetData* current_resultset_data = nullptr;

	int last_rc = 0;
	std::string last_error;
	std::string last_state;

	std::map<std::string, ICursor*> _declared_cursors;
	std::map<std::string, PGResultSetData*> _prepared_stmts;

	int decode_binary = DECODE_BINARY_DEFAULT;

	int _pgsql_exec(ICursor *crsr, std::string);
	int _pgsql_exec_params(ICursor* crsr, std::string query, int nParams, const std::vector<int>& paramTypes, const std::vector<std::string>& paramValues, const std::vector<int>& paramLengths, const std::vector<int>& paramFormats);

	bool retrieve_prepared_statement_source(const std::string& prep_stmt_name, std::string& src);

	int get_num_rows(PGresult* r);

	void pgsqlClearError();
	void pgsqlSetError(int err_code, std::string sqlstate, std::string err_msg);

	bool use_native_cursors = true;
};

