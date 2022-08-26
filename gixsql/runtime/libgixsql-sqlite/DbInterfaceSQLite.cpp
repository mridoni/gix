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

#include "DbInterfaceSQLite.h"

#include <cstring>
#include "IConnection.h"
#include "Logger.h"
#include "utils.h"

#define DEFAULT_CURSOR_ARRAYSIZE	100

static std::string __get_trimmed_hostref_or_literal(void* data, int l);
static std::string sqlite_fixup_parameters(const std::string& sql);

DbInterfaceSQLite::DbInterfaceSQLite()
{}


DbInterfaceSQLite::~DbInterfaceSQLite()
{

}

int DbInterfaceSQLite::init(const std::shared_ptr<spdlog::logger>& _logger)
{
	owner = NULL;
	connaddr = NULL;
	current_statement_data = NULL;
	last_rc = 0;

	auto lib_sink = _logger->sinks().at(0);
	lib_logger = std::make_shared<spdlog::logger>("libgixsql-sqlite", lib_sink);
	lib_logger->set_level(_logger->level());
	lib_logger->info("libgixsql-sqlite logger started");

	return DBERR_NO_ERROR;
}

int DbInterfaceSQLite::connect(IDataSourceInfo* conn_info, IConnectionOptions* opts)
{
	sqlite3* conn;
	std::string connstr;
	char* err_msg = 0;

	connaddr = nullptr;
	current_statement_data = nullptr;

	//connstr = (conn_info->getDbName().empty() ? "" : "dbname=" + conn_info->getDbName() + " ") +
	//	(conn_info->getHost().empty() ? "" : "host=" + conn_info->getHost() + " ") +
	//	(conn_info->getPort() == 0 ? "" : "port=" + std::to_string(conn_info->getPort()) + " ") +
	//	(conn_info->getUsername().empty() ? "" : "user=" + conn_info->getUsername() + " ") +
	//	(conn_info->getPassword().empty() ? "" : "password=" + conn_info->getPassword() + " ");

	int sqlite_rc = sqlite3_open(conn_info->getHost().c_str(), &conn);

	if (sqlite_rc != SQLITE_OK || conn == NULL) {
		return DBERR_CONNECTION_FAILED;
	}

	if (!opts->client_encoding.empty()) {
		std::string sql = "PRAGMA encoding = '" + opts->client_encoding + "';";
		sqlite_rc = sqlite3_exec(conn, sql.c_str(), 0, 0, &err_msg);
		if (sqlite_rc != SQLITE_OK) {
			sqlite3_close(conn);
			return DBERR_CONNECTION_FAILED;
		}
	}

	auto driver_opts = conn_info->getOptions();
	//if (opts.find("default_schema") != opts.end()) {
	//	std::string default_schema = opts["default_schema"];
	//	if (!default_schema.empty()) {
	//		std::string default_schema = opts["default_schema"];
	//		std::string spq = "set search_path to " + default_schema;
	//		auto r = PQexec(conn, spq.c_str());
	//		auto rc = PQresultStatus(r);
	//		if (rc != PGRES_COMMAND_OK) {
	//			last_rc = rc;
	//			last_error = PQresultErrorMessage(r);
	//			LOG_ERROR("%s\n", last_error);
	//			PQfinish(conn);
	//			return DBERR_CONNECTION_FAILED;
	//		}
	//	}
	//}

	//if (opts.find("decode_binary") != opts.end()) {
	//	std::string opt_decode_binary = opts["decode_binary"];
	//	if (!opt_decode_binary.empty()) {
	//		if (opt_decode_binary == "on" || opt_decode_binary == "1") {
	//			this->decode_binary = DECODE_BINARY_ON;
	//		}

	//		if (opt_decode_binary == "off" || opt_decode_binary == "0") {
	//			this->decode_binary = DECODE_BINARY_OFF;
	//		}
	//	}
	//}

	connaddr = conn;
	sqlite3_extended_result_codes(connaddr, 1);

	if (owner)
		owner->setOpened(true);

	return DBERR_NO_ERROR;
}

int DbInterfaceSQLite::reset()
{
	int rc = terminate_connection();
	if (rc == DBERR_NO_ERROR)
		return DBERR_NO_ERROR;
	else
		return DBERR_CONN_RESET_FAILED;
}

int DbInterfaceSQLite::terminate_connection()
{
	//for (auto it = _prepared_stmts.begin(); it != _prepared_stmts.end(); ++it) {
	//	if (it->second) {
	//		delete it->second;
	//		it->second = nullptr;
	//	}
	//}

	//for (auto it = _declared_cursors.begin(); it != _declared_cursors.end(); ++it) {
	//	// TODO: what?
	//}

	//if (current_statement_data) {
	//	delete current_statement_data;
	//	current_statement_data = nullptr;
	//}

	if (connaddr) {
		sqlite3_close(connaddr);
		connaddr = nullptr;
	}

	if (owner)
		owner->setOpened(false);

	return DBERR_NO_ERROR;
}

int DbInterfaceSQLite::begin_transaction()
{
	int rc = exec("BEGIN TRANSACTION");
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_BEGIN_TX_FAILED;
}

int DbInterfaceSQLite::end_transaction(std::string completion_type)
{
	if (completion_type != "COMMIT" && completion_type != "ROLLBACK")
		return DBERR_END_TX_FAILED;

	int rc = exec(completion_type);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_END_TX_FAILED;
}

char* DbInterfaceSQLite::get_error_message()
{
	return (char*)last_error.c_str();
}

int DbInterfaceSQLite::get_error_code()
{
	return last_rc;
}

std::string DbInterfaceSQLite::get_state()
{
	return last_state;
}

void DbInterfaceSQLite::set_owner(IConnection* conn)
{
	owner = conn;
}

IConnection* DbInterfaceSQLite::get_owner()
{
	return owner;
}

std::string vector_join(const std::vector<std::string>& v, char sep)
{
	std::string s;

	for (std::vector< std::string>::const_iterator p = v.begin();
		p != v.end(); ++p) {
		s += *p;
		if (p != v.end() - 1)
			s += sep;
	}
	return s;
}

int DbInterfaceSQLite::prepare(std::string stmt_name, std::string sql)
{
	std::string prepared_sql;
	SQLiteStatementData* res = new SQLiteStatementData();

	stmt_name = to_lower(stmt_name);

	lib_logger->trace(FMT_FILE_FUNC "SQLite::prepare ({}) - SQL: {}", __FILE__, __func__, stmt_name, sql);

	if (this->_prepared_stmts.find(stmt_name) != _prepared_stmts.end()) {
		return DBERR_PREPARE_FAILED;
	}

	if (this->owner->getConnectionOptions()->fixup_parameters) {
		prepared_sql = sqlite_fixup_parameters(sql);
		lib_logger->trace(FMT_FILE_FUNC "SQLite::fixup parameters is on", __FILE__, __func__);
		lib_logger->trace(FMT_FILE_FUNC "SQLite::prepare ({}) - SQL(P): {}", __FILE__, __func__, stmt_name, prepared_sql);
	}
	else {
		prepared_sql = sql;
	}

	lib_logger->trace(FMT_FILE_FUNC "SQLite::prepare ({}) - SQL(P): {}", __FILE__, __func__, stmt_name, prepared_sql);

	int rc = sqlite3_prepare_v2(connaddr, sql.c_str(), sql.size(), &res->statement, nullptr);
	if (sqliteRetrieveError(rc) != SQLITE_OK) {
		lib_logger->error(FMT_FILE_FUNC "SQLite::prepare ({} - res: ({}) {}", __FILE__, __func__, stmt_name, last_rc, last_error);
		return DBERR_PREPARE_FAILED;
	}

	sqliteClearError();

	lib_logger->trace(FMT_FILE_FUNC "SQLite::prepare ({} - res: ({}) {}", __FILE__, __func__, stmt_name, last_rc, last_error);

	_prepared_stmts[stmt_name] = res;

	return DBERR_NO_ERROR;
}


int DbInterfaceSQLite::exec_prepared(std::string stmt_name, std::vector<std::string>& paramValues, std::vector<int> paramLengths, std::vector<int> paramFormats)
{

	lib_logger->trace(FMT_FILE_FUNC "statement name: {}", __FILE__, __func__, stmt_name);

	stmt_name = to_lower(stmt_name);

	if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end()) {
		lib_logger->error("Statement not found: {}", stmt_name);
		return DBERR_SQL_ERROR;
	}

	int nParams = (int)paramValues.size();

	SQLiteStatementData* wk_rs = _prepared_stmts[stmt_name];
	wk_rs->resizeParams(nParams);

	sqlite3_reset(wk_rs->statement);
	sqlite3_clear_bindings(wk_rs->statement);

	for (int i = 0; i < nParams; i++) {

		int rc = sqlite3_bind_text(wk_rs->statement, i + 1, paramValues.at(i).c_str(), paramValues.at(i).size(), NULL);
		if (sqliteRetrieveError(rc) != SQLITE_OK)
			return DBERR_SQL_ERROR;
	}

	int step_rc = sqlite3_step(wk_rs->statement);
	if (step_rc != SQLITE_DONE && step_rc != SQLITE_ROW) {
		int rc = sqliteRetrieveError(step_rc);
		return DBERR_SQL_ERROR;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceSQLite::exec(std::string query)
{
	return _sqlite_exec(nullptr, query);
}

int DbInterfaceSQLite::_sqlite_exec(ICursor* crsr, std::string query, SQLiteStatementData* prep_stmt_data)
{
	int rc = 0;
	uint32_t nquery_cols = 0;
	std::string q = query;
	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#", __FILE__, __func__, q);

	SQLiteStatementData* wk_rs = nullptr;
	
	if (!prep_stmt_data) {
		wk_rs = (SQLiteStatementData*)((crsr != NULL) ? crsr->getPrivateData() : current_statement_data);

		if (wk_rs) {
			if (wk_rs && wk_rs == current_statement_data) {
				delete current_statement_data;
				current_statement_data = nullptr;
			}
		}

		wk_rs = new SQLiteStatementData();
		rc = sqlite3_prepare_v2(connaddr, query.c_str(), query.size(), &wk_rs->statement, nullptr);
		if (sqliteRetrieveError(rc) != SQLITE_OK) {
			return DBERR_SQL_ERROR;
		}
	}
	else {
		wk_rs = prep_stmt_data;	// Already prepared
	}

	int step_rc = sqlite3_step(wk_rs->statement);
	if (step_rc != SQLITE_DONE && step_rc != SQLITE_ROW) {
		rc = sqliteRetrieveError(step_rc);
		return DBERR_SQL_ERROR;
	}

	if (crsr) {
		if (crsr->getPrivateData())
			delete (SQLiteStatementData *) crsr->getPrivateData();

		crsr->setPrivateData(wk_rs);
	}
	else {
		current_statement_data = wk_rs;
	}

	return DBERR_NO_ERROR;
}


int DbInterfaceSQLite::exec_params(std::string query, int nParams, const std::vector<int>& paramTypes, const std::vector<std::string>& paramValues, const std::vector<int>& paramLengths, const std::vector<int>& paramFormats)
{
	return _sqlite_exec_params(nullptr, query, nParams, paramTypes, paramValues, paramLengths, paramFormats);
}

int DbInterfaceSQLite::_sqlite_exec_params(ICursor* crsr, std::string query, int nParams, const std::vector<int>& paramTypes, const std::vector<std::string>& paramValues, const std::vector<int>& paramLengths, const std::vector<int>& paramFormats, SQLiteStatementData* prep_stmt_data)
{
	std::string q = query;
	int rc = 0;

	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#", __FILE__, __func__, q);

	SQLiteStatementData* wk_rs = nullptr;

	if (!prep_stmt_data) {
		wk_rs = (SQLiteStatementData*)((crsr != NULL) ? crsr->getPrivateData() : current_statement_data);

		if (wk_rs) {
			if (wk_rs == current_statement_data) {
				delete current_statement_data;
				current_statement_data = nullptr;
			}
		}

		wk_rs = new SQLiteStatementData();
		rc = sqlite3_prepare_v2(connaddr, query.c_str(), query.size(), &wk_rs->statement, nullptr);
		if (sqliteRetrieveError(rc) != SQLITE_OK) {
			return DBERR_SQL_ERROR;
		}
	}
	else {
		wk_rs = prep_stmt_data;	// Already prepared
	}

	for (int i = 0; i < nParams; i++) {

		int rc = sqlite3_bind_text(wk_rs->statement, i + 1, paramValues.at(i).c_str(), paramValues.at(i).size(), NULL);
		if (sqliteRetrieveError(rc))
			return DBERR_SQL_ERROR;
	}


	int step_rc = sqlite3_step(wk_rs->statement);
	if (step_rc != SQLITE_DONE && step_rc != SQLITE_ROW) {
		rc = sqliteRetrieveError(step_rc);
		return DBERR_SQL_ERROR;
	}

	if (crsr) {
		if (crsr->getPrivateData())
			delete (SQLiteStatementData*) crsr->getPrivateData();

		crsr->setPrivateData(wk_rs);
	}
	else {
		current_statement_data = wk_rs;
		
	}

	return DBERR_NO_ERROR;
}

bool DbInterfaceSQLite::is_cursor_from_prepared_statement(ICursor* cursor)
{
	//std::string squery = cursor->getQuery();
	//void* src_addr = nullptr;
	//int src_len = 0;

	//if (squery.size() == 0) {
	//	cursor->getQuerySource(&src_addr, &src_len);
	//	squery = __get_trimmed_hostref_or_literal(src_addr, src_len);
	//}

	//trim(squery);
	//squery = to_lower(squery);

	//return squery.size() > 1 && starts_with(squery, "@") && _prepared_stmts.find(squery.substr(1)) != _prepared_stmts.end();
	return false;
}

int DbInterfaceSQLite::close_cursor(ICursor* cursor)
{
	//if (!cursor) {
	//	lib_logger->error("Invalid cursor reference");
	//	return DBERR_CLOSE_CURSOR_FAILED;
	//}

	//// Prepared statements used for cursors will be disposed separately
	//if (!is_cursor_from_prepared_statement(cursor)) {
	//	SQLiteStatementData* dp = (SQLiteStatementData*)cursor->getPrivateData();

	//	if (!dp || !dp->statement)
	//		return DBERR_CLOSE_CURSOR_FAILED;

	//	int rc = dpiStmt_release(dp->statement);
	//	dp->statement = nullptr;
	//	if (dpiRetrieveError(rc) != DPI_SUCCESS) {
	//		return DBERR_CLOSE_CURSOR_FAILED;
	//	}

	//	delete dp;
	//}

	//cursor->setPrivateData(nullptr);
	//cursor->setOpened(false);

	return DBERR_NO_ERROR;
}

int DbInterfaceSQLite::cursor_declare(ICursor* cursor, bool with_hold, int res_type)
{
	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceSQLite::cursor_declare_with_params(ICursor* cursor, char** param_values, bool with_hold, int res_type)
{
	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceSQLite::cursor_open(ICursor* cursor)
{
	int rc = 0;

	if (!cursor)
		return DBERR_OPEN_CURSOR_FAILED;

	std::string sname = cursor->getName();
	std::string full_query;
	std::vector<int> empty;

	std::string squery = cursor->getQuery();
	void* src_addr = nullptr;
	int src_len = 0;

	if (squery.size() == 0) {
		cursor->getQuerySource(&src_addr, &src_len);
		squery = __get_trimmed_hostref_or_literal(src_addr, src_len);
	}

	SQLiteStatementData* prepared_stmt_data = nullptr;
	if (starts_with(squery, "@")) {
		if (!retrieve_prepared_statement(squery.substr(1), &prepared_stmt_data)) {
			// last_error, etc. set by retrieve_prepared_statement_source
			return DBERR_OPEN_CURSOR_FAILED;
		}
	}

	if (squery.empty()) {
		last_rc = -1;
		last_error = "Empty query";
		return DBERR_OPEN_CURSOR_FAILED;
	}

	if (cursor->getNumParams() > 0) {
		std::vector<std::string> params = cursor->getParameterValues();
		std::vector<int> param_types = cursor->getParameterTypes();
		std::vector<int> param_lengths = cursor->getParameterLengths();
		rc = _sqlite_exec_params(cursor, squery, cursor->getNumParams(), param_types, params, param_lengths, param_types, prepared_stmt_data);
	}
	else {
		rc = _sqlite_exec(cursor, squery, prepared_stmt_data);
	}

	if (sqliteRetrieveError(rc) == SQLITE_OK) {
		cursor->setOpened(true);

		// SQLite automatically positions it on the first row, so we need a mechanism to avoid fetching (and skipping) it
		SQLiteStatementData* wk_rs = (SQLiteStatementData*)cursor->getPrivateData();
		wk_rs->_on_first_row = true;

		return DBERR_NO_ERROR;
	}
	else {
		cursor->setOpened(false);
		return DBERR_OPEN_CURSOR_FAILED;
	}
}

int DbInterfaceSQLite::fetch_one(ICursor* cursor, int fetchmode)
{
	lib_logger->trace(FMT_FILE_FUNC "mode: {}", __FILE__, __func__, FETCH_NEXT_ROW);

	if (!owner) {
		lib_logger->error("Invalid connection reference");
		return DBERR_CONN_NOT_FOUND;
	}

	if (!cursor) {
		lib_logger->error("Invalid cursor reference");
		return DBERR_FETCH_ROW_FAILED;
	}

	lib_logger->trace(FMT_FILE_FUNC "owner id: {}, cursor name: {}, mode: {}", __FILE__, __func__, owner->getId(), cursor->getName(), FETCH_NEXT_ROW);

	SQLiteStatementData* dp = (SQLiteStatementData*)cursor->getPrivateData();

	if (!dp || !dp->statement)
		return DBERR_FETCH_ROW_FAILED;

	// SQLite automatically positions it on the first row, so we need a mechanism to avoid fetching (and skipping) it
	if (dp->_on_first_row) {
		dp->_on_first_row = false;
	}
	else {
		int step_rc = sqlite3_step(dp->statement);
		if (step_rc != SQLITE_DONE && step_rc != SQLITE_ROW) {
			sqliteRetrieveError(step_rc);
			return DBERR_SQL_ERROR;
		}

		if (step_rc == SQLITE_DONE)
			return DBERR_NO_DATA;
	}

	return DBERR_NO_ERROR;
}

bool DbInterfaceSQLite::get_resultset_value(ResultSetContextType resultset_context_type, void* context, int row, int col, char* bfr, int bfrlen, int* value_len)
{
	int rc = 0;
	SQLiteStatementData* wk_rs = nullptr;

	switch (resultset_context_type) {
		case ResultSetContextType::CurrentResultSet:
			wk_rs = current_statement_data;
			break;

		case ResultSetContextType::PreparedStatement:
		{
			if (!context)
				return false;

			std::string stmt_name = (char*)context;
			stmt_name = to_lower(stmt_name);
			if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end()) {
				lib_logger->error("Invalid prepared statement name: {}", stmt_name);
				return false;
			}

			wk_rs = (SQLiteStatementData*)_prepared_stmts[stmt_name];
		}
		break;

		case ResultSetContextType::Cursor:
		{
			ICursor* c = (ICursor*)context;
			if (!c) {
				lib_logger->error("Invalid cursor reference");
				return false;
			}
			wk_rs = (SQLiteStatementData*)c->getPrivateData();
		}
		break;
	}

	if (!wk_rs) {
		lib_logger->error("Invalid resultset");
		return false;
	}

	const unsigned char *c = sqlite3_column_text(wk_rs->statement, col);
	int l = strlen((const char *)c);

	if (l > bfrlen) {
		return false;
	}

	*value_len = l;
	memcpy(bfr, c, l);
	bfr[l] = '\0';

	return true;
}

bool DbInterfaceSQLite::move_to_first_record(std::string stmt_name)
{
	SQLiteStatementData* dp = nullptr;

	if (stmt_name.empty()) {
		if (!current_statement_data) {
			sqliteSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
			return DBERR_MOVE_TO_FIRST_FAILED;
		}

		dp = current_statement_data;
	}
	else {
		stmt_name = to_lower(stmt_name);
		if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end()) {
			sqliteSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
			return false;
		}
		dp = _prepared_stmts[stmt_name];
	}

	if (!dp || !dp->statement) {
		sqliteSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
		return false;
	}

	int nrows = sqlite3_data_count(dp->statement);
	if (nrows <= 0) {
		sqliteSetError(DBERR_NO_DATA, "02000", "No data");
		return false;
	}
	
	return true;
}

int DbInterfaceSQLite::supports_num_rows()
{
	return 0;
}

int DbInterfaceSQLite::get_num_rows(ICursor* crsr)
{
	//dpiStmt* wk_rs = nullptr;

	//if (crsr) {
	//	SQLiteStatementData* p = (SQLiteStatementData*)crsr->getPrivateData();
	//	wk_rs = p->statement;
	//}
	//else {
	//	if (!current_statement_data)
	//		return -1;

	//	wk_rs = current_statement_data->statement;
	//}

	//if (wk_rs)
	//	return wk_rs->rowCount;
	//else
		return -1;
}

//int DbInterfaceSQLite::has_data(ResultSetContextType resultset_context_type, void* context)
//{
//	SQLiteStatementData* wk_rs = nullptr;
//
//	switch (resultset_context_type) {
//		case ResultSetContextType::CurrentResultSet:
//			wk_rs = current_statement_data;
//			break;
//
//		case ResultSetContextType::PreparedStatement:
//		{
//			if (!context)
//				return false;
//
//			std::string stmt_name = (char*)context;
//			stmt_name = to_lower(stmt_name);
//			if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end()) {
//				lib_logger->error("Invalid prepared statement name: {}", stmt_name);
//				return DBERR_SQL_ERROR;
//			}
//
//			wk_rs = (SQLiteStatementData*)_prepared_stmts[stmt_name];
//		}
//		break;
//
//		case ResultSetContextType::Cursor:
//		{
//			ICursor* c = (ICursor*)context;
//			if (!c) {
//				lib_logger->error("Invalid cursor reference");
//				return DBERR_SQL_ERROR;
//			}
//			wk_rs = (SQLiteStatementData*)c->getPrivateData();
//		}
//		break;
//	}
//
//	if (!wk_rs) {
//		lib_logger->error("Invalid resultset");
//		return DBERR_SQL_ERROR;
//	}
//
//	return sqlite3_data_count(wk_rs->statement);
//}

int DbInterfaceSQLite::get_num_fields(ICursor* crsr)
{
	sqlite3_stmt* wk_rs = nullptr;

	if (crsr) {
		SQLiteStatementData* p = (SQLiteStatementData*)crsr->getPrivateData();
		wk_rs = p->statement;
	}
	else {
		if (!current_statement_data)
			return -1;

		wk_rs = current_statement_data->statement;
	}

	if (wk_rs) {
		return sqlite3_column_count(wk_rs);
	}
	else
		return -1;
}

int DbInterfaceSQLite::_sqlite_get_num_rows(sqlite3_stmt* r)
{
	//uint64_t res = 0;
	//if (!r)
	//	return -1;

	//int rc = dpiStmt_getRowCount(r, &res);
	//if (dpiRetrieveError(rc))
	//	return -1;

	//return (int)res;
	return -1;
}

bool DbInterfaceSQLite::retrieve_prepared_statement(const std::string& prep_stmt_name, SQLiteStatementData** prepared_stmt_data)
{
	std::string stmt_name = to_lower(prep_stmt_name);
	if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end() || _prepared_stmts[stmt_name] == nullptr || _prepared_stmts[stmt_name]->statement == nullptr)
		return false;

	*prepared_stmt_data = _prepared_stmts[stmt_name];
	return true;
}

int DbInterfaceSQLite::sqliteRetrieveError(int rc)
{
	char bfr[1024];

	if (rc != SQLITE_OK) {
		int ext_err = sqlite3_extended_errcode(connaddr);
		const char* err_msg = sqlite3_errmsg(connaddr);
		sprintf(bfr, "ERROR: %s", err_msg);

		last_error = bfr;
		last_rc = (rc > 0) ? -rc : rc;

		last_state = "HY000";	// TODO: fix this
	}
	else {
		sqliteClearError();
	}

	return rc;
}

void DbInterfaceSQLite::sqliteClearError()
{
	last_error = "";
	last_rc = DBERR_NO_ERROR;
	last_state = "00000";
}

void DbInterfaceSQLite::sqliteSetError(int err_code, std::string sqlstate, std::string err_msg)
{
	last_error = err_msg;
	last_rc = err_code;
	last_state = sqlstate;
}

SQLiteStatementData::SQLiteStatementData()
{
	this->params_count = 0;
	this->coldata_count = 0;
}

SQLiteStatementData::~SQLiteStatementData()
{
	cleanup();
	if (statement) {
		sqlite3_finalize(statement);
		statement = nullptr;
	}
}

void SQLiteStatementData::resizeParams(int n)
{
	cleanup();

	params_count = n;
	//this->params = new dpiVar * [n];
	//this->params_bfrs = new dpiData * [n];

}

void SQLiteStatementData::resizeColumnData(int n)
{
	coldata_count = n;
	//this->coldata = new dpiVar * [n];
	//this->coldata_bfrs = new dpiData * [n];
}

void SQLiteStatementData::cleanup()
{
	//if (params) {
	//	for (int i = 0; i < params_count; i++) {
	//		dpiVar_release(params[i]);
	//		params[i] = nullptr;
	//	}
	//	delete[] params;
	//	params = nullptr;
	//}

	//if (coldata) {
	//	for (int i = 0; i < coldata_count; i++) {
	//		dpiVar_release(coldata[i]);
	//		coldata[i] = nullptr;
	//	}
	//	delete[] coldata;
	//	coldata = nullptr;
	//}

	params_count = 0;
	coldata_count = 0;
}

static std::string __get_trimmed_hostref_or_literal(void* data, int l)
{
	if (!data)
		return std::string();

	if (!l)
		return std::string((char*)data);

	if (l > 0) {
		std::string s = std::string((char*)data, l);
		return trim_copy(s);
	}

	// variable-length fields (negative length)
	void* actual_data = (char*)data + VARLEN_LENGTH_SZ;
	VARLEN_LENGTH_T* len_addr = (VARLEN_LENGTH_T*)data;
	int actual_len = (*len_addr);

	// Should we check the actual length against the defined length?
	//...

	std::string t = std::string((char*)actual_data, (-l) - VARLEN_LENGTH_SZ);
	return trim_copy(t);
}

static std::string sqlite_fixup_parameters(const std::string& sql)
{
#if 0
	int n = 1;
	bool in_single_quoted_string = false;
	bool in_double_quoted_string = false;
	std::string out_sql;

	for (auto itc = sql.begin(); itc != sql.end(); ++itc) {
		char c = *itc;

		switch (c) {
		case '"':
			out_sql += c;
			in_double_quoted_string = !in_double_quoted_string;
			continue;

		case '\'':
			out_sql += c;
			in_single_quoted_string = !in_single_quoted_string;
			continue;

		case '$':	// :1 is valid in SQLite, so we just change the prefix
			out_sql += ':';
			continue;

		case '?':
			if (!in_single_quoted_string && !in_double_quoted_string)
				out_sql += (":" + std::to_string(n++));
			else
				out_sql += c;
			continue;

		default:
			out_sql += c;

		}
	}

	return out_sql;
#else
	return sql;
#endif
}
