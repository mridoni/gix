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

#include "DbInterfaceODBC.h"
#include "SqlVar.h"
#include "IConnection.h"
#include "Logger.h"
#include "utils.h"

#include "cobol_var_types.h"

#include <cstring>


SQLHANDLE DbInterfaceODBC::odbc_global_env_context = nullptr;
int DbInterfaceODBC::odpi_global_env_context_usage_count = 0;

static std::string __get_trimmed_hostref_or_literal(void* data, int l);
static std::string odbc_fixup_parameters(const std::string& sql);

DbInterfaceODBC::DbInterfaceODBC()
{
}


DbInterfaceODBC::~DbInterfaceODBC()
{
	odpi_global_env_context_usage_count--;
	if (odbc_global_env_context && odpi_global_env_context_usage_count == 0) {
		SQLFreeHandle(SQL_HANDLE_ENV, odbc_global_env_context);
		odbc_global_env_context = nullptr;
	}

	if (conn_handle)
		SQLFreeHandle(SQL_HANDLE_DBC, conn_handle);
}



int DbInterfaceODBC::init(const std::shared_ptr<spdlog::logger>& _logger)
{
	conn_handle = NULL;
	owner = NULL;

	auto lib_sink = _logger->sinks().at(0);
	lib_logger = std::make_shared<spdlog::logger>("libgixsql-odbc", lib_sink);
	lib_logger->set_level(_logger->level());
	lib_logger->info("libgixsql-odbc logger started");

	if (!odbc_global_env_context) {
		SQLRETURN rc = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &odbc_global_env_context);
		if (rc != SQL_SUCCESS) {
			lib_logger->debug(FMT_FILE_FUNC "FATAL ERROR: Can't allocate SQL Handle for the ODBC environment", __FILE__, __func__);
			lib_logger->error("FATAL ERROR: Can't allocate SQL Handle for the ODBC environment");
			odbc_global_env_context = NULL;
			return DBERR_OUT_OF_MEMORY;
		}

		// set ODBC3 version but ignore the error
		rc = SQLSetEnvAttr(odbc_global_env_context, SQL_ATTR_ODBC_VERSION, (SQLPOINTER)SQL_OV_ODBC3, 0);
		if (last_rc != SQL_SUCCESS) {
			lib_logger->debug(FMT_FILE_FUNC  "WARNING: Cannot set ODBC version", __FILE__, __func__);
			lib_logger->error("WARNING: Cannot set ODBC version");
		}

		// set NTS if possible to avoid extra moves
		rc = SQLSetEnvAttr(odbc_global_env_context, SQL_ATTR_OUTPUT_NTS, (SQLPOINTER)SQL_FALSE, 0);
		if (last_rc != SQL_SUCCESS) {
			lib_logger->debug(FMT_FILE_FUNC  "WARNING: Cannot set NTS", __FILE__, __func__);
			lib_logger->error("WARNING: Cannot set ODBC NTS");
		}
	}

	odpi_global_env_context_usage_count++;

	int rc = SQLAllocHandle(SQL_HANDLE_DBC, odbc_global_env_context, &conn_handle);
	if (odbcRetrieveError(rc, ErrorSource::Environmennt) != SQL_SUCCESS) {
		lib_logger->debug(FMT_FILE_FUNC  "FATAL ERROR: Can't allocate SQL Handle for the ODBC connection", __FILE__, __func__);
		lib_logger->error("FATAL ERROR: Can't allocate SQL Handle for the ODBC connection");
		conn_handle = NULL;
		return DBERR_OUT_OF_MEMORY;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::connect(IDataSourceInfo* conn_string, IConnectionOptions* opts)
{
	int rc = 0;
	char dbms_name[256];
	std::string host = conn_string->getHost();
	std::string user = conn_string->getUsername();
	std::string pwd = conn_string->getPassword();

	lib_logger->trace(FMT_FILE_FUNC  "ODBC: DB connect to DSN '{}' user = '{}'", __FILE__, __func__, host, user);

	// Connect
	if (!user.empty()) {
		rc = SQLConnect(conn_handle, (SQLCHAR*)host.c_str(), SQL_NTS, (SQLCHAR*)user.c_str(), SQL_NTS, (SQLCHAR*)pwd.c_str(), SQL_NTS);
	}
	else {
		rc = SQLConnect(conn_handle, (SQLCHAR*)host.c_str(), SQL_NTS, 0, 0, 0, 0);
	}

	if (odbcRetrieveError(rc, ErrorSource::Connection) != SQL_SUCCESS) {
		return DBERR_CONNECTION_FAILED;
	}

	if (!opts->autocommit) {
		// try to set AUTOCOMMIT OFF
		rc = SQLSetConnectAttr(conn_handle, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF, 0);
		if (rc != SQL_SUCCESS) {
			lib_logger->debug(FMT_FILE_FUNC  "ODBC: SEVERE ERROR: Can't set autocommit OFF. Error = {}", __FILE__, __func__, last_rc);
		}
	}


	int rc_warning_only = SQLGetInfo(conn_handle, SQL_DBMS_NAME, (SQLPOINTER)dbms_name, sizeof(dbms_name), NULL);
	if (rc_warning_only != SQL_SUCCESS) {
		lib_logger->debug(FMT_FILE_FUNC  "WARNING: Cannot retrieve DBMS name", __FILE__, __func__);
		lib_logger->error("WARNING: Cannot retrieve DBMS name");
	}
	else {
		lib_logger->debug(FMT_FILE_FUNC "DBMS name is [{}]", __FILE__, __func__, dbms_name);
	}

	if (owner)
		owner->setOpened(true);

	lib_logger->debug(FMT_FILE_FUNC  "ODBC: Connection registration successful", __FILE__, __func__);

	odbcClearError();
	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::reset()
{
	lib_logger->trace(FMT_FILE_FUNC  "ODBC: connection reset invoked", __FILE__, __func__);

	int rc = terminate_connection();
	if (rc == DBERR_NO_ERROR)
		return DBERR_NO_ERROR;
	else
		return DBERR_CONN_RESET_FAILED;
}

int DbInterfaceODBC::terminate_connection()
{
	lib_logger->trace(FMT_FILE_FUNC "ODBC: connection termination invoked", __FILE__, __func__);

	int rc = SQLDisconnect(conn_handle);

	if (owner)
		owner->setOpened(false);

	if (odbcRetrieveError(rc, ErrorSource::Connection) != SQL_SUCCESS)
		return DBERR_DISCONNECT_FAILED;

	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::begin_transaction()
{
	lib_logger->debug(FMT_FILE_FUNC  "ODBC: begin transaction invoked", __FILE__, __func__);

	// Nothing to do for ODBC
	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::end_transaction(std::string completion_type)
{
	lib_logger->trace(FMT_FILE_FUNC  "ODBC: end transaction invoked", __FILE__, __func__);

	if (completion_type != "COMMIT" && completion_type != "ROLLBACK")
		return DBERR_END_TX_FAILED;

	if (!current_statement_data || !current_statement_data->statement) {
		lib_logger->error("ODBC: Invalid statement reference");
		return DBERR_END_TX_FAILED;
	}

	SQLSMALLINT sql_completion_type = (completion_type == "COMMIT") ? SQL_COMMIT : SQL_ROLLBACK;
	delete current_statement_data;
	current_statement_data = nullptr;

	int rc = SQLEndTran(SQL_HANDLE_DBC, conn_handle, sql_completion_type);
	if (odbcRetrieveError(rc, ErrorSource::Statement) != SQL_SUCCESS) {
		lib_logger->debug(FMT_FILE_FUNC  "ODBC: Error while ending transaction (2)({}): {}", __FILE__, __func__, completion_type, last_rc);
		lib_logger->error("ODBC: Error while ending transaction (1)({}): {}", completion_type, last_rc);
		return DBERR_END_TX_FAILED;
	}

	std::map<std::string, ICursor*>::iterator it;
	std::vector<ICursor*> cur_to_remove;
	std::vector<ICursor*>::iterator it2;

	for (it = _declared_cursors.begin(); it != _declared_cursors.end(); it++) {
		ICursor* c = (it)->second;
		if (!c->isWithHold())
			cur_to_remove.push_back(c);
	}

	for (it2 = cur_to_remove.begin(); it2 != cur_to_remove.end(); it2++) {
		ICursor* c = (*it2);
		_declared_cursors.erase(c->getName());
		close_cursor(c);
	}

	return DBERR_NO_ERROR;
}

char* DbInterfaceODBC::get_error_message()
{
	return (char*)last_error.c_str();
}

int DbInterfaceODBC::get_error_code()
{
	return last_rc;
}

std::string DbInterfaceODBC::get_state()
{
	return last_state;
}

void DbInterfaceODBC::set_owner(IConnection* conn)
{
	owner = conn;
}

IConnection* DbInterfaceODBC::get_owner()
{
	return owner;
}

int DbInterfaceODBC::prepare(std::string stmt_name, std::string sql)
{
	std::string prepared_sql;
	ODBCStatementData* res = new ODBCStatementData(conn_handle);
	if (!res->statement) {
		delete res;
		return DBERR_PREPARE_FAILED;
	}
	stmt_name = to_lower(stmt_name);

	lib_logger->trace(FMT_FILE_FUNC "ODPI::prepare ({}) - SQL: {}", __FILE__, __func__, stmt_name, sql);

	if (this->_prepared_stmts.find(stmt_name) != _prepared_stmts.end()) {
		return DBERR_PREPARE_FAILED;
	}

	if (this->owner->getConnectionOptions()->fixup_parameters) {
		prepared_sql = odbc_fixup_parameters(sql);
		lib_logger->trace(FMT_FILE_FUNC "ODPI::fixup parameters is on", __FILE__, __func__);
		lib_logger->trace(FMT_FILE_FUNC "ODPI::prepare ({}) - SQL(P): {}", __FILE__, __func__, stmt_name, prepared_sql);
	}
	else {
		prepared_sql = sql;
	}

	lib_logger->trace(FMT_FILE_FUNC "ODPI::prepare ({}) - SQL(P): {}", __FILE__, __func__, stmt_name, prepared_sql);

	int rc = SQLPrepare(res->statement, (SQLCHAR*)prepared_sql.c_str(), SQL_NTS);
	if (odbcRetrieveError(rc, ErrorSource::Statement, res->statement) < 0) {
		lib_logger->error(FMT_FILE_FUNC "ODPI::prepare ({} - res: ({}) {}", __FILE__, __func__, stmt_name, last_rc, last_error);
		return DBERR_PREPARE_FAILED;
	}

	odbcClearError();

	lib_logger->trace(FMT_FILE_FUNC "ODPI::prepare ({} - res: ({}) {}", __FILE__, __func__, stmt_name, last_rc, last_error);

	_prepared_stmts[stmt_name] = res;

	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::exec_prepared(std::string stmt_name, std::vector<std::string>& paramValues, std::vector<int> paramLengths, std::vector<int> paramFormats)
{
	lib_logger->trace(FMT_FILE_FUNC "statement name: {}", __FILE__, __func__, stmt_name);

	stmt_name = to_lower(stmt_name);

	if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end()) {
		lib_logger->error("Statement not found: {}", stmt_name);
		return DBERR_SQL_ERROR;
	}

	int nParams = (int)paramValues.size();

	ODBCStatementData* wk_rs = _prepared_stmts[stmt_name];
	wk_rs->resizeParams(nParams);

	wk_rs->resizeParams(nParams);

	for (int i = 0; i < nParams; i++) {
		SQLLEN len;
		int ptype = cobol2odbctype(paramFormats[i]);
		int ctype = cobol2ctype(paramFormats[i]);

		int rc = SQLBindParameter(wk_rs->statement,
			i + 1,
			SQL_PARAM_INPUT,
			SQL_C_CHAR,
			ptype, // SQL_VARCHAR,
			10,
			0,
			(SQLPOINTER)paramValues.at(i).c_str(),
			(SQLLEN)paramValues.at(i).size(),
			NULL);

		if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
			//free(pvals);
			last_rc = rc;
			lib_logger->error("ODBC: Error while binding parameter {} in prepared statement ({}): {}", i + 1, last_rc, stmt_name);
			return DBERR_SQL_ERROR;
		}
	}

	int rc = SQLExecute(wk_rs->statement);
	if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
		return DBERR_SQL_ERROR;
	}

	return DBERR_NO_ERROR;
}


int DbInterfaceODBC::exec(std::string _query)
{
	return _odbc_exec(nullptr, _query);
}


int DbInterfaceODBC::_odbc_exec(ICursor* crsr, const std::string query, ODBCStatementData* prep_stmt_data)
{
	int rc = 0;
	uint32_t nquery_cols = 0;
	std::string q = query;
	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#", __FILE__, __func__, q);

	ODBCStatementData* wk_rs = nullptr;

	if (!prep_stmt_data) {
		wk_rs = (ODBCStatementData*)((crsr != NULL) ? crsr->getPrivateData() : current_statement_data);

		if (wk_rs && wk_rs == current_statement_data) {
			delete current_statement_data;
			current_statement_data = nullptr;
		}

		wk_rs = new ODBCStatementData(conn_handle);
		if (!wk_rs->statement) {
			delete wk_rs;
			return DBERR_SQL_ERROR;
		}

		rc = SQLPrepare(wk_rs->statement, (SQLCHAR*)query.c_str(), SQL_NTS);
		if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
			return DBERR_SQL_ERROR;
		}
	}
	else {
		wk_rs = prep_stmt_data;	// Already prepared
	}

	rc = SQLExecute(wk_rs->statement);
	if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
		return DBERR_SQL_ERROR;
	}

	if (!prep_stmt_data) {
		q = trim_copy(q);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			int nrows = get_affected_rows(wk_rs);
			if (nrows <= 0) {
				last_rc = NO_REC_CODE_DEFAULT;
				lib_logger->error("ODBC: cannot retrieve the number of affected rows. Reason: ({}): {}", last_rc, last_error);
				return DBERR_SQL_ERROR;
			}
		}
	}

	if (last_rc == SQL_SUCCESS) {
		if (crsr) {
			if (crsr->getPrivateData())
				delete (ODBCStatementData*)crsr->getPrivateData();

			crsr->setPrivateData(wk_rs);
		}
		else
			current_statement_data = wk_rs;

		return DBERR_NO_ERROR;
	}
	else {
		last_rc = -(10000 + last_rc);
		return DBERR_SQL_ERROR;
	}

}

int DbInterfaceODBC::exec_params(std::string query, int nParams, const std::vector<int>& paramTypes, const std::vector<std::string>& paramValues, const std::vector<int>& paramLengths, const std::vector<int>& paramFormats)
{
	return _odbc_exec_params(nullptr, query, nParams, paramTypes, paramValues, paramLengths, paramFormats);
}

int DbInterfaceODBC::_odbc_exec_params(ICursor* crsr, std::string query, int nParams, const std::vector<int>& paramTypes, const std::vector<std::string>& paramValues, const std::vector<int>& paramLengths, const std::vector<int>& paramFormats, ODBCStatementData* prep_stmt_data)
{
	std::string q = query;
	int rc = 0;

	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#", __FILE__, __func__, q);

	ODBCStatementData* wk_rs = nullptr;

	if (!prep_stmt_data) {
		wk_rs = (ODBCStatementData*)((crsr != NULL) ? crsr->getPrivateData() : current_statement_data);

		if (wk_rs && wk_rs == current_statement_data) {
			delete current_statement_data;
			current_statement_data = nullptr;
		}

		wk_rs = new ODBCStatementData(conn_handle);
		if (!wk_rs->statement) {
			delete wk_rs;
			return DBERR_SQL_ERROR;
		}

		rc = SQLPrepare(wk_rs->statement, (SQLCHAR*)query.c_str(), SQL_NTS);
		if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
			return DBERR_SQL_ERROR;
		}
	}
	else {
		wk_rs = prep_stmt_data;	// Already prepared
	}

	wk_rs->resizeParams(nParams);

	for (int i = 0; i < nParams; i++) {
		SQLLEN len;
		int ptype = cobol2odbctype(paramTypes[i]);
		int ctype = cobol2ctype(paramTypes[i]);

		rc = SQLBindParameter(wk_rs->statement,
			i + 1,
			SQL_PARAM_INPUT,
			SQL_C_CHAR,
			ptype, // SQL_VARCHAR,
			10,
			0,
			(SQLPOINTER)paramValues.at(i).c_str(),
			(SQLLEN)paramValues.at(i).size(),
			NULL);

		if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
			//free(pvals);
			last_rc = rc;
			lib_logger->error("ODBC: Error while binding parameter {} in statement ({}): {}", i + 1, last_rc, query);
			return DBERR_SQL_ERROR;
		}
	}

	rc = SQLExecute(wk_rs->statement);
	if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
		lib_logger->error("ODBC: Error while executing statement ({}): {}", last_rc, last_error);
		return DBERR_SQL_ERROR;
	}

	if (!prep_stmt_data) {
		q = trim_copy(q);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			int nrows = get_affected_rows(wk_rs);
			if (nrows <= 0) {
				last_rc = NO_REC_CODE_DEFAULT;
				lib_logger->error("ODBC: cannot retrieve the number of affected rows. Reason: ({}): {}", last_rc, last_error);
				return DBERR_SQL_ERROR;
			}
		}
	}

	if (last_rc == SQL_SUCCESS) {
		if (crsr) {
			if (crsr->getPrivateData())
				delete (ODBCStatementData*)crsr->getPrivateData();

			crsr->setPrivateData(wk_rs);
		}
		else
			current_statement_data = wk_rs;

		return DBERR_NO_ERROR;
	}
	else {
		last_rc = -(10000 + last_rc);
		return DBERR_SQL_ERROR;
	}
}

bool DbInterfaceODBC::is_cursor_from_prepared_statement(ICursor* cursor)
{
	std::string squery = cursor->getQuery();
	void* src_addr = nullptr;
	int src_len = 0;

	if (squery.size() == 0) {
		cursor->getQuerySource(&src_addr, &src_len);
		squery = __get_trimmed_hostref_or_literal(src_addr, src_len);
	}

	trim(squery);
	squery = to_lower(squery);

	return squery.size() > 1 && starts_with(squery, "@") && _prepared_stmts.find(squery.substr(1)) != _prepared_stmts.end();
}


int DbInterfaceODBC::close_cursor(ICursor* cursor)
{
	if (!cursor) {
		lib_logger->error("Invalid cursor reference");
		return DBERR_CLOSE_CURSOR_FAILED;
	}

	// Prepared statements used for cursors will be disposed separately
	if (!is_cursor_from_prepared_statement(cursor)) {
		ODBCStatementData* dp = (ODBCStatementData*)cursor->getPrivateData();

		if (!dp || !dp->statement)
			return DBERR_CLOSE_CURSOR_FAILED;

		SQLHANDLE cursor_handle = dp->statement;

		int rc = SQLCloseCursor(cursor_handle);
		if (odbcRetrieveError(rc, ErrorSource::Statement, cursor_handle) != SQL_SUCCESS) {
			lib_logger->error("ODBC: Error while closing cursor ({}) {}", last_rc, cursor->getName());
			return DBERR_CLOSE_CURSOR_FAILED;
		}

		delete (ODBCStatementData*)cursor->getPrivateData();
		cursor->setPrivateData(nullptr);
		cursor->setOpened(false);

		if (rc != SQL_SUCCESS) {
			lib_logger->error("ODBC: Error while closing cursor ({}) {}", last_rc, cursor->getName());
			return DBERR_CLOSE_CURSOR_FAILED;
		}

	}
	else {
		cursor->setPrivateData(nullptr);
		cursor->setOpened(false);
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::cursor_declare(ICursor* cursor, bool with_hold, int res_type)
{
	lib_logger->trace(FMT_FILE_FUNC "ODBC: cursor declare invoked", __FILE__, __func__);

	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	ODBCStatementData* wk_rs = new ODBCStatementData(conn_handle);
	if (!wk_rs->statement) {
		delete wk_rs;
		return DBERR_DECLARE_CURSOR_FAILED;
	}

	int rc = SQLSetCursorName(wk_rs->statement, (SQLCHAR*)cursor->getName().c_str(), SQL_NTS);
	lib_logger->debug(FMT_FILE_FUNC "ODBC: setting cursor name: [{}]", __FILE__, __func__, cursor->getName());
	if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
		lib_logger->error("ODBC: Error while setting cursor name ({}) {}", last_rc, cursor->getName());
		return DBERR_DECLARE_CURSOR_FAILED;
	}

	cursor->setPrivateData(wk_rs);

	std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::cursor_declare_with_params(ICursor* cursor, char** param_values, bool with_hold, int res_type)
{
	lib_logger->trace(FMT_FILE_FUNC "ODBC: cursor declare (with params) invoked", __FILE__, __func__);

	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	ODBCStatementData* wk_rs = new ODBCStatementData(conn_handle);
	if (!wk_rs->statement) {
		delete wk_rs;
		return DBERR_DECLARE_CURSOR_FAILED;
	}

	int rc = SQLSetCursorName(wk_rs->statement, (SQLCHAR*)cursor->getName().c_str(), SQL_NTS);
	lib_logger->debug(FMT_FILE_FUNC "ODBC: setting cursor name: [{}]", __FILE__, __func__, cursor->getName());
	if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
		lib_logger->error("ODBC: Error while setting cursor name ({}) {}", last_rc, cursor->getName());
		return DBERR_DECLARE_CURSOR_FAILED;
	}

	cursor->setPrivateData(wk_rs);

	std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::cursor_open(ICursor* cursor)
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

	ODBCStatementData* prepared_stmt_data = nullptr;
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
		rc = _odbc_exec_params(cursor, squery, cursor->getNumParams(), param_types, params, param_lengths, param_types, prepared_stmt_data);
	}
	else {
		rc = _odbc_exec(cursor, squery, prepared_stmt_data);
	}

	if (rc == SQL_SUCCESS) {
		cursor->setOpened(true);
		return DBERR_NO_ERROR;
	}
	else {
		cursor->setOpened(false);
		return DBERR_OPEN_CURSOR_FAILED;
	}
}

int DbInterfaceODBC::fetch_one(ICursor* cursor, int fetchmode)
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

	ODBCStatementData* dp = (ODBCStatementData*)cursor->getPrivateData();

	if (!dp || !dp->statement)
		return DBERR_FETCH_ROW_FAILED;

	int rc = SQLFetch(dp->statement);
	if (rc == SQL_NO_DATA)
		return DBERR_NO_DATA;

	if (odbcRetrieveError(rc, ErrorSource::Statement, dp->statement) != SQL_SUCCESS) {
		return DBERR_FETCH_ROW_FAILED;
	}

	return DBERR_NO_ERROR;
}

bool DbInterfaceODBC::get_resultset_value(ResultSetContextType resultset_context_type, void* context, int row, int col, char* bfr, int bfrlen, int* value_len)
{
	int rc = 0;
	ODBCStatementData* wk_rs = nullptr;

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

		wk_rs = (ODBCStatementData*)_prepared_stmts[stmt_name];
	}
	break;

	case ResultSetContextType::Cursor:
	{
		ICursor* c = (ICursor*)context;
		if (!c) {
			lib_logger->error("Invalid cursor reference");
			return false;
		}
		wk_rs = (ODBCStatementData*)c->getPrivateData();
	}
	break;
	}

	if (!wk_rs) {
		lib_logger->error("Invalid resultset");
		return false;
	}

	//SQL_C_BINARY

	SQLLEN reslen = 0;
	bool is_binary = false;
	if (!column_is_binary(wk_rs->statement, col + 1, &is_binary))
		return false;

	if (!is_binary) {
		rc = SQLGetData(wk_rs->statement, col + 1, SQL_C_CHAR, bfr, bfrlen, &reslen);
		*value_len = reslen;
	}
	else {
		unsigned char *tmp_bfr = new unsigned char[bfrlen * 2];
		uint8_t b0, b1;
		rc = SQLGetData(wk_rs->statement, col + 1, SQL_C_CHAR, tmp_bfr, bfrlen * 2, &reslen);
		if (rc == SQL_SUCCESS) {
			for (int i = 0; i < bfrlen; i++) {
				uint8_t b1 = tmp_bfr[i * 2];
				uint8_t b0 = tmp_bfr[(i * 2) + 1];
				if (b0 >= '0' && b0 <= '9') b0 -= '0'; else b0 = (b0 - 'a') + 10;
				if (b1 >= '0' && b1 <= '9') b1 -= '0'; else b1 = (b1 - 'a') + 10;
				bfr[i] = b0 + (b1 << 4);
			}
		}
		delete[] tmp_bfr;
		*value_len = reslen / 2;
	}
	if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs->statement) != SQL_SUCCESS) {
		return false;
	}

#ifdef VERBOSE
	lib_logger->trace(FMT_FILE_FUNC "col: {}, data: {}", __FILE__, __func__, col, std::string(c, l));
	std::string s = fmt::format("col: {}, data: {}", col, std::string(c, l));
	fprintf(stderr, "%s\n", s.c_str());
#endif

	return true;
}

bool DbInterfaceODBC::move_to_first_record(std::string stmt_name)
{
	ODBCStatementData* dp = nullptr;

	lib_logger->trace(FMT_FILE_FUNC "ODBC: moving to first row in resultset", __FILE__, __func__);

	if (stmt_name.empty()) {
		if (!current_statement_data) {
			odbcSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
			return false;
		}

		dp = current_statement_data;
	}
	else {
		stmt_name = to_lower(stmt_name);
		if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end()) {
			odbcSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
			return false;
		}
		dp = _prepared_stmts[stmt_name];
	}

	if (!dp || !dp->statement) {
		odbcSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
		return false;
	}

	int rc = SQLFetch(dp->statement);
	if (rc == SQL_NO_DATA) {
		odbcSetError(DBERR_NO_DATA, "02000", "No data");
		return false;
	}

	if (odbcRetrieveError(rc, ErrorSource::Statement, dp->statement) != SQL_SUCCESS) {
		lib_logger->trace(FMT_FILE_FUNC "ODBC: Error while moving to first row in resultset", __FILE__, __func__);
		odbcSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
		return false;
	}

	return true;
}

int DbInterfaceODBC::supports_num_rows()
{
	return 0;
}

int DbInterfaceODBC::get_num_rows(ICursor* crsr)
{
	return -1;
}

int DbInterfaceODBC::get_num_fields(ICursor* crsr)
{
	SQLHANDLE wk_rs = nullptr;

	if (crsr) {
		ODBCStatementData* p = (ODBCStatementData*)crsr->getPrivateData();
		wk_rs = p->statement;
	}
	else {
		if (!current_statement_data)
			return -1;

		wk_rs = current_statement_data->statement;
	}

	if (wk_rs) {

		SQLSMALLINT NumCols = 0;
		int rc = SQLNumResultCols(wk_rs, &NumCols);
		if (odbcRetrieveError(rc, ErrorSource::Statement, wk_rs) != SQL_SUCCESS) {
			return SQL_ERROR;
		}
		return NumCols;
	}

	return -1;
}

int DbInterfaceODBC::cobol2odbctype(int t)
{
	switch (t) {
	case COBOL_TYPE_UNSIGNED_NUMBER:
	case COBOL_TYPE_SIGNED_NUMBER_TC:
	case COBOL_TYPE_SIGNED_NUMBER_TS:
	case COBOL_TYPE_SIGNED_NUMBER_LC:
	case COBOL_TYPE_SIGNED_NUMBER_LS:
	case COBOL_TYPE_UNSIGNED_NUMBER_PD:
	case COBOL_TYPE_SIGNED_NUMBER_PD:
	case COBOL_TYPE_UNSIGNED_BINARY:
	case COBOL_TYPE_SIGNED_BINARY:
		return SQL_NUMERIC;

	case COBOL_TYPE_ALPHANUMERIC:
	case COBOL_TYPE_JAPANESE:
		return SQL_VARCHAR;

	default:
		return SQL_VARCHAR;
	}
}

int DbInterfaceODBC::cobol2ctype(int t)
{
	switch (t) {
	case COBOL_TYPE_UNSIGNED_NUMBER:
	case COBOL_TYPE_SIGNED_NUMBER_TC:
	case COBOL_TYPE_SIGNED_NUMBER_TS:
	case COBOL_TYPE_SIGNED_NUMBER_LC:
	case COBOL_TYPE_SIGNED_NUMBER_LS:
	case COBOL_TYPE_UNSIGNED_NUMBER_PD:
	case COBOL_TYPE_SIGNED_NUMBER_PD:
	case COBOL_TYPE_UNSIGNED_BINARY:
	case COBOL_TYPE_SIGNED_BINARY:
		return SQL_C_NUMERIC;

	case COBOL_TYPE_ALPHANUMERIC:
	case COBOL_TYPE_JAPANESE:
		return SQL_C_CHAR;

	default:
		return SQL_C_CHAR;
	}
}

int DbInterfaceODBC::get_data_len(SQLHANDLE hStmt, int cnum)
{
	SQLRETURN	   rc;
	SQLCHAR		   ColumnName[256];
	SQLSMALLINT    ColumnNameLen;
	SQLSMALLINT    ColumnDataType;
	SQLULEN        ColumnDataSize;
	SQLSMALLINT    ColumnDataDigits;
	SQLSMALLINT    ColumnDataNullable;
	SQLCHAR* ColumnData;
	SQLLEN         ColumnDataLen;

	rc = SQLDescribeCol(
		hStmt,                    // Select Statement (Prepared)
		cnum + 1,                      // Columnn Number
		ColumnName,            // Column Name (returned)
		256,         // size of Column Name buffer
		&ColumnNameLen,        // Actual size of column name
		&ColumnDataType,       // SQL Data type of column
		&ColumnDataSize,       // Data size of column in table
		&ColumnDataDigits,     // Number of decimal digits
		&ColumnDataNullable);  // Whether column nullable

	return ColumnDataSize;
}

bool DbInterfaceODBC::retrieve_prepared_statement(const std::string& prep_stmt_name, ODBCStatementData** prepared_stmt_data)
{
	std::string stmt_name = to_lower(prep_stmt_name);
	if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end() || _prepared_stmts[stmt_name] == nullptr || _prepared_stmts[stmt_name]->statement == nullptr)
		return false;

	*prepared_stmt_data = _prepared_stmts[stmt_name];
	return true;
}

int DbInterfaceODBC::odbcRetrieveError(int rc, ErrorSource err_src, SQLHANDLE h)
{
	SQLHANDLE err_handle = nullptr;
	SQLSMALLINT handle_type = 0;
	std::vector<std::string> odbc_errors;
	SQLINTEGER i = 0;
	SQLINTEGER NativeError = 0;
	SQLCHAR SQLState[7];
	SQLCHAR MessageText[1024];
	char bfr[2000];
	SQLSMALLINT TextLength;
	SQLRETURN ret = 0;

	SQLINTEGER main_NativeError = 0;
	SQLCHAR main_SQLState[7];

	if (rc != SQL_SUCCESS) {

		// *************
		switch (err_src) {
		case ErrorSource::Environmennt:
			err_handle = this->odbc_global_env_context;
			handle_type = SQL_HANDLE_ENV;
			break;

		case ErrorSource::Connection:
			err_handle = conn_handle;
			handle_type = SQL_HANDLE_DBC;
			break;

		case ErrorSource::Statement:
			handle_type = SQL_HANDLE_STMT;
			if (h != 0)
				err_handle = h;
			else {
				if (current_statement_data && current_statement_data->statement)
					err_handle = current_statement_data->statement;
			}
			break;

		default:
			lib_logger->error("Invalid error source specified");
			return -1;
		}

		if (err_handle == nullptr || handle_type == 0) {
			last_rc = rc;
			last_error = "Unknown error";
			last_state = "HY000";
			return rc;
		}

		do {
			i++;
			ret = SQLGetDiagRec(handle_type, err_handle, i, SQLState, &NativeError,
				MessageText, sizeof(MessageText), &TextLength);

			if (SQL_SUCCEEDED(ret)) {
				sprintf(bfr, "%s (%d): %s", SQLState, (long)NativeError, MessageText);
				odbc_errors.push_back(std::string(bfr));

#if _DEBUG
				lib_logger->trace("ODBC error code: {}", bfr);
#endif
			}

			if (i == 1) {
				main_NativeError = NativeError;
				strcpy((char*)&main_SQLState, (char*)&SQLState);
			}
		} while (ret == SQL_SUCCESS);

		last_error = "";

		for (std::vector< std::string>::const_iterator p = odbc_errors.begin();
			p != odbc_errors.end(); ++p) {
			last_error += *p;
			if (p != odbc_errors.end() - 1)
				last_error += ',';
		}

		if (rc == SQL_SUCCESS_WITH_INFO) {
			last_rc = abs(main_NativeError);
		}
		else {
			last_rc = main_NativeError > 0 ? -main_NativeError : main_NativeError;
		}

		last_state = (char*)&main_SQLState;
	}
	else {
		odbcClearError();
	}

	return rc;
}

void DbInterfaceODBC::odbcClearError()
{
	last_error = "";
	last_rc = DBERR_NO_ERROR;
	last_state = "00000";
}

void DbInterfaceODBC::odbcSetError(int err_code, std::string sqlstate, std::string err_msg)
{
	last_error = err_msg;
	last_rc = err_code;
	last_state = sqlstate;
}

int DbInterfaceODBC::get_affected_rows(ODBCStatementData* d)
{
	if (!d || !d->statement)
		return DBERR_SQL_ERROR;

	lib_logger->trace(FMT_FILE_FUNC "ODBC: getting number of rows affected by last statement", __FILE__, __func__);

	SQLLEN NumRows = 0;
	int rc = SQLRowCount(d->statement, &NumRows);
	if (odbcRetrieveError(rc, ErrorSource::Statement, d->statement) != SQL_SUCCESS) {
		lib_logger->error("ODBC: Error while getting row count: {}", last_error);
		return DBERR_SQL_ERROR;
	}

	lib_logger->debug(FMT_FILE_FUNC  "ODBC: affected row count: {}", __FILE__, __func__, (int)NumRows);

	return (int)NumRows;
}

ODBCStatementData::ODBCStatementData(SQLHANDLE conn_handle)
{
	int rc = SQLAllocHandle(SQL_HANDLE_STMT, conn_handle, &this->statement);
	if (rc != SQL_SUCCESS) {
		this->statement = nullptr;
	}
}

ODBCStatementData::~ODBCStatementData()
{
	int rc = 0;
	if (this->statement) {
		rc = SQLFreeStmt(this->statement, SQL_CLOSE);
		this->statement = nullptr;	// Not actually needed, since we are in a destructor, but...
	}
}

void ODBCStatementData::resizeParams(int n)
{
}

void ODBCStatementData::resizeColumnData(int n)
{
}

bool DbInterfaceODBC::column_is_binary(SQLHANDLE stmt, int col_index, bool* is_binary)
{
	SQLLEN sql_type = 0;
	SQLRETURN rc = SQLColAttribute(
		stmt,
		col_index,
		SQL_DESC_TYPE,
		nullptr,
		0,
		nullptr,
		&sql_type);

	if (odbcRetrieveError(rc, ErrorSource::Statement, stmt) != SQL_SUCCESS)
		return false;

	*is_binary = (sql_type == SQL_BINARY || sql_type == SQL_VARBINARY || sql_type == SQL_LONGVARBINARY);
	return true;
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

static std::string odbc_fixup_parameters(const std::string& sql)
{
	int n = 1;
	bool in_single_quoted_string = false;
	bool in_double_quoted_string = false;
	bool in_param_id = false;
	std::string out_sql;

	for (auto itc = sql.begin(); itc != sql.end(); ++itc) {
		char c = *itc;

		if (in_param_id && isalnum(c))
			continue;
		else {
			in_param_id = false;
		}

		switch (c) {
		case '"':
			out_sql += c;
			in_double_quoted_string = !in_double_quoted_string;
			continue;

		case '\'':
			out_sql += c;
			in_single_quoted_string = !in_single_quoted_string;
			continue;

		case '$':
		case ':':
			if (!in_single_quoted_string && !in_double_quoted_string) {
				out_sql += '?';
				in_param_id = true;
			}
			else
				out_sql += c;
			continue;

		default:
			out_sql += c;

		}
	}

	return out_sql;
}
