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

#include "DbInterfaceOracle.h"

#include <cstring>
#include "IConnection.h"
#include "Logger.h"
#include "utils.h"

#define DEFAULT_CURSOR_ARRAYSIZE	100

dpiContext* DbInterfaceOracle::odpi_global_context = nullptr;
int DbInterfaceOracle::odpi_global_context_usage_count = 0;

static std::string __get_trimmed_hostref_or_literal(void* data, int l);
static std::string odpi_fixup_parameters(const std::string& sql);

DbInterfaceOracle::DbInterfaceOracle()
{}


DbInterfaceOracle::~DbInterfaceOracle()
{
	odpi_global_context_usage_count--;
	if (odpi_global_context && odpi_global_context_usage_count == 0) {
		dpiContext_destroy(odpi_global_context);
		odpi_global_context = nullptr;
	}

	if (current_statement_data)
		delete current_statement_data;

}

int DbInterfaceOracle::init(const std::shared_ptr<spdlog::logger>& _logger)
{
	char bfr[1024];
	dpiErrorInfo info;

	owner = nullptr;
	connaddr = nullptr;
	current_statement_data = nullptr;
	last_rc = 0;
	last_error = "";
	last_state = "";

	auto lib_sink = _logger->sinks().at(0);
	lib_logger = std::make_shared<spdlog::logger>("libgixsql-pgsql", lib_sink);
	lib_logger->set_level(_logger->level());
	lib_logger->info("libgixsql-pgsql logger started");

	if (!odpi_global_context) {
		if (dpiContext_createWithParams(DPI_MAJOR_VERSION, DPI_MINOR_VERSION, NULL, &odpi_global_context, &info) < 0) {
			sprintf(bfr, "ERROR: %.*s (%s: %s), offset: %u\n", info.messageLength,
				info.message, info.fnName, info.action, info.offset);

			last_error = bfr;
			last_rc = info.code;
			last_state = info.sqlState;
			return DBERR_CONN_INIT_ERROR;
		}
	}
	
	odpi_global_context_usage_count++;

	return DBERR_NO_ERROR;
}

int DbInterfaceOracle::connect(IDataSourceInfo* conn_info, IConnectionOptions* opts)
{
	lib_logger->trace(FMT_FILE_FUNC "ODPI::connect - autocommit: {:d}, encoding: {}", __FILE__, __func__, opts->autocommit, opts->client_encoding);

	std::string conn_string = "//" + conn_info->getHost();
	if (conn_info->getPort())
		conn_string += ":" + std::to_string(conn_info->getPort());

	conn_string += "/" + conn_info->getDbName();

	dpiConn* conn = NULL;
	current_statement_data = nullptr;

	last_rc = 0;
	last_error = "";
	last_state = "";

	lib_logger->trace("odpi - connection string: [{}]", conn_string);

	if (!opts->client_encoding.empty()) {
		lib_logger->warn("Setting the encoding is not supported on Oracle, set NLS_LANG before trying to connect");
	}

	int rc = dpiConn_create(odpi_global_context,
		conn_info->getUsername().c_str(), conn_info->getUsername().size(),
		conn_info->getPassword().c_str(), conn_info->getPassword().size(),
		conn_string.c_str(), conn_string.size(),
		NULL, NULL, &conn);

	if (rc < 0) {
		dpiRetrieveError(rc);
		lib_logger->error("An error occurred while trying to establish a connection: ({}:{}) {}", last_rc, last_state, last_error);
		return DBERR_CONNECTION_FAILED;
	}

	connaddr = conn;

	if (owner)
		owner->setOpened(true);

	return DBERR_NO_ERROR;
}

int DbInterfaceOracle::reset()
{
	int rc = terminate_connection();
	if (rc == DBERR_NO_ERROR)
		return DBERR_NO_ERROR;
	else
		return DBERR_CONN_RESET_FAILED;
}

int DbInterfaceOracle::terminate_connection()
{
	for (auto it = _prepared_stmts.begin(); it != _prepared_stmts.end(); ++it) {
		if (it->second) {
			delete it->second;
			it->second = nullptr;
		}
	}

	for (auto it = _declared_cursors.begin(); it != _declared_cursors.end(); ++it) {
		// TODO: what?
	}

	if (current_statement_data) {
		delete current_statement_data;
		current_statement_data = nullptr;
	}

	if (connaddr) {
		dpiConn_close(connaddr, DPI_MODE_CONN_CLOSE_DEFAULT, nullptr, 0);
		connaddr = NULL;
	}

	if (owner)
		owner->setOpened(false);

	return DBERR_NO_ERROR;
}

int DbInterfaceOracle::begin_transaction()
{
	int rc = exec("BEGIN TRANSACTION");
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_BEGIN_TX_FAILED;
}

int DbInterfaceOracle::end_transaction(std::string completion_type)
{
	if (completion_type != "COMMIT" && completion_type != "ROLLBACK")
		return DBERR_END_TX_FAILED;

	int rc = exec(completion_type);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_END_TX_FAILED;
}

char* DbInterfaceOracle::get_error_message()
{
	return (char *)last_error.c_str();
}

int DbInterfaceOracle::get_error_code()
{
	return last_rc;
}

std::string DbInterfaceOracle::get_state()
{
	return last_state;
}

void DbInterfaceOracle::set_owner(IConnection* conn)
{
	owner = conn;
}

IConnection* DbInterfaceOracle::get_owner()
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

int DbInterfaceOracle::prepare(std::string stmt_name, std::string sql)
{
	std::string prepared_sql;
	OdpiStatementData* res = new OdpiStatementData();

	stmt_name = to_lower(stmt_name);

	lib_logger->trace(FMT_FILE_FUNC "ODPI::prepare ({}) - SQL: {}", __FILE__, __func__, stmt_name, sql);

	if (this->_prepared_stmts.find(stmt_name) != _prepared_stmts.end()) {
		return DBERR_PREPARE_FAILED;
	}

	if (this->owner->getConnectionOptions()->fixup_parameters) {
		prepared_sql = odpi_fixup_parameters(sql);
		lib_logger->trace(FMT_FILE_FUNC "ODPI::fixup parameters is on", __FILE__, __func__);
		lib_logger->trace(FMT_FILE_FUNC "ODPI::prepare ({}) - SQL(P): {}", __FILE__, __func__, stmt_name, prepared_sql);
	}
	else {
		prepared_sql = sql;
	}

	lib_logger->trace(FMT_FILE_FUNC "ODPI::prepare ({}) - SQL(P): {}", __FILE__, __func__, stmt_name, prepared_sql);

	int rc = dpiConn_prepareStmt(connaddr, 0, prepared_sql.c_str(), prepared_sql.size(), nullptr, 0, &res->statement);
	if (dpiRetrieveError(rc) < 0) {
		lib_logger->error(FMT_FILE_FUNC "ODPI::prepare ({} - res: ({}) {}", __FILE__, __func__, stmt_name, last_rc, last_error);
		return DBERR_PREPARE_FAILED;
	}
	
	dpiClearError();

	lib_logger->trace(FMT_FILE_FUNC "ODPI::prepare ({} - res: ({}) {}", __FILE__, __func__, stmt_name, last_rc, last_error);

	_prepared_stmts[stmt_name] = res;

	return DBERR_NO_ERROR;
}

int get_oracle_type(int t)
{
	switch (t) {
		case COBOL_TYPE_UNSIGNED_NUMBER:
		case COBOL_TYPE_SIGNED_NUMBER_TC:
		case COBOL_TYPE_SIGNED_NUMBER_LS:
		case COBOL_TYPE_UNSIGNED_NUMBER_PD:
		case COBOL_TYPE_SIGNED_NUMBER_PD:
		case COBOL_TYPE_UNSIGNED_BINARY:
		case COBOL_TYPE_SIGNED_BINARY:
			return DPI_ORACLE_TYPE_NUMBER;

		case COBOL_TYPE_JAPANESE:
		case COBOL_TYPE_ALPHANUMERIC:
			return DPI_ORACLE_TYPE_VARCHAR;

		default:
			return DPI_ORACLE_TYPE_VARCHAR;
	}
}

int DbInterfaceOracle::exec_prepared(std::string stmt_name, std::vector<std::string>& paramValues, std::vector<int> paramLengths, std::vector<int> paramFormats)
{
	
	lib_logger->trace(FMT_FILE_FUNC "statement name: {}", __FILE__, __func__, stmt_name);
	
	stmt_name = to_lower(stmt_name);
	
	if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end()) {
		lib_logger->error("Statement not found: {}", stmt_name);
		return DBERR_SQL_ERROR;
	}
	
	int nParams = (int)paramValues.size();
	
	OdpiStatementData* wk_rs = _prepared_stmts[stmt_name];
	wk_rs->resizeParams(nParams);

	for (int i = 0; i < nParams; i++) {
		int rc = dpiConn_newVar(connaddr, get_oracle_type(paramFormats.at(i)), DPI_NATIVE_TYPE_BYTES, 1, paramLengths.at(i), 1, 0, NULL, &wk_rs->params[i], &wk_rs->params_bfrs[i]);
		if (dpiRetrieveError(rc) < 0) {
			return DBERR_SQL_ERROR;
		}
		rc = dpiVar_setFromBytes(wk_rs->params[i], 0, paramValues.at(i).c_str(), paramValues.at(i).size());
		if (dpiRetrieveError(rc) < 0) {
			return DBERR_SQL_ERROR;
		}
		rc = dpiStmt_bindByPos(wk_rs->statement, i + 1, wk_rs->params[i]);
		if (dpiRetrieveError(rc) < 0) {
			return DBERR_SQL_ERROR;
		}
	}

	uint32_t nquery_cols;
	int rc = dpiStmt_execute(wk_rs->statement, DPI_MODE_EXEC_DEFAULT, &nquery_cols);
	if (dpiRetrieveError(rc) < 0) {
		return DBERR_SQL_ERROR;
	}

	dpiQueryInfo info;
	wk_rs->resizeColumnData(nquery_cols);
	for (int i = 1; i <= nquery_cols; i++) {

		rc = dpiStmt_getQueryInfo(wk_rs->statement, i, &info);
		if (dpiRetrieveError(rc) != DPI_SUCCESS) { return DBERR_SQL_ERROR; }

		rc = dpiConn_newVar(connaddr, info.typeInfo.oracleTypeNum, DPI_NATIVE_TYPE_BYTES, DEFAULT_CURSOR_ARRAYSIZE, info.typeInfo.clientSizeInBytes, 1, 0, NULL, &wk_rs->coldata[i - 1], &wk_rs->coldata_bfrs[i - 1]);
		if (dpiRetrieveError(rc) != DPI_SUCCESS) { return DBERR_SQL_ERROR; }

		rc = dpiStmt_define(wk_rs->statement, i, wk_rs->coldata[i - 1]);
		if (dpiRetrieveError(rc) != DPI_SUCCESS) { return DBERR_SQL_ERROR; }
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceOracle::exec(std::string query)
{
	return _odpi_exec(nullptr, query);
}

int DbInterfaceOracle::_odpi_exec(ICursor* crsr, std::string query, OdpiStatementData *prep_stmt_data)
{
	int rc = 0;
	uint32_t nquery_cols = 0;
	std::string q = query;
	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#", __FILE__, __func__, q);

	OdpiStatementData* wk_rs = nullptr;
	
	if (!prep_stmt_data) {
		wk_rs = (OdpiStatementData*)((crsr != NULL) ? crsr->getPrivateData() : current_statement_data);

		if (wk_rs && wk_rs == current_statement_data) {
			delete current_statement_data;
			current_statement_data = nullptr;
		}

		wk_rs = new OdpiStatementData();
		rc = dpiConn_prepareStmt(connaddr, 0, query.c_str(), query.size(), NULL, 0, &wk_rs->statement);
		if (dpiRetrieveError(rc) != DPI_SUCCESS) {
			return DBERR_SQL_ERROR;
		}
	}
	else {
		wk_rs = prep_stmt_data;	// Already prepared
	}

	rc = dpiStmt_execute(wk_rs->statement, DPI_MODE_EXEC_DEFAULT, &nquery_cols);
	if (dpiRetrieveError(rc) != DPI_SUCCESS) {
		return DBERR_SQL_ERROR;
	}

	dpiQueryInfo info;
	wk_rs->resizeColumnData(nquery_cols);
	for (int i = 1; i <= nquery_cols; i++) {

		rc = dpiStmt_getQueryInfo(wk_rs->statement, i, &info);
		if (dpiRetrieveError(rc) != DPI_SUCCESS) { return DBERR_SQL_ERROR; }

		rc = dpiConn_newVar(connaddr, info.typeInfo.oracleTypeNum, DPI_NATIVE_TYPE_BYTES, DEFAULT_CURSOR_ARRAYSIZE, info.typeInfo.clientSizeInBytes, 1, 0, NULL, &wk_rs->coldata[i-1], &wk_rs->coldata_bfrs[i-1]);
		if (dpiRetrieveError(rc) != DPI_SUCCESS) { return DBERR_SQL_ERROR; }

		rc = dpiStmt_define(wk_rs->statement, i, wk_rs->coldata[i-1]);
		if (dpiRetrieveError(rc) != DPI_SUCCESS) { return DBERR_SQL_ERROR; }
	}

	if (!prep_stmt_data) {
		q = trim_copy(q);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			int nrows = _odpi_get_num_rows(wk_rs->statement);
			if (nrows <= 0) {
				last_rc = 100;
				return DBERR_SQL_ERROR;
			}
		}
	}

	if (last_rc == DPI_SUCCESS) {
		if (crsr) {
			if (crsr->getPrivateData())
				delete (OdpiStatementData *) crsr->getPrivateData();

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


int DbInterfaceOracle::exec_params(std::string query, int nParams, const std::vector<int>& paramTypes, const std::vector<std::string>& paramValues, const std::vector<int>& paramLengths, const std::vector<int>& paramFormats)
{
	return _odpi_exec_params(nullptr, query, nParams, paramTypes, paramValues, paramLengths, paramFormats);
}

int DbInterfaceOracle::_odpi_exec_params(ICursor* crsr, std::string query, int nParams, const std::vector<int>& paramTypes, const std::vector<std::string>& paramValues, const std::vector<int>& paramLengths, const std::vector<int>& paramFormats, OdpiStatementData* prep_stmt_data)
{
	std::string q = query;
	int rc = 0;

	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#", __FILE__, __func__, q);

	OdpiStatementData* wk_rs = nullptr;

	if (!prep_stmt_data) {
		wk_rs = (OdpiStatementData*)((crsr != NULL) ? crsr->getPrivateData() : current_statement_data);

		if (wk_rs && wk_rs == current_statement_data) {
			delete current_statement_data;
			current_statement_data = nullptr;
		}

		wk_rs = new OdpiStatementData();
		rc = dpiConn_prepareStmt(connaddr, 0, query.c_str(), query.size(), NULL, 0, &wk_rs->statement);
		if (dpiRetrieveError(rc) != DPI_SUCCESS) {
			return DBERR_SQL_ERROR;
		}
	}
	else {
		wk_rs = prep_stmt_data;	// Already prepared
	}

	wk_rs->resizeParams(nParams);

	for (int i = 0; i < nParams; i++) {

		rc = dpiConn_newVar(connaddr, get_oracle_type(paramFormats.at(i)), DPI_NATIVE_TYPE_BYTES, 1, paramValues.at(i).size(), 1, 0, NULL, &wk_rs->params[i], &wk_rs->params_bfrs[i]);
		if (dpiRetrieveError(rc) < 0) {
			return DBERR_SQL_ERROR;
		}

		// Some ODPI weirdness: numbers cannot have a '+' sign if positive, only '-' is accepted for negative numbers
		// We handle this here instead of patching ODPI so we can update it in the future
		if (COBOL_TYPE_IS_NUMERIC(paramTypes.at(i)) && paramValues.at(i).size() > 0 && paramValues.at(i).at(0) == '+') {
			rc = dpiVar_setFromBytes(wk_rs->params[i], 0, paramValues.at(i).c_str() + 1, paramValues.at(i).size() - 1);
		}
		else {
			rc = dpiVar_setFromBytes(wk_rs->params[i], 0, paramValues.at(i).c_str(), paramValues.at(i).size());
		}
		
		if (dpiRetrieveError(rc) < 0) {
			return DBERR_SQL_ERROR;
		}
		rc = dpiStmt_bindByPos(wk_rs->statement, i + 1, wk_rs->params[i]);
		if (dpiRetrieveError(rc) < 0) {
			return DBERR_SQL_ERROR;
		}
	}

	uint32_t nquery_cols = 0;
	rc = dpiStmt_execute(wk_rs->statement, DPI_MODE_EXEC_DEFAULT, &nquery_cols);
	if (dpiRetrieveError(rc) != DPI_SUCCESS) {
		return DBERR_SQL_ERROR;
	}

	for (int i = 1; i <= nquery_cols; i++) {
		rc = dpiStmt_defineValue(wk_rs->statement, i, DPI_ORACLE_TYPE_VARCHAR, DPI_NATIVE_TYPE_BYTES, 0, 0, NULL);
		if (dpiRetrieveError(rc) != DPI_SUCCESS) {
			return DBERR_SQL_ERROR;
		}
	}

	if (!prep_stmt_data) {
		q = trim_copy(q);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			int nrows = _odpi_get_num_rows(wk_rs->statement);
			if (nrows <= 0) {
				last_rc = 100;
				return DBERR_SQL_ERROR;
			}
		}
	}

	if (last_rc == DPI_SUCCESS) {
		if (crsr) {
			if (crsr->getPrivateData())
				delete (OdpiStatementData *) crsr->getPrivateData();

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

bool DbInterfaceOracle::is_cursor_from_prepared_statement(ICursor* cursor)
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

int DbInterfaceOracle::close_cursor(ICursor* cursor)
{
	if (!cursor) {
		lib_logger->error("Invalid cursor reference");
		return DBERR_CLOSE_CURSOR_FAILED;
	}

	// Prepared statements used for cursors will be disposed separately
	if (!is_cursor_from_prepared_statement(cursor)) {
		OdpiStatementData* dp = (OdpiStatementData*)cursor->getPrivateData();

		if (!dp || !dp->statement)
			return DBERR_CLOSE_CURSOR_FAILED;

		int rc = dpiStmt_release(dp->statement);
		dp->statement = nullptr;
		if (dpiRetrieveError(rc) != DPI_SUCCESS) {
			return DBERR_CLOSE_CURSOR_FAILED;
		}

		delete dp;
	}

	cursor->setPrivateData(nullptr);
	cursor->setOpened(false);

	return DBERR_NO_ERROR;
}

int DbInterfaceOracle::cursor_declare(ICursor* cursor, bool with_hold, int res_type)
{
	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceOracle::cursor_declare_with_params(ICursor* cursor, char** param_values, bool with_hold, int res_type)
{
	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceOracle::cursor_open(ICursor* cursor)
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

	OdpiStatementData* prepared_stmt_data = nullptr;
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
		rc = _odpi_exec_params(cursor, squery, cursor->getNumParams(), param_types, params, param_lengths, param_types, prepared_stmt_data);
	}
	else {
		rc = _odpi_exec(cursor, squery, prepared_stmt_data);
	}

	if (dpiRetrieveError(rc) == DPI_SUCCESS) {
		cursor->setOpened(true);
		return DBERR_NO_ERROR;
	}
	else {
		cursor->setOpened(false);
		return DBERR_OPEN_CURSOR_FAILED;
	}
}

int DbInterfaceOracle::fetch_one(ICursor* cursor, int fetchmode)
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
	
	OdpiStatementData* dp = (OdpiStatementData *)cursor->getPrivateData();

	if (!dp || !dp->statement)
		return DBERR_FETCH_ROW_FAILED;

	int found;
	uint32_t bfr_row_index;
	int rc = dpiStmt_fetch(dp->statement, &found, &bfr_row_index);

	if (dpiRetrieveError(rc) != DPI_SUCCESS)
		return DBERR_FETCH_ROW_FAILED;

	if (!found)
		return DBERR_NO_DATA;

	return DBERR_NO_ERROR;
}

bool DbInterfaceOracle::get_resultset_value(ResultSetContextType resultset_context_type, void* context, int row, int col, char* bfr, int bfrlen, int* value_len)
{
	int rc = 0;
	OdpiStatementData* wk_rs = nullptr;

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

			wk_rs = (OdpiStatementData*)_prepared_stmts[stmt_name];
		}
		break;

		case ResultSetContextType::Cursor:
		{
			ICursor* c = (ICursor*)context;
			if (!c) {
				lib_logger->error("Invalid cursor reference");
				return false;
			}
			wk_rs = (OdpiStatementData*)c->getPrivateData();
		}
		break;
	}

	if (!wk_rs) {
		lib_logger->error("Invalid resultset");
		return false;
	}

	dpiData* col_data;
	dpiQueryInfo col_info;
	dpiNativeTypeNum nativeTypeNum;

	rc = dpiStmt_getQueryValue(wk_rs->statement, (col + 1), &nativeTypeNum, &col_data);
	if (dpiRetrieveError(rc) < 0) {
		lib_logger->error("Invalid column data");
		return false;
	}

	rc = dpiStmt_getQueryInfo(wk_rs->statement, (col + 1), &col_info);
	if (dpiRetrieveError(rc) < 0) {
		lib_logger->error("Invalid column data");
		return false;
	}
	
	uint32_t sz = col_info.typeInfo.sizeInChars;

	char *c = col_data->value.asBytes.ptr;
	uint32_t l = col_data->value.asBytes.length;

#ifdef VERBOSE
	lib_logger->trace(FMT_FILE_FUNC "col: {}, data: {}", __FILE__, __func__, col, std::string(c, l));
	std::string s = fmt::format("col: {}, data: {}", col, std::string(c, l));
	fprintf(stderr, "%s\n", s.c_str());
#endif

	if (l > bfrlen) {
		return false;
	}

	*value_len = (int)l;
	memcpy(bfr, c, l);
	bfr[l] = '\0';

	return true;
}

bool DbInterfaceOracle::move_to_first_record(std::string stmt_name)
{
	OdpiStatementData* dp = nullptr;

	lib_logger->trace(FMT_FILE_FUNC "ODPI: moving to first row in resultset", __FILE__, __func__);

	if (stmt_name.empty()) {
		if (!current_statement_data) {
			dpiSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
			return false;
		}
		
		dp = current_statement_data;
	}
	else {
		stmt_name = to_lower(stmt_name);
		if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end()) {
			dpiSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
			return false;
		}
		dp = _prepared_stmts[stmt_name];
	}

	if (!dp || !dp->statement) {
		dpiSetError(DBERR_MOVE_TO_FIRST_FAILED, "HY000", "Invalid statement reference");
		return false;
	}

	int found;
	uint32_t bfr_row_index;
	int rc = dpiStmt_fetch(dp->statement, &found, &bfr_row_index);

	if (dpiRetrieveError(rc) != DPI_SUCCESS) 
		return false;

	if (!found) {
		dpiSetError(DBERR_NO_DATA, "02000", "No data");
		return false;
	}

	return true;
}

int DbInterfaceOracle::supports_num_rows()
{
	return 1;
}

int DbInterfaceOracle::get_num_rows(ICursor* crsr)
{
	dpiStmt* wk_rs = nullptr;

	if (crsr) {
		OdpiStatementData* p = (OdpiStatementData*)crsr->getPrivateData();
		wk_rs = p->statement;
	}
	else {
		if (!current_statement_data)
			return -1;

		wk_rs = current_statement_data->statement;
	}

	if (wk_rs)
		return wk_rs->rowCount;
	else
		return -1;
}

int DbInterfaceOracle::get_num_fields(ICursor* crsr)
{
	dpiStmt *wk_rs = nullptr;
	
	if (crsr) {
		OdpiStatementData* p = (OdpiStatementData *)crsr->getPrivateData();
		wk_rs = p->statement;
	}
	else {
		if (!current_statement_data)
			return -1;

		wk_rs = current_statement_data->statement;
	}

	if (wk_rs) {
		uint32_t col_count = -1;
		if (dpiStmt_getNumQueryColumns(wk_rs, &col_count) >= 0)
			return col_count;
		else
			return -1;
	}
	else
		return -1;
}

int DbInterfaceOracle::_odpi_get_num_rows(dpiStmt* r)
{
	uint64_t res = 0;
	if (!r)
		return -1;

	int rc = dpiStmt_getRowCount(r, &res);
	if (dpiRetrieveError(rc))
		return -1;

	return (int)res;
}

bool DbInterfaceOracle::retrieve_prepared_statement(const std::string& prep_stmt_name, OdpiStatementData**prepared_stmt_data)
{
	std::string stmt_name = to_lower(prep_stmt_name);
	if (_prepared_stmts.find(stmt_name) == _prepared_stmts.end() || _prepared_stmts[stmt_name] == nullptr || _prepared_stmts[stmt_name]->statement == nullptr)
		return false;

	*prepared_stmt_data = _prepared_stmts[stmt_name];
	return true;
}

int DbInterfaceOracle::dpiRetrieveError(int rc)
{
	char bfr[1024];
	dpiErrorInfo info;

	if (rc < 0) {
		dpiContext_getError(odpi_global_context, &info);
		sprintf(bfr, "ERROR: %.*s (%s: %s), offset: %u\n", info.messageLength,
			info.message, info.fnName, info.action, info.offset);

		last_error = bfr;
		last_rc = info.code > 0 && !info.isWarning ? -info.code : info.code;

		last_state = info.sqlState;
	}
	else {
		dpiClearError();
	}

	return rc;
}

void DbInterfaceOracle::dpiClearError()
{
	last_error = "";
	last_rc = DBERR_NO_ERROR;
	last_state = "00000";
}

void DbInterfaceOracle::dpiSetError(int err_code, std::string sqlstate, std::string err_msg)
{
	last_error = err_msg;
	last_rc = err_code;
	last_state = sqlstate;
}

OdpiStatementData::OdpiStatementData()
{
	this->params_count = 0;
	this->coldata_count = 0;
}

OdpiStatementData::~OdpiStatementData()
{
	cleanup();
	if (statement) {
		dpiStmt_release(statement);
		statement = nullptr;
	}
}

void OdpiStatementData::resizeParams(int n)
{
	cleanup();

	params_count = n;
	this->params = new dpiVar * [n];
	this->params_bfrs = new dpiData * [n];

}

void OdpiStatementData::resizeColumnData(int n)
{
	coldata_count = n;
	this->coldata = new dpiVar * [n];
	this->coldata_bfrs = new dpiData * [n];
}

void OdpiStatementData::cleanup()
{
	if (params) {
		for (int i = 0; i < params_count; i++) {
			dpiVar_release(params[i]);
			params[i] = nullptr;
		}
		delete[] params;
		params = nullptr;
	}

	if (coldata) {
		for (int i = 0; i < coldata_count; i++) {
			dpiVar_release(coldata[i]);
			coldata[i] = nullptr;
		}
		delete[] coldata;
		coldata = nullptr;
	}

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

static std::string odpi_fixup_parameters(const std::string& sql)
{
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

		case '$':	// :1 is valid in Oracle, so we just change the prefix
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
}