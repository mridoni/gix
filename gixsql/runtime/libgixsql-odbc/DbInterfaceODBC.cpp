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

#define ERR_SRC_ENV		1
#define ERR_SRC_CONN	2
#define ERR_SRC_STMT	3

DbInterfaceODBC::DbInterfaceODBC()
{
}


DbInterfaceODBC::~DbInterfaceODBC()
{
	// TODO: Investigate: if enabled this seems to cause a crash in some situations
	// where the handle is freed twice
	//if (cur_stmt_handle)
	//	SQLFreeHandle(SQL_HANDLE_STMT, cur_stmt_handle);

	if (cur_stmt_handle)
		SQLFreeHandle(SQL_HANDLE_DBC, conn_handle);

	if (cur_stmt_handle)
		SQLFreeHandle(SQL_HANDLE_ENV, env_handle);
}



int DbInterfaceODBC::init(const std::shared_ptr<spdlog::logger>& _logger)
{
	env_handle = NULL;
	conn_handle = NULL;
	cur_stmt_handle = NULL;
	driver_has_num_rows_support = 1;
	dynamic_cursor_emulation = false;
	rowid_col_name = "";
	current_rowid_val[0] = 0;
	owner = NULL;

	auto lib_sink = _logger->sinks().at(0);
	lib_logger = std::make_shared<spdlog::logger>("libgixsql-odbc", lib_sink);
	lib_logger->set_level(_logger->level());
	lib_logger->info("libgixsql-odbc logger started");

	SQLRETURN rc = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env_handle);
	if (rc != SQL_SUCCESS) {
		lib_logger->debug(FMT_FILE_FUNC "FATAL ERROR: Can't allocate SQL Handle for the ODBC environment", __FILE__, __func__);
		lib_logger->error("FATAL ERROR: Can't allocate SQL Handle for the ODBC environment");
		env_handle = NULL;
		return DBERR_OUT_OF_MEMORY;
	}

	// set ODBC3 version but ignore the error
	rc = SQLSetEnvAttr(env_handle, SQL_ATTR_ODBC_VERSION, (SQLPOINTER)SQL_OV_ODBC3, 0);
	if (last_rc != SQL_SUCCESS) {
		lib_logger->debug(FMT_FILE_FUNC  "WARNING: Cannot set ODBC version", __FILE__, __func__);
		lib_logger->error("WARNING: Cannot set ODBC version");
	}

	// set NTS if possible to avoid extra moves
	rc = SQLSetEnvAttr(env_handle, SQL_ATTR_OUTPUT_NTS, (SQLPOINTER)SQL_FALSE, 0);
	if (last_rc != SQL_SUCCESS) {
		lib_logger->debug(FMT_FILE_FUNC  "WARNING: Cannot set NTS", __FILE__, __func__);
		lib_logger->error("WARNING: Cannot set ODBC NTS");
	}

	rc = SQLAllocHandle(SQL_HANDLE_DBC, env_handle, &conn_handle);
	if (rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_ENV);
		lib_logger->debug(FMT_FILE_FUNC  "FATAL ERROR: Can't allocate SQL Handle for the ODBC connection", __FILE__, __func__);
		lib_logger->error("FATAL ERROR: Can't allocate SQL Handle for the ODBC connection");
		SQLFreeHandle(SQL_HANDLE_ENV, env_handle);
		env_handle = NULL;
		conn_handle = NULL;
		return DBERR_OUT_OF_MEMORY;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::connect(IDataSourceInfo* conn_string, int autocommit, std::string encoding)
{
	char dbms_name[256];
	std::string host = conn_string->getHost();
	std::string user = conn_string->getUsername();
	std::string pwd = conn_string->getPassword();

	lib_logger->trace(FMT_FILE_FUNC  "ODBC: DB connect to DSN '{}' user = '{}'",__FILE__, __func__, host, user);

	// Connect
	if (!user.empty()) {
		last_rc = SQLConnect(conn_handle, (SQLCHAR*)host.c_str(), SQL_NTS, (SQLCHAR*)user.c_str(), SQL_NTS, (SQLCHAR*)pwd.c_str(), SQL_NTS);
	}
	else {
		last_rc = SQLConnect(conn_handle, (SQLCHAR*)host.c_str(), SQL_NTS, 0, 0, 0, 0);
	}

	if (last_rc != SQL_SUCCESS && last_rc != SQL_SUCCESS_WITH_INFO) {
		retrieve_odbc_error(ERR_SRC_CONN);
		return DBERR_CONNECTION_FAILED;
	}

	last_rc = SQLAllocHandle(SQL_HANDLE_STMT, conn_handle, &cur_stmt_handle);
	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_CONN);
		lib_logger->debug(FMT_FILE_FUNC "FATAL ERROR: Can't allocate SQL Handle for the ODBC statement", __FILE__, __func__);
		lib_logger->error("FATAL ERROR: Can't allocate SQL Handle for the ODBC statement");
		return DBERR_CONNECTION_FAILED;
	}


	if (!autocommit) {
		// try to set AUTOCOMMIT OFF
		last_rc = SQLSetConnectAttr(conn_handle, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF, 0);
		if (last_rc != SQL_SUCCESS) {
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

	if (strncmp(dbms_name, "DB2", strlen("DB2")) == 0 || strncmp(dbms_name, "Oracle", strlen("Oracle")) == 0 || strncmp(dbms_name, "PostgreSQL", strlen("PostgreSQL")) == 0) {
		lib_logger->debug(FMT_FILE_FUNC "Driver does not have extended support for SQLNumRows", __FILE__, __func__);
		driver_has_num_rows_support = 0;
	}

	const char* enable_dynamic_cursor_emulation = getenv("GIXSQL_DYN_CRSR_EMU");
	if (enable_dynamic_cursor_emulation && strcmp(enable_dynamic_cursor_emulation, "1") == 0) {
		if (strncmp(dbms_name, "PostgreSQL", strlen("PostgreSQL")) == 0) {
			lib_logger->debug(FMT_FILE_FUNC "Driver will emulate dynamic (updatable) cursors", __FILE__, __func__);
			dynamic_cursor_emulation = true;
			rowid_col_name = "ctid";
		}
	}
	
	if (owner)
		owner->setOpened(true);

	lib_logger->debug(FMT_FILE_FUNC  "OCSQL-ODBC: Connection registration successful", __FILE__, __func__);
	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::reset()
{
	lib_logger->trace(FMT_FILE_FUNC  "ODBC: connection reset invoked", __FILE__, __func__);

	terminate_connection();
	if (last_rc == DBERR_NO_ERROR)
		return DBERR_NO_ERROR;
	else
		return DBERR_CONN_RESET_FAILED;
}

int DbInterfaceODBC::terminate_connection()
{
	lib_logger->trace(FMT_FILE_FUNC "ODBC: connection termination invoked", __FILE__, __func__);

	last_rc = SQLDisconnect(conn_handle);
	if (owner)
		owner->setOpened(false);

	retrieve_odbc_error(ERR_SRC_CONN);
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

	SQLSMALLINT sql_completion_type = (completion_type == "COMMIT") ? SQL_COMMIT : SQL_ROLLBACK;
	last_rc = SQLFreeStmt(cur_stmt_handle, SQL_CLOSE);
	if (last_rc != SQL_SUCCESS) {
		lib_logger->debug(FMT_FILE_FUNC "ODBC: Error while ending transaction (1)({}): {}", __FILE__, __func__, completion_type, last_rc);
		lib_logger->error("ODBC: Error while ending transaction (1)({}): {}", completion_type, last_rc);
		retrieve_odbc_error(ERR_SRC_STMT);
		return DBERR_END_TX_FAILED;
	}

	last_rc = SQLEndTran(SQL_HANDLE_DBC, conn_handle, sql_completion_type);
	if (last_rc != SQL_SUCCESS) {
		lib_logger->debug(FMT_FILE_FUNC  "ODBC: Error while ending transaction (2)({}): {}", __FILE__, __func__, completion_type, last_rc);
		lib_logger->error("ODBC: Error while ending transaction (1)({}): {}", completion_type, last_rc);
		retrieve_odbc_error(ERR_SRC_STMT);
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
	if (odbc_errors.size() > 0) {
		return strdup(odbc_errors.at(0).c_str());
	}
	else {
		return (char*)"ODBC: No error";
	}
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
	last_rc = DBERR_NOT_IMPL;
	odbc_errors.push_back("NOTIMPL");
	return DBERR_PREPARE_FAILED;
}

int DbInterfaceODBC::exec_prepared(std::string stmt_name, std::vector<std::string> &paramValues, std::vector<int> paramLengths, std::vector<int> paramFormats)
{
	last_rc = DBERR_NOT_IMPL;
	odbc_errors.push_back("NOTIMPL");
	return DBERR_SQL_ERROR;
}


int DbInterfaceODBC::exec(std::string _query)
{
	return _odbc_exec(nullptr, _query);
}


int DbInterfaceODBC::_odbc_exec(ICursor* crsr, const std::string query)
{
	int rc = 0;
	SQLHANDLE exec_handle = 0;

	lib_logger->trace(FMT_FILE_FUNC "ODBC EXEC SQL: {}", __FILE__, __func__, query);


	if (is_begin_transaction_statement(query)) {
		lib_logger->debug(FMT_FILE_FUNC "ODBC - BEGIN TRANSACTION invoked, skipping statement", __FILE__, __func__);
		return DBERR_NO_ERROR;
	}

	if (is_commit_or_rollback_statement(query)) {
		rc = end_transaction(query);
		if (rc != DBERR_NO_ERROR) {	// Error already retrieved in end_transaction
			return DBERR_SQL_ERROR;
		}
		return DBERR_NO_ERROR;
	}

	if (!crsr) {
		exec_handle = cur_stmt_handle;
		rc = SQLFreeStmt(exec_handle, SQL_CLOSE);
		if (rc != SQL_SUCCESS) {
			last_rc = rc;
			retrieve_odbc_error(ERR_SRC_STMT, exec_handle);
			lib_logger->error("ODBC: Error while releasing statement ({}): {}", last_rc, query);
			return DBERR_SQL_ERROR;
		}
	}
	else {
		exec_handle = crsr->getPrivateData();
	}

	// TODO: here we should handle dynamic cursor emulation, but we  have a problem: since by definition 
	// parameters are not handled here, we cannot simply add one for the row ID. This probably
	// will have to be redirected to an exec_params call

	rc = SQLPrepare(exec_handle, (SQLCHAR*)query.c_str(), SQL_NTS);
	if (rc != SQL_SUCCESS) {
		last_rc = rc;
		retrieve_odbc_error(ERR_SRC_STMT, exec_handle);
		lib_logger->error("ODBC: Error while preparing statement: {}", last_rc);
		return DBERR_SQL_ERROR;
	}

	rc = SQLExecute(exec_handle);
	if (rc != SQL_SUCCESS) {
		last_rc = rc;
		retrieve_odbc_error(ERR_SRC_STMT, exec_handle);

#if !_DEBUG
		if (last_rc != 100)
			lib_logger->error("ODBC: Error while executing statement ({}): {}", last_rc, query);
#endif
		return DBERR_SQL_ERROR;
}

	if (!crsr) {
		std::string q = trim_copy(query);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			SQLLEN NumRows = 0;
			int tmp_rc = SQLRowCount(cur_stmt_handle, &NumRows);
			if (tmp_rc == SQL_SUCCESS) {
				if (NumRows == 0) {
					last_rc = 100;
					return DBERR_SQL_ERROR;
				}
			}
		}
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::exec_params(std::string _query, int nParams, int* paramTypes, std::vector<std::string>& paramValues, int* paramLengths, int* paramFormats)
{
	return _odbc_exec_params(nullptr, _query, nParams, paramTypes, paramValues, paramLengths, paramFormats);
}

int DbInterfaceODBC::_odbc_exec_params(ICursor* crsr, std::string _query, int nParams, int* paramTypes, std::vector<std::string>& paramValues, int* paramLengths, int* paramFormats)
{
	std::string query = _query;
	int rc = 0;
	SQLHANDLE exec_handle = 0;
	bool add_rowid_param = false;

	lib_logger->trace(FMT_FILE_FUNC "ODBC EXEC SQL ({}): {}", __FILE__, __func__, query, crsr ? "WITH CURSOR" : "NO CURSOR");

	if (is_begin_transaction_statement(query)) {
		lib_logger->debug(FMT_FILE_FUNC "ODBC - BEGIN TRANSACTION invoked, skipping statement", __FILE__, __func__);
		return 1;
	}

	if (is_commit_or_rollback_statement(query)) {
		rc = end_transaction(query);
		if (rc != DBERR_NO_ERROR) {	// Error already retrieved in end_transaction
			return DBERR_SQL_ERROR;
		}
		return DBERR_NO_ERROR;
	}

	if (!crsr) {
		exec_handle = cur_stmt_handle;
		rc = SQLFreeStmt(exec_handle, SQL_CLOSE);
		if (rc != SQL_SUCCESS) {
			last_rc = rc;
			retrieve_odbc_error(ERR_SRC_STMT, exec_handle);
			lib_logger->error("ODBC: Error while releasing statement ({}): {}", last_rc, query);
			return DBERR_SQL_ERROR;
		}
	}
	else {
		exec_handle = crsr->getPrivateData();
	}

	if (dynamic_cursor_emulation) {
		std::string _wco_cname = "";
		if (crsr && is_select_statement(query)) {
			int p = find_nocase(" FOR UPDATE", query);
			if (p != std::string::npos) {
				query = query.substr(0, p);
				query = "SELECT " + rowid_col_name + "," + query.substr(6);
				lib_logger->debug(FMT_FILE_FUNC "Query is being rewritten as [{}]", __FILE__, __func__, query);
			}
		}

		if (!crsr && is_update_or_delete_statement(query)) {
			std::string _wco_cname;
			int p = 0;
			if (has_where_current_of(query, _wco_cname, &p)) {
				lib_logger->debug(FMT_FILE_FUNC "ODBC - WHERE CURRENT OF CLAUSE, CURSOR IS: [{}]", __FILE__, __func__, _wco_cname);
				if (_declared_cursors.find(_wco_cname) != _declared_cursors.end()) {
					query = query.substr(0, p) + "WHERE " + rowid_col_name + " = ?";
					lib_logger->debug(FMT_FILE_FUNC "Query is being rewritten as [{}]", __FILE__, __func__, query);
					add_rowid_param = true;
				}
			}
		}
	}

	rc = SQLPrepare(exec_handle, (SQLCHAR*)query.c_str(), SQL_NTS);
	if (rc != SQL_SUCCESS) {
		last_rc = rc;
		retrieve_odbc_error(ERR_SRC_STMT, exec_handle);
		lib_logger->error("ODBC: Error while preparing statement ({}): {}", last_rc, query);
		return DBERR_SQL_ERROR;
	}

	char** pvals = (char** ) calloc(nParams, sizeof(char*));

	for (int i = 0; i < nParams; i++) {
		pvals[i] = (char*)paramValues.at(i).c_str();
	}

	for (int i = 0; i < nParams; i++) {
		SQLLEN len;
		int ptype = cobol2odbctype(paramTypes[i]);
		int ctype = cobol2ctype(paramTypes[i]);

		rc = SQLBindParameter(exec_handle,
			i + 1,
			SQL_PARAM_INPUT,
			SQL_C_CHAR,
			ptype, // SQL_VARCHAR,
			10,
			0,
			(SQLPOINTER)pvals[i],
			(SQLLEN)strlen(pvals[i]),
			NULL);

		if (rc != SQL_SUCCESS) {
			free(pvals);
			last_rc = rc;
			retrieve_odbc_error(ERR_SRC_STMT, exec_handle);
			lib_logger->error("ODBC: Error while binding parameter {} in statement ({}): {}", i + 1, last_rc, query);
			return DBERR_SQL_ERROR;
		}
	}

	if (add_rowid_param) {
		if (!current_rowid_val) {
			return DBERR_SQL_ERROR;
		}
		lib_logger->debug(FMT_FILE_FUNC "Binding row id parameter: [{}]", __FILE__, __func__, current_rowid_val);
		rc = SQLBindParameter(exec_handle,
			nParams + 1,
			SQL_PARAM_INPUT,
			SQL_C_CHAR,
			SQL_VARCHAR, // SQL_VARCHAR,
			10,
			0,
			(SQLPOINTER)current_rowid_val,
			(SQLLEN)strlen(current_rowid_val),
			NULL);
		if (rc) {
			free(pvals);
			last_rc = rc;
			retrieve_odbc_error(ERR_SRC_STMT, exec_handle);
			lib_logger->error("ODBC: Error while binding special parameter {} in statement ({}): {}", nParams + 1, last_rc, query);
			return DBERR_SQL_ERROR;
		}
	}

	rc = SQLExecute(exec_handle);
	if (rc != SQL_SUCCESS) {
		free(pvals);
		last_rc = rc;
		retrieve_odbc_error(ERR_SRC_STMT, exec_handle);
		if (last_rc != 100)
			lib_logger->error("ODBC: Error while executing statement ({}): {}", last_rc, query);

		return DBERR_SQL_ERROR;
	}

	free(pvals);

	if (!crsr) {
		std::string q = trim_copy(_query);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			SQLLEN NumRows = 0;
			int tmp_rc = SQLRowCount(cur_stmt_handle, &NumRows);
			if (tmp_rc == SQL_SUCCESS) {
				if (NumRows == 0) {
					last_rc = 100;
					return DBERR_SQL_ERROR;
				}
			}
		}
	}

	return DBERR_NO_ERROR;
}


int DbInterfaceODBC::close_cursor(ICursor* cursor)
{
	lib_logger->trace(FMT_FILE_FUNC "ODBC: close cursor invoked", __FILE__, __func__);

	SQLHANDLE cursor_handle = cursor->getPrivateData();
	if (!cursor_handle) {
		lib_logger->error("ODBC: Error while closing cursor: invalid ODBC cursor data");
		return DBERR_CLOSE_CURSOR_FAILED;
	}

	last_rc = SQLCloseCursor(cursor_handle);
	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_STMT, cursor_handle);
		lib_logger->error("ODBC: Error while closing cursor ({}) {}", last_rc, cursor->getName());
		return DBERR_CLOSE_CURSOR_FAILED;
	}

	last_rc = SQLFreeHandle(SQL_HANDLE_STMT, cursor_handle);
	cursor->setPrivateData(NULL);

	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_STMT);
		lib_logger->error("ODBC: Error while closing cursor ({}) {}", last_rc, cursor->getName());
		return DBERR_CLOSE_CURSOR_FAILED;
	}

	std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it != _declared_cursors.end()) {
		_declared_cursors.erase(cursor->getName());
	}

	return DBERR_NO_ERROR;

}

int DbInterfaceODBC::cursor_declare(ICursor* cursor, bool with_hold, int res_type)
{
	lib_logger->trace(FMT_FILE_FUNC "ODBC: cursor declare invoked", __FILE__, __func__);

	if (cursor != NULL) {

		SQLHANDLE cursor_handle = NULL;
		last_rc = SQLAllocHandle(SQL_HANDLE_STMT, conn_handle, &cursor_handle);
		if (last_rc != SQL_SUCCESS) {
			retrieve_odbc_error(ERR_SRC_STMT);
			lib_logger->error("ODBC: Error while allocating cursor ({}) {}", last_rc, cursor->getName());
			return DBERR_DECLARE_CURSOR_FAILED;
		}

		last_rc = SQLSetCursorName(cursor_handle, (SQLCHAR*)cursor->getName().c_str(), SQL_NTS);
		lib_logger->debug(FMT_FILE_FUNC "ODBC: setting cursor name: [{}]", __FILE__, __func__, cursor->getName());
		if (last_rc != SQL_SUCCESS) {
			retrieve_odbc_error(ERR_SRC_STMT);
			lib_logger->error("ODBC: Error while setting cursor name ({}) {}", last_rc, cursor->getName());
			return DBERR_DECLARE_CURSOR_FAILED;
		}

		cursor->setPrivateData(last_rc == DBERR_NO_ERROR ? cursor_handle : NULL);

		std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
		if (it == _declared_cursors.end()) {
			_declared_cursors[cursor->getName()] = cursor;
		}
	}

	// Nothing else to do here
	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::cursor_declare_with_params(ICursor* cursor, char** param_values, bool with_hold, int res_type)
{
	lib_logger->trace(FMT_FILE_FUNC "ODBC: cursor declare (with params) invoked" __FILE__, __func__);

	if (cursor != NULL) {

		std::string cursor_name = to_lower(cursor->getName());

		SQLHANDLE cursor_handle = NULL;
		last_rc = SQLAllocHandle(SQL_HANDLE_STMT, conn_handle, &cursor_handle);
		if (last_rc != SQL_SUCCESS) {
			retrieve_odbc_error(ERR_SRC_STMT);
			lib_logger->error("ODBC: Error while allocating cursor ({}) {}", last_rc, cursor->getName());
			return DBERR_DECLARE_CURSOR_FAILED;
		}

		last_rc = SQLSetCursorName(cursor_handle, (SQLCHAR*)cursor_name.c_str(), SQL_NTS);
		lib_logger->debug(FMT_FILE_FUNC  "ODBC: setting cursor name: [{}]", __FILE__, __func__, cursor_name);
		if (last_rc != SQL_SUCCESS) {
			retrieve_odbc_error(ERR_SRC_STMT);
			lib_logger->error("ODBC: Error while setting cursor name ({}) {}", last_rc, cursor_name);
			return DBERR_DECLARE_CURSOR_FAILED;
		}

		cursor->setPrivateData(last_rc == DBERR_NO_ERROR ? cursor_handle : NULL);

		std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
		if (it == _declared_cursors.end()) {
			_declared_cursors[cursor->getName()] = cursor;
		}
	}

	// Nothing to do here
	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::cursor_open(ICursor* cursor)
{
	lib_logger->trace(FMT_FILE_FUNC "ODBC: open cursor invoked", __FILE__, __func__);

	std::string query = cursor->getQuery();

	//SQLHANDLE save_handle = cur_stmt_handle;

	int rc = 0;

	if (cursor->getNumParams() > 0) {
		std::vector<std::string> params = cursor->getParameterValues();
		std::vector<int> param_types = cursor->getParameterTypes();
		rc = _odbc_exec_params(cursor, std::string(query), cursor->getNumParams(), param_types.data(), params, NULL, NULL);
	}
	else {
		rc = _odbc_exec(cursor, std::string(query));
	}


	//cur_stmt_handle = save_handle;

	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_OPEN_CURSOR_FAILED;
}

int DbInterfaceODBC::fetch_one(ICursor* cursor, int fetchmode)
{
	int res = DBERR_NO_ERROR;

	lib_logger->trace(FMT_FILE_FUNC "ODBC: fetch from cursor invoked", __FILE__, __func__);

	SQLHANDLE save_handle = cur_stmt_handle;

	cur_stmt_handle = cursor->getPrivateData();
	if (!cur_stmt_handle) {
		cur_stmt_handle = save_handle;
		lib_logger->error("ODBC: Error while fetching row from cursor: invalid ODBC cursor data");
		return DBERR_FETCH_ROW_FAILED;
	}

	std::string cname = cursor->getName();
	last_rc = SQLFetch(cur_stmt_handle);

	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_STMT);
		lib_logger->error("ODBC: Error while fetching row from cursor ({}) {}", last_rc, cname);
		res = DBERR_FETCH_ROW_FAILED;
	}

	if (dynamic_cursor_emulation && !last_rc) {
		SQLLEN reslen = 0;
		last_rc = SQLGetData(cur_stmt_handle, 1, SQL_C_CHAR, current_rowid_val, 128, &reslen);
		if (last_rc != SQL_SUCCESS)
			res = DBERR_FETCH_ROW_FAILED;
	}

	cur_stmt_handle = save_handle;
	cursor->increaseRowNum();

	return res;
}

bool DbInterfaceODBC::get_resultset_value(ICursor* cursor, int row, int col, char* bfr, int bfrlen, int *value_len)
{
	int rc = 0;
	SQLLEN reslen;
	SQLHANDLE save_handle;

	*value_len = 0;

	if (cursor) {
		save_handle = cur_stmt_handle;
		cur_stmt_handle = cursor->getPrivateData();
	}

	if (cursor && dynamic_cursor_emulation) {
		col += 1;
	}

	int len = get_data_len(cur_stmt_handle, col);
	if (len == 0) {
		lib_logger->error("zero-length column: {} {} ", row, col);
		return NULL;
	}

	last_rc = SQLGetData(cur_stmt_handle, col + 1, SQL_C_CHAR, bfr, bfrlen, &reslen);
	if (cursor) {
		cur_stmt_handle = save_handle;
	}

	if (rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_STMT);
		return NULL;
	}

	*value_len = reslen;

	return bfr;
}

int DbInterfaceODBC::move_to_first_record()
{
	lib_logger->trace(FMT_FILE_FUNC "ODBC: moving to first row in resultset", __FILE__, __func__);

	last_rc = SQLFetch(cur_stmt_handle);
	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_STMT);

		lib_logger->debug(FMT_FILE_FUNC "ODBC: Error while moving to first row in resultset", __FILE__, __func__);

		return DBERR_MOVE_TO_FIRST_FAILED;
	}

	lib_logger->debug(FMT_FILE_FUNC "ODBC: moved to first row succeeded", __FILE__, __func__);

	return DBERR_NO_ERROR;
}

int DbInterfaceODBC::supports_num_rows()
{
	return driver_has_num_rows_support;
}

int DbInterfaceODBC::get_num_rows(ICursor *crsr)
{
	SQLHANDLE* wk_rs = (SQLHANDLE*)((crsr != NULL) ? crsr->getPrivateData() : cur_stmt_handle);

	lib_logger->trace(FMT_FILE_FUNC "ODBC: getting number of rows", __FILE__, __func__);

	SQLLEN NumRows = 0;
	last_rc = SQLRowCount(wk_rs, &NumRows);
	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_STMT, wk_rs);
		lib_logger->error("ODBC: Error while getting row count");
		return DBERR_NO_DATA;
	}

	lib_logger->debug(FMT_FILE_FUNC  "ODBC: row count: {}", __FILE__, __func__, (int)NumRows);

	return (int)NumRows;
}

int DbInterfaceODBC::get_num_fields(ICursor* crsr)
{
	SQLHANDLE* wk_rs = (SQLHANDLE*)((crsr != NULL) ? crsr->getPrivateData() : cur_stmt_handle);

	SQLSMALLINT NumCols = 0;
	last_rc = SQLNumResultCols(wk_rs, &NumCols);
	if (last_rc != SQL_SUCCESS) {
		retrieve_odbc_error(ERR_SRC_STMT, wk_rs);
		lib_logger->error("ODBC: Error while getting column count");
		return DBERR_NO_DATA;
	}
	return (int)NumCols;
}

void DbInterfaceODBC::retrieve_odbc_error(int err_source, SQLHANDLE err_stmt)
{
	SQLINTEGER i = 0;
	SQLINTEGER NativeError;
	SQLCHAR SQLState[7];
	SQLCHAR MessageText[1024];
	char bfr[2000];
	SQLSMALLINT TextLength;
	SQLRETURN ret;

	SQLHANDLE handle;
	SQLSMALLINT handle_type;


	switch (err_source) {
		case ERR_SRC_ENV:
			handle = env_handle;
			handle_type = SQL_HANDLE_ENV;
			break;

		case ERR_SRC_CONN:
			handle = conn_handle;
			handle_type = SQL_HANDLE_DBC;
			break;

		case ERR_SRC_STMT:
			handle_type = SQL_HANDLE_STMT;
			handle = (err_stmt != 0) ? err_stmt : cur_stmt_handle;
			break;

		default:
			lib_logger->error("Invalid error source specified") ;
			return;
	}

	odbc_errors.clear();

	do {
		ret = SQLGetDiagRec(handle_type, handle, ++i, SQLState, &NativeError,
			MessageText, sizeof(MessageText), &TextLength);

		int ret2 = SQLGetDiagRec(SQL_HANDLE_DBC, conn_handle, ++i, SQLState, &NativeError,
			MessageText, sizeof(MessageText), &TextLength);

		if (SQL_SUCCEEDED(ret)) {
			sprintf(bfr, "{}:%ld:%ld:{}", SQLState, (long)i, (long)NativeError, MessageText);
			odbc_errors.push_back(std::string(bfr));

#if _DEBUG
			lib_logger->trace("ODBC error code: {}", bfr);
#endif
		}
	} while (ret == SQL_SUCCESS);
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
	SQLCHAR*	   ColumnData;
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
