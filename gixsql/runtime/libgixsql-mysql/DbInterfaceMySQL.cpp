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

#include <cstring>

#include "DbInterfaceMySQL.h"
#include "IConnection.h"
#include "Logger.h"
#include "utils.h"

// TODO: fix this
#define CLIENT_SIDE_CURSOR

DbInterfaceMySQL::DbInterfaceMySQL()
{
	connaddr = nullptr;
	last_rc = 0;
	owner = nullptr;
}


DbInterfaceMySQL::~DbInterfaceMySQL()
{}

int DbInterfaceMySQL::init(const std::shared_ptr<spdlog::logger>& _logger)
{
	connaddr = NULL;
	//current_resultset.clear();
	owner = NULL;

	auto lib_sink = _logger->sinks().at(0);
	lib_logger = std::make_shared<spdlog::logger>("libgixsql-mysql", lib_sink);
	lib_logger->set_level(_logger->level());
	lib_logger->info("libgixsql-mysql logger started");

	return DBERR_NO_ERROR;
}

int DbInterfaceMySQL::connect(IDataSourceInfo *conn_string, int autocommit, string encoding)
{
	MYSQL *conn;
	string connstr;

	connaddr = NULL;

	lib_logger->trace(FMT_FILE_FUNC "connstring: {} - autocommit: {} - encoding: {}", __FILE__, __func__,	conn_string->get(), autocommit, encoding);

	unsigned int port = conn_string->getPort() > 0 ? conn_string->getPort() : 3306;
	conn = mysql_init(NULL);
	conn = mysql_real_connect(conn, conn_string->getHost().c_str(), conn_string->getUsername().c_str(),
		conn_string->getPassword().c_str(), conn_string->getDbName().c_str(),
		port, NULL, 0); // CLIENT_MULTI_STATEMENTS?

	if (conn == NULL) {
		return DBERR_CONNECTION_FAILED;
	}

	if (!encoding.empty()) {
		string qenc = "SET NAMES " + encoding;
		mysql_real_query(conn, qenc.c_str(), qenc.size());
	}

	connaddr = conn;
	current_resultset.clear();

	if (owner)
		owner->setOpened(true);

	return DBERR_NO_ERROR;
}

int DbInterfaceMySQL::reset()
{
	int rc = terminate_connection();
	if (rc == DBERR_NO_ERROR)
		return DBERR_NO_ERROR;
	else
		return DBERR_CONN_RESET_FAILED;
}

int DbInterfaceMySQL::terminate_connection()
{
	int rc = 0;

	if (!current_resultset.clear())
		rc = mysql_errno(connaddr);

	if (connaddr && !rc) {
		mysql_close(connaddr);
		rc = mysql_errno(connaddr);
		connaddr = NULL;
	}

	if (owner)
		owner->setOpened(false);

	return (rc == 0) ? DBERR_NO_ERROR : DBERR_DISCONNECT_FAILED;
}

int DbInterfaceMySQL::begin_transaction()
{
	int rc = exec("START TRANSACTION");
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_BEGIN_TX_FAILED;
}

int DbInterfaceMySQL::end_transaction(string completion_type)
{
	if (completion_type != "COMMIT" && completion_type != "ROLLBACK")
		return DBERR_END_TX_FAILED;

	int rc = exec(completion_type);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_END_TX_FAILED;
}

char *DbInterfaceMySQL::get_error_message()
{
	return (char *)last_error.c_str();
}

int DbInterfaceMySQL::get_error_code()
{
	return last_rc;
}

std::string DbInterfaceMySQL::get_state()
{
	return last_state;
}

void DbInterfaceMySQL::set_owner(IConnection *conn)
{
	owner = conn;
}

IConnection *DbInterfaceMySQL::get_owner()
{
	return owner;
}

int DbInterfaceMySQL::prepare(std::string stmt_name, std::string sql)
{
	last_rc = DBERR_NOT_IMPL;
	last_error = "NOTIMPL";
	return DBERR_PREPARE_FAILED;
}

int DbInterfaceMySQL::exec_prepared(std::string stmt_name, std::vector<std::string> &paramValues, std::vector<int> paramLengths, std::vector<int> paramFormats)
{
	last_rc = DBERR_NOT_IMPL;
	last_error = "NOTIMPL";
	return DBERR_SQL_ERROR;
}

int DbInterfaceMySQL::_mysql_exec_params(ICursor *crsr, string query, int nParams, int *paramTypes, vector<string> &paramValues, int *paramLengths, int *paramFormats)
{
	string q = query;
	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#", __FILE__, __func__, q);

	MySQLResultsetData* wk_rs = (MySQLResultsetData*)((crsr != NULL) ? crsr->getPrivateData() : &current_resultset);
	if (!wk_rs) {
		lib_logger->debug(FMT_FILE_FUNC "Invalid resultset data", __FILE__, __func__);
		lib_logger->error("Invalid resultset data");
		return DBERR_OUT_OF_MEMORY;
	}

	wk_rs->clear();
	wk_rs->setResultsetHandle(mysql_stmt_init(connaddr));

	/*
		Prepared statements are restricted to only a few category of statements.
		The type of queries that they work on is limited to DML (INSERT, REPLACE, UPDATE, and DELETE), CREATE TABLE, and SELECT queries.
		Support for additional query types will be added in further versions, to make the prepared statements API more general.
	*/

	if (is_dml_statement(q)) {

		if (!wk_rs->hasValidHandle())
			return DBERR_OUT_OF_MEMORY;

		last_rc = mysql_stmt_prepare(wk_rs->getResultsetHandle(), q.c_str(), q.size());
		if (last_rc) {
			last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
			lib_logger->error("MySQL: Error while preparing statement ({}): {}", q, last_error);
			wk_rs->clear();
			return DBERR_SQL_ERROR;
		}

		MYSQL_BIND *bound_param_defs = (MYSQL_BIND *)calloc(sizeof(MYSQL_BIND), nParams);
		for (int i = 0; i < nParams; i++) {
			MYSQL_BIND *bound_param = &bound_param_defs[i];

			bound_param->buffer_type = MYSQL_TYPE_STRING;
			bound_param->buffer = (char *)paramValues.at(i).c_str();
			bound_param->buffer_length = paramValues.at(i).size();
		}
		mysql_stmt_bind_param(wk_rs->getResultsetHandle(), bound_param_defs);

		free(bound_param_defs);

		last_rc = mysql_stmt_execute(wk_rs->getResultsetHandle());
		if (last_rc) {
			last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
			wk_rs->clear();
			lib_logger->error("MySQL: Error while executing query ({}): {}", last_rc, q);
			
			return DBERR_SQL_ERROR;
		}

		if (mysql_stmt_field_count(wk_rs->getResultsetHandle())) {

			// Set STMT_ATTR_UPDATE_MAX_LENGTH attribute
			bool aBool = 1;
			int rc = mysql_stmt_attr_set(wk_rs->getResultsetHandle(), STMT_ATTR_UPDATE_MAX_LENGTH, &aBool);

#ifdef CLIENT_SIDE_CURSOR
			last_rc = mysql_stmt_store_result(wk_rs->getResultsetHandle());
			if (last_rc) {
				lib_logger->error("MySQL: Error while storing resultset ({})", last_rc);
				last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
				wk_rs->clear();
				return DBERR_SQL_ERROR;
			}
#endif
			if (!wk_rs->setup_buffers()) {
				lib_logger->error("MySQL: Error while initializing resultset buffers");
				last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
				last_rc = DBERR_INVALID_COLUMN_DATA;
				wk_rs->clear();
				return DBERR_SQL_ERROR;
			}
		}
	}
	else {
		last_rc = mysql_real_query(connaddr, q.c_str(), q.size());
		if (last_rc) {
			last_error = retrieve_mysql_error_message(NULL);
			lib_logger->error("MySQL: Error while executing query [{} : {}] - {}", last_rc, get_error_message(), q);
			if (wk_rs->hasValidHandle())
				last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
			wk_rs->clear();
			return DBERR_SQL_ERROR;
		}
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceMySQL::_mysql_exec(ICursor *crsr, const string query)
{
	string q = query;

	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#",  __FILE__, __func__, q);

	MySQLResultsetData* wk_rs = (MySQLResultsetData*)((crsr != NULL) ? crsr->getPrivateData() : &current_resultset);
	if (!wk_rs) {
		lib_logger->debug(FMT_FILE_FUNC "Invalid resultset data", __FILE__, __func__);
		lib_logger->error("Invalid resultset data");
		return DBERR_OUT_OF_MEMORY;
	}

	wk_rs->clear();
	wk_rs->setResultsetHandle(mysql_stmt_init(connaddr));

	/*
		Prepared statements are restricted to only a few category of statements.
		The type of queries that they work on is limited to DML (INSERT, REPLACE, UPDATE, and DELETE), CREATE TABLE, and SELECT queries.
		Support for additional query types will be added in further versions, to make the prepared statements API more general.
	*/

	if (is_dml_statement(q)) {
		if (!wk_rs->getResultsetHandle())
			return DBERR_OUT_OF_MEMORY;

		last_rc = mysql_stmt_prepare(wk_rs->getResultsetHandle(), q.c_str(), q.size());
		if (last_rc) {
			last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
			wk_rs->clear();
			return DBERR_SQL_ERROR;
		}

		last_rc = mysql_stmt_execute(wk_rs->getResultsetHandle());
		if (last_rc) {
			last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
			lib_logger->error("MySQL: Error while executing query [{} : {}] - {}", last_rc, get_error_message(), q);
			wk_rs->clear();
			return DBERR_SQL_ERROR;
		}

		if (mysql_stmt_field_count(wk_rs->getResultsetHandle())) {

			// Set STMT_ATTR_UPDATE_MAX_LENGTH attribute
			bool aBool = 1;
			int rc = mysql_stmt_attr_set(wk_rs->getResultsetHandle(), STMT_ATTR_UPDATE_MAX_LENGTH, &aBool);

#ifdef CLIENT_SIDE_CURSOR
			last_rc = mysql_stmt_store_result(wk_rs->getResultsetHandle());
			if (last_rc) {
				last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
				lib_logger->error("MySQL: Error while storing resultset ({}) - {}", last_rc, last_error);
				wk_rs->clear();
				return DBERR_SQL_ERROR;
			}
#endif
			if (!wk_rs->setup_buffers()) {
				lib_logger->error("MySQL: Error while initializing resultset buffers");
				last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
				last_rc = DBERR_INVALID_COLUMN_DATA;
				wk_rs->clear();
				return DBERR_SQL_ERROR;
			}
		}
	}
	else {
		last_rc = mysql_real_query(connaddr, q.c_str(), q.size());
		if (last_rc) {
			last_error = retrieve_mysql_error_message(NULL);
			lib_logger->error("MySQL: Error while executing query [{} : {}] - {}", last_rc, get_error_message(), q);
			if (wk_rs->hasValidHandle())
				last_error = retrieve_mysql_error_message(wk_rs->getResultsetHandle());
			wk_rs->clear();
			return DBERR_SQL_ERROR;
		}
	}

	return DBERR_NO_ERROR;
}

const char *DbInterfaceMySQL::retrieve_mysql_error_message(MYSQL_STMT *stmt)
{
	const char *err = NULL;
	if (stmt != NULL)
		err = mysql_stmt_error(stmt);
	else
		if (connaddr != NULL)
			err = mysql_error(connaddr);

	return (!err) ? "Unknown error" : err;
}

int DbInterfaceMySQL::retrieve_mysql_error_code(MYSQL_STMT *stmt)
{
	if (stmt != NULL)
		return mysql_stmt_errno(stmt);
	else
		if (connaddr != NULL)
			return mysql_errno(connaddr);
		else
			return -1;
}

int DbInterfaceMySQL::exec(string query)
{
	return _mysql_exec(NULL, query);
}


int DbInterfaceMySQL::exec_params(string query, int nParams, int *paramTypes, vector<string> &paramValues, int *paramLengths, int *paramFormats)
{
	return _mysql_exec_params(NULL, query, nParams, paramTypes, paramValues, paramLengths, paramFormats);
}


int DbInterfaceMySQL::close_cursor(ICursor *cursor)
{
	lib_logger->trace(FMT_FILE_FUNC "MySQL: close cursor invoked", __FILE__, __func__);

	if (!cursor) {
		lib_logger->error("MySQL: ERROR: invalid cursor ({})", cursor->getName());
		return DBERR_CLOSE_CURSOR_FAILED;
	}

	MySQLResultsetData *cursor_data = (MySQLResultsetData *)cursor->getPrivateData();
	if (!cursor_data) {
		lib_logger->error("MySQL: ERROR: closing uninitialized cursor ({})", cursor->getName());
		return DBERR_CLOSE_CURSOR_FAILED;
	}

	bool b = cursor_data->clear();
	delete cursor_data;

	cursor->setPrivateData(NULL);

	if (!b) {
		lib_logger->error("MySQL: Error while closing cursor ({})", cursor->getName());
		return DBERR_CLOSE_CURSOR_FAILED;
	}
	return DBERR_NO_ERROR;
}

int DbInterfaceMySQL::cursor_declare(ICursor *cursor, bool with_hold, int res_type)
{
	lib_logger->trace(FMT_FILE_FUNC "MySQL: cursor declare invoked", __FILE__, __func__);

	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it != _declared_cursors.end()) {
		lib_logger->trace(FMT_FILE_FUNC "MySQL: cursor already exists: {}", __FILE__, __func__, cursor->getName());
		return DBERR_CURSOR_EXISTS;
	}

	//MYSQL_STMT *wk_mysql_stmt = mysql_stmt_init(connaddr);
	//if (!wk_mysql_stmt) {
	//	last_rc = retrieve_mysql_error_code(NULL);
	//	last_error = retrieve_mysql_error_message(NULL);
	//	return DBERR_DECLARE_CURSOR_FAILED;
	//}

	MySQLResultsetData *cursor_data = new MySQLResultsetData();
	//current_resultset.setResultsetHandle(wk_mysql_stmt);

	cursor->setPrivateData(cursor_data);
	_declared_cursors[cursor->getName()] = cursor;

	return DBERR_NO_ERROR;
}

int DbInterfaceMySQL::cursor_declare_with_params(ICursor *cursor, char **param_values, bool with_hold, int res_type)
{
	lib_logger->trace(FMT_FILE_FUNC "MySQL: cursor declare invoked", __FILE__, __func__);
	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it != _declared_cursors.end()) {
		lib_logger->trace(FMT_FILE_FUNC "MySQL: cursor already exists: {}", __FILE__, __func__, cursor->getName());
		return DBERR_CURSOR_EXISTS;
	}

	MYSQL_STMT * wk_mysql_stmt = mysql_stmt_init(connaddr);
	if (!wk_mysql_stmt) {
		last_rc = retrieve_mysql_error_code(NULL);
		last_error = retrieve_mysql_error_message(NULL);
		return DBERR_DECLARE_CURSOR_FAILED;
	}

	MySQLResultsetData *cursor_data = new MySQLResultsetData();
	current_resultset.setResultsetHandle(wk_mysql_stmt);

	cursor->setPrivateData(cursor_data);
	_declared_cursors[cursor->getName()] = cursor;

	return DBERR_NO_ERROR;
}

int DbInterfaceMySQL::cursor_open(ICursor *cursor)
{
	lib_logger->trace(FMT_FILE_FUNC "MySQL: open cursor invoked", __FILE__, __func__);
	if (!cursor) {
		lib_logger->error("Invalid cursor");
		return DBERR_OPEN_CURSOR_FAILED;
	}

	if (_declared_cursors.find(cursor->getName()) == _declared_cursors.end()) {
		lib_logger->error("Invalid cursor: {}", cursor->getName());
		return DBERR_OPEN_CURSOR_FAILED;
	}

	string query = cursor->getQuery();

	if (cursor->getNumParams() > 0) {
		vector<string> params = cursor->getParameterValues();

		int rc = _mysql_exec_params(cursor, string(query), cursor->getNumParams(), NULL, params, NULL, NULL);
		return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_OPEN_CURSOR_FAILED;
	}
	else {
		int rc = _mysql_exec(cursor, string(query));
		return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_OPEN_CURSOR_FAILED;
	}
}

int DbInterfaceMySQL::fetch_one(ICursor *cursor, int fetchmode)
{
	lib_logger->trace(FMT_FILE_FUNC "MySQL: fetch from cursor invoked", __FILE__, __func__);

	if (!cursor) {
		lib_logger->error("MySQL: ERROR: invalid cursor");
		return DBERR_FETCH_ROW_FAILED;
	}

	MySQLResultsetData *cursor_resultset = (MySQLResultsetData *)cursor->getPrivateData();;
	if (!cursor_resultset) {
		lib_logger->error("MySQL: ERROR: invalid cursor resultset ({})", cursor->getName());
		return DBERR_FETCH_ROW_FAILED;
	}

	if (!cursor_resultset->hasValidHandle()) {
		lib_logger->error("MySQL: ERROR: invalid cursor resultset ({})", cursor->getName());
		return DBERR_FETCH_ROW_FAILED;
	}

	int ncols = cursor_resultset->getColumnCount();
	if (ncols <= 0) {
		lib_logger->error("MySQL: ERROR: invalid column count for cursor ({})", cursor->getName());
		return DBERR_FETCH_ROW_FAILED;
	}

	MYSQL_BIND *bound_res_cols = (MYSQL_BIND *)calloc(sizeof(MYSQL_BIND), ncols);
	for (int i = 0; i < ncols; i++) {
		MYSQL_BIND *bound_res_col = &bound_res_cols[i];
		bound_res_col->buffer_type = MYSQL_TYPE_STRING;
		bound_res_col->buffer = cursor_resultset->data_buffers.at(i);
		bound_res_col->buffer_length = cursor_resultset->data_buffer_lengths.at(i) + 1;
	}

	int rc = mysql_stmt_bind_result(cursor_resultset->getResultsetHandle(), bound_res_cols);
	if (rc) {
		last_error = retrieve_mysql_error_message(cursor_resultset->getResultsetHandle());
		lib_logger->error("MySQL: Error while binding resultset row for cursor ({} : {}) {}", get_error_code(), get_error_message(), cursor->getName().c_str());
		return DBERR_FETCH_ROW_FAILED;
	}

	last_rc = mysql_stmt_fetch(cursor_resultset->getResultsetHandle());
	if (last_rc) {
		last_error = (last_rc == MYSQL_DATA_TRUNCATED) ? "Data truncated" : retrieve_mysql_error_message(cursor_resultset->getResultsetHandle());
		lib_logger->error("MySQL: Error while fetching row from cursor ({} : {}) {}", get_error_code(), get_error_message(), cursor->getName().c_str());
		return DBERR_FETCH_ROW_FAILED;
	}

	return DBERR_NO_ERROR;
}

bool DbInterfaceMySQL::get_resultset_value(ICursor *cursor, int row, int col, char *bfr, int bfrlen, int *value_len)
{
	*value_len = 0;

	MySQLResultsetData *wk_rs = (MySQLResultsetData *)((cursor != NULL) ? cursor->getPrivateData() : &current_resultset);

	if (col < wk_rs->data_buffers.size()) {
		char *data = wk_rs->data_buffers.at(col);
		unsigned long datalen = *(wk_rs->data_lengths.at(col));
		if (datalen > bfrlen) {
			lib_logger->error("MySQL: ERROR: data truncated: needed {} bytes, {} allocated", datalen, bfrlen);	// was just a warning
			return false;
		}

		strcpy(bfr, wk_rs->data_buffers.at(col));
		*value_len = strlen(bfr);

		return true;
	}
	else {
		lib_logger->error("MySQL: invalid column index: {}, max: {}", col, wk_rs->data_buffers.size() - 1);
		return false;
	}
}

int DbInterfaceMySQL::move_to_first_record()
{
	lib_logger->trace(FMT_FILE_FUNC  "MySQL: moving to first row in resultset", __FILE__, __func__);

	if (!current_resultset.hasValidHandle()) {
		lib_logger->error("MySQL: ERROR: invalid statement resultset");
		return DBERR_MOVE_TO_FIRST_FAILED;
	}

	int ncols = current_resultset.getColumnCount();
	if (ncols <= 0) {
		lib_logger->error("MySQL: ERROR: invalid column count for statement");
		return DBERR_MOVE_TO_FIRST_FAILED;
	}

	MYSQL_BIND *bound_res_cols = (MYSQL_BIND *)calloc(sizeof(MYSQL_BIND), ncols);
	for (int i = 0; i < ncols; i++) {
		MYSQL_BIND *bound_res_col = &bound_res_cols[i];
		bound_res_col->buffer_type = MYSQL_TYPE_STRING;
		bound_res_col->buffer = current_resultset.data_buffers.at(i);
		bound_res_col->buffer_length = current_resultset.data_buffer_lengths.at(i) + 1;
		bound_res_col->length = current_resultset.data_lengths.at(i);
	}

	int rc = mysql_stmt_bind_result(current_resultset.getResultsetHandle(), bound_res_cols);
	if (rc) {
		lib_logger->error("MySQL: Error while binding resultset ({}) : {}", get_error_code(), get_error_message());
		return DBERR_MOVE_TO_FIRST_FAILED;
	}

	last_rc = mysql_stmt_fetch(current_resultset.getResultsetHandle());
	if (last_rc) {
		last_error = retrieve_mysql_error_message(current_resultset.getResultsetHandle());
		lib_logger->error("MySQL: Error while moving to first row of current resultset ({}) : {}", get_error_code(), get_error_message());
		return DBERR_MOVE_TO_FIRST_FAILED;
	}

	return DBERR_NO_ERROR;
}

int DbInterfaceMySQL::supports_num_rows()
{
	return 1;
}

int DbInterfaceMySQL::get_num_rows(ICursor* crsr)
{
	MySQLResultsetData* wk_rs = (MySQLResultsetData*)((crsr != NULL) ? crsr->getPrivateData() : &current_resultset);

	if (wk_rs && wk_rs->hasValidHandle())
		return  (int)mysql_stmt_num_rows(wk_rs->getResultsetHandle());
	else
		return -1;
}

int DbInterfaceMySQL::get_num_fields(ICursor* crsr)
{
	MySQLResultsetData* wk_rs = (MySQLResultsetData*)((crsr != NULL) ? crsr->getPrivateData() : &current_resultset);

	if (wk_rs && wk_rs->hasValidHandle())
		return  mysql_stmt_field_count(wk_rs->getResultsetHandle());
	else
		return -1;
}

MySQLResultsetData::MySQLResultsetData()
{
	_mysql_stmt = nullptr;
}

MySQLResultsetData::~MySQLResultsetData()
{
	clear();
}

bool MySQLResultsetData::setup_buffers()
{
	if (!_mysql_stmt)
		return false;

	MYSQL_RES *metadata = mysql_stmt_result_metadata(_mysql_stmt);

	clear_buffers();

	if (!metadata) {
		return false;
	}

	unsigned int field_count = mysql_stmt_field_count(_mysql_stmt);

	for (unsigned int i = 0; i < field_count; i++) {
		MYSQL_FIELD *f = &metadata->fields[i];
		int len = f->max_length + 1;
		char *bfr = (char *)calloc(len, 1);
		data_buffers.push_back(bfr);
		data_buffer_lengths.push_back(len);

		unsigned long *dl = (unsigned long *)calloc(1, sizeof(int));
		data_lengths.push_back(dl);
	}

	mysql_free_result(metadata);

	return true;
}

bool MySQLResultsetData::clear()
{
	bool b = true;
	if (_mysql_stmt)
		b = mysql_stmt_close(_mysql_stmt);

	this->_mysql_stmt = NULL;
	clear_buffers();

	return b;
}

int MySQLResultsetData::getColumnCount()
{
	return data_buffers.size();
}

void MySQLResultsetData::setResultsetHandle(MYSQL_STMT* m)
{
	_mysql_stmt = m;
}

MYSQL_STMT* MySQLResultsetData::getResultsetHandle()
{
	return _mysql_stmt;
}

bool MySQLResultsetData::hasValidHandle()
{
	return _mysql_stmt != nullptr;
}

void MySQLResultsetData::clear_buffers()
{
	for (int i = 0; i < data_buffers.size(); i++) {
		if (data_buffers.at(i))
			free(data_buffers.at(i));
	}
	data_buffers.clear();
	data_buffer_lengths.clear();
}
