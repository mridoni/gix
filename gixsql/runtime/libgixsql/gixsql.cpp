/*
* Copyright (C) 2021 Marco Ridoni
* Copyright (C) 2013 Tokyo System House Co.,Ltd.
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

#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cstdbool>
#include <vector>
#include <map>
#include <string>
#include <cstring>

#if (defined(_WIN32) || defined(_WIN64)) && !defined(__MINGW32__)
#include <io.h>
#else
#include <unistd.h>
#endif
#include <math.h>

#include "Connection.h"
#include "ConnectionManager.h"
#include "Cursor.h"
#include "CursorManager.h"
#include "DbInterfaceFactory.h"
#include "DataSourceInfo.h"
#include "SqlVar.h"
#include "SqlVarList.h"

#include "IDbInterface.h"
#include "IConnection.h"

#include "Logger.h"
#include "utils.h"
#include "gixsql.h"
#include "platform.h"
#include "Logger.h"

#define FAIL_ON_ERROR(_rc, _st, _dbi, _err) if (_rc != DBERR_NO_ERROR) { \
										setStatus(_st, _dbi, _err); \
										return RESULT_FAILED; \
								   }


struct query_info {
	char *pname;  // default
	char *query;
	int nParams;
};

static ConnectionManager connection_manager;
static CursorManager cursor_manager;


static void sqlca_initialize(struct sqlca_t *);
static int setStatus(struct sqlca_t * st, IDbInterface *dbi, int err);
static bool get_autocommit(DataSourceInfo *);
static std::string get_client_encoding(DataSourceInfo *);
static void init_sql_var_list(void);

/* sql var list */
SqlVarList _current_sql_var_list;
SqlVarList _res_sql_var_list;

static int _gixsqlExec(Connection *conn, struct sqlca_t *st, char *_query);
static int _gixsqlExecParams(Connection *conn, struct sqlca_t *st, char *_query, unsigned int nParams);
static int _gixsqlCursorDeclare(struct sqlca_t *st, Connection *conn, std::string connection_name, std::string cursor_name, int with_hold, char *query, int nParams);

static std::string get_hostref_or_literal(void *data, int connection_id_tl);
bool prepare_statement(const std::string &s, std::string &s_out, std::vector<std::string> &params);

DECLARE_LOGGER_STATIC(logger);

static void
sqlca_initialize(struct sqlca_t * sqlca)
{
	memcpy((char *)sqlca, (char *)&sqlca_init, sizeof(struct sqlca_t));
}

LIBGIXSQL_API int
GIXSQLConnect(struct sqlca_t *st, void *d_data_source, int data_source_tl, void *d_connection_id, int connection_id_tl,
				void *d_username, int username_tl, void *d_password, int password_tl)
{
	LOG_DEBUG(__FILE__, __func__, "GIXSQLConnect start\n");

	std::string data_source_info;
	std::string connection_id;
	std::string username;
	std::string password;

	// CONNECT ... AS
	connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);

	data_source_info = get_hostref_or_literal(d_data_source, data_source_tl);
	username = get_hostref_or_literal(d_username, username_tl);
	password = get_hostref_or_literal(d_password, password_tl);

	sqlca_initialize(st);

	if (!connection_id.empty() && connection_manager.exists(connection_id)) {
		LOG_DEBUG(__FILE__, __func__, "Connection already defined: %s", connection_id.c_str());
		setStatus(st, NULL, DBERR_NO_ERROR);
		return RESULT_FAILED;
	}

	DataSourceInfo *data_source = new DataSourceInfo();
	int rc = data_source->init(data_source_info, username, password);
	if (rc != 0) {
		LOG_ERROR("Cannot initialize connection parameters, aborting, data source is [%s]", data_source_info.c_str());
		setStatus(st, NULL, DBERR_CONN_INIT_ERROR);
		return RESULT_FAILED;
	}

	std::string dbtype = data_source->getDbType();
	IDbInterface *dbi = DbInterfaceFactory::getInterface(dbtype);
	if (dbi == NULL) {
		LOG_ERROR("Cannot determine or load database type (%s), aborting", dbtype.c_str());
		setStatus(st, NULL, DBERR_CONN_INVALID_DBTYPE);
		return RESULT_FAILED;
	}

	bool autocommit = get_autocommit(data_source);
	std::string client_encoding = get_client_encoding(data_source);

	LOG_DEBUG(__FILE__, __func__, "Connection string : %s\n", data_source->get().c_str());
	LOG_DEBUG(__FILE__, __func__, "Data source info  : %s\n", data_source->dump().c_str());
	LOG_DEBUG(__FILE__, __func__, "Autocommit        : %d\n", autocommit);

	rc = dbi->connect(data_source, autocommit, client_encoding);
	if (rc != DBERR_NO_ERROR) {
		setStatus(st, dbi, DBERR_CONNECTION_FAILED);
		return RESULT_FAILED;
	}

	if (autocommit) {
		rc = dbi->begin_transaction();
		if (rc != DBERR_NO_ERROR) {
			setStatus(st, dbi, DBERR_BEGIN_TX_FAILED);
			dbi->terminate_connection();
			return RESULT_FAILED;
		}
	}

	Connection *c = connection_manager.create();
	c->setName(connection_id);	// it might still be empty, the connection manager will assign a default name
	c->setAutoCommit(autocommit);
	c->setConnectionInfo(data_source);
	c->setEncoding(client_encoding);
	c->setDbInterface(dbi);
	c->setOpened(true);
	connection_manager.add(c);

	dbi->set_owner((IConnection *)c);

	LOG_DEBUG(__FILE__, __func__, "connection success. connectId = %d, dbname = %s\n", c->getId(), connection_id.c_str());

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLConnectReset(struct sqlca_t *st, void *d_connection_id, int connection_id_tl)
{
	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
		return RESULT_FAILED;
	}

	cursor_manager.clearConnectionCursors(conn->getId(), true);

	IDbInterface *dbi = conn->getDbInterface();
	int rc = dbi->reset();
	FAIL_ON_ERROR(rc, st, dbi, DBERR_CONN_RESET_FAILED)


	//if (conn->getAutoCommit()) {
	//	rc = dbi->begin_transaction();
	//	FAIL_ON_ERROR(rc, st, dbi, DBERR_BEGIN_TX_FAILED)
	//}

	//LOG_DEBUG(__FILE__, __func__, "connection reset success. connectId = %d\n", conn->getId());

	setStatus(st, NULL, DBERR_NO_ERROR);

	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLExec(struct sqlca_t *st, void *d_connection_id, int connection_id_tl, char *_query)
{

	LOG_DEBUG(__FILE__, __func__, "GIXSQLExec start\n");
	LOG_DEBUG(__FILE__, __func__, "GIXSQLExec SQL: %s\n", _query);

	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		LOG_ERROR("Can't find a connection");
		setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
		return RESULT_FAILED;
	}

	sqlca_initialize(st);

	if (_query == NULL || strlen(_query) == 0) {
		LOG_ERROR("Empty query");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	return _gixsqlExec(conn, st, _query);
}

LIBGIXSQL_API int
GIXSQLExecImmediate(struct sqlca_t *st, void *d_connection_id, int connection_id_tl, void *d_query, int query_tl)
{

	LOG_DEBUG(__FILE__, __func__, "GIXSQLExecImmediate start\n");

	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		LOG_ERROR("Can't find a connection");
		setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
		return RESULT_FAILED;
	}

	std::string query = get_hostref_or_literal(d_query, query_tl);

	LOG_DEBUG(__FILE__, __func__, "GIXSQLExecImmediate SQL: %s\n", query.c_str());

	sqlca_initialize(st);

	if (query.empty()) {
		LOG_ERROR("Empty query");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	return _gixsqlExec(conn, st, (char *)query.c_str());
}

static int _gixsqlExec(Connection *conn, struct sqlca_t *st, char *_query)
{
	std::string query = _query;
	int rc = 0;
	IDbInterface *dbi = conn->getDbInterface();

	if (is_commit_or_rollback_statement(query)) {
		cursor_manager.clearConnectionCursors(conn->getId(), false);

		if (conn->getAutoCommit()) {
			rc = dbi->end_transaction(query);
			FAIL_ON_ERROR(rc, st, dbi, DBERR_END_TX_FAILED)

			rc = dbi->begin_transaction();
			FAIL_ON_ERROR(rc, st, dbi, DBERR_BEGIN_TX_FAILED)
		}
		else {
			rc = dbi->exec(query);
			FAIL_ON_ERROR(rc, st, dbi, DBERR_SQL_ERROR)
		}
	}
	else {
		rc = dbi->exec(query);
		FAIL_ON_ERROR(rc, st, dbi, DBERR_SQL_ERROR)

		if (is_dml_statement(query) && conn->getAutoCommit()) {
			rc = dbi->end_transaction("COMMIT");
			FAIL_ON_ERROR(rc, st, dbi, DBERR_END_TX_FAILED)

			rc = dbi->begin_transaction();
			FAIL_ON_ERROR(rc, st, dbi, DBERR_BEGIN_TX_FAILED)
		}
	}

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLExecParams(struct sqlca_t *st, void *d_connection_id, int connection_id_tl, char *_query, int nParams)
{
	LOG_DEBUG(__FILE__, __func__, "GIXSQLExecParams start\n");

	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		LOG_ERROR("Can't find a connection");
		setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
		return RESULT_FAILED;
	}

	if (_query == NULL || strlen(_query) == 0) {
		LOG_ERROR("Empty query");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	sqlca_initialize(st);

	if (_current_sql_var_list.size() > nParams) {
		setStatus(st, NULL, DBERR_TOO_MANY_ARGUMENTS);
		return RESULT_FAILED;
	}
	else if (_current_sql_var_list.size() < nParams) {
		setStatus(st, NULL, DBERR_TOO_FEW_ARGUMENTS);
		return RESULT_FAILED;
	}


	return _gixsqlExecParams(conn, st, _query, nParams);
}

static int _gixsqlExecParams(Connection *conn, struct sqlca_t *st, char *_query, unsigned int nParams)
{
	std::vector<std::string> params;
	std::vector<int> param_types;
	std::vector<SqlVar *>::iterator it;

	// set parameters
	for (it = _current_sql_var_list.begin(); it != _current_sql_var_list.end(); it++) {
		params.push_back((*it)->getRealData());
		param_types.push_back((*it)->getType());
	}

	std::string query = _query;
	int rc = 0;
	IDbInterface *dbi = conn->getDbInterface();
	if (!dbi)
		FAIL_ON_ERROR(1, st, dbi, DBERR_SQL_ERROR)

	if (is_commit_or_rollback_statement(query)) {
		cursor_manager.clearConnectionCursors(conn->getId(), false);

		if (conn->getAutoCommit()) {
			rc = dbi->end_transaction(query);
			FAIL_ON_ERROR(rc, st, dbi, DBERR_END_TX_FAILED)

			rc = dbi->begin_transaction();
			FAIL_ON_ERROR(rc, st, dbi, DBERR_BEGIN_TX_FAILED)
		}
		else {
			rc = dbi->exec_params(query, nParams, param_types.data(), params, NULL, NULL);
			FAIL_ON_ERROR(rc, st, dbi, DBERR_SQL_ERROR)
		}
	}
	else {
		rc = dbi->exec_params(query, nParams, param_types.data(), params, NULL, NULL);
		FAIL_ON_ERROR(rc, st, dbi, DBERR_SQL_ERROR)

		if (is_dml_statement(query) && conn->getAutoCommit()) {
			rc = dbi->end_transaction(query);
			FAIL_ON_ERROR(rc, st, dbi, DBERR_END_TX_FAILED)

			rc = dbi->begin_transaction();
			FAIL_ON_ERROR(rc, st, dbi, DBERR_BEGIN_TX_FAILED)
		}
	}

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int GIXSQLExecPrepared(sqlca_t *st, void *d_connection_id, int connection_id_tl, char *stmt_name, int nParams)
{
	LOG_DEBUG(__FILE__, __func__, "GIXSQLExecPrepared start\n");

	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		LOG_ERROR("Can't find a connection");
		setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
		return RESULT_FAILED;
	}

	if (stmt_name == NULL || strlen(stmt_name) == 0) {
		LOG_ERROR("Empty statement name");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	sqlca_initialize(st);

	// TODO: check number of parameters
	//if (_current_sql_var_list.size() > nParams) {
	//	setStatus(st, NULL, DBERR_TOO_MANY_ARGUMENTS);
	//	return RESULT_FAILED;
	//}
	//else if (_current_sql_var_list.size() < nParams) {
	//	setStatus(st, NULL, DBERR_TOO_FEW_ARGUMENTS);
	//	return RESULT_FAILED;
	//}

	std::vector<std::string> params;
	std::vector<int> empty;
	std::vector<SqlVar *>::iterator it;

	// set parameters
	for (it = _current_sql_var_list.begin(); it != _current_sql_var_list.end(); it++) {
		params.push_back((*it)->getRealData());
	}

	int rc = 0;
	IDbInterface *dbi = conn->getDbInterface();
	if (!dbi)
		FAIL_ON_ERROR(1, st, dbi, DBERR_SQL_ERROR)

	rc = dbi->exec_prepared(stmt_name, params, empty, empty);
	FAIL_ON_ERROR(rc, st, dbi, DBERR_SQL_ERROR)

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLCursorDeclareParams(struct sqlca_t *st, void *d_connection_id, int connection_id_tl, char *cursor_name, int with_hold, char *_query, int nParams) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLCursorDeclareParams start for cursor [%s]\n", cursor_name);
	LOG_DEBUG(__FILE__, __func__, "SQL:#%s#\n", _query);

	int id;
	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		LOG_DEBUG(__FILE__, __func__, "connection id is not found. Cursor will be completely initialized later\n");
	}

	// check argument
	if (_query == NULL || strlen(_query) == 0 ||
		cursor_name == NULL || strlen(cursor_name) == 0) {
		LOG_ERROR("Empty query/cursor");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	// check argument
	if (nParams == 0) {
		setStatus(st, NULL, DBERR_NO_PARAMETERS);
		return RESULT_FAILED;
	}

	if (_current_sql_var_list.size() > nParams) {
		setStatus(st, NULL, DBERR_TOO_MANY_ARGUMENTS);
		return RESULT_FAILED;
	}
	else if (_current_sql_var_list.size() < nParams) {
		setStatus(st, NULL, DBERR_TOO_FEW_ARGUMENTS);
		return RESULT_FAILED;
	}

	int rc = _gixsqlCursorDeclare(st, conn, connection_id, cursor_name, with_hold, _query, nParams);

	if (!rc && !conn && d_connection_id && connection_id_tl) {
		// This means we can try to resolve it later (possibly on open), so we save the COBOL field reference
		Cursor *c = cursor_manager.get(cursor_name);
		if (c) {
			c->setConnectionReference(d_connection_id, connection_id_tl);
		}

	}

	return rc;
}


LIBGIXSQL_API int
GIXSQLCursorDeclare(struct sqlca_t *st, void *d_connection_id, int connection_id_tl, char *cursor_name, int with_hold, char *_query) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLCursorDeclare start for cursor [%s]\n", cursor_name);
	LOG_DEBUG(__FILE__, __func__, "SQL:#%s#\n", _query);

	int id;
	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		LOG_DEBUG(__FILE__, __func__, "connection id is not found. Cursor will be completely initialized later\n");
	}

	// check argument
	if (_query == NULL || strlen(_query) == 0 ||
		cursor_name == NULL || strlen(cursor_name) == 0) {
		LOG_ERROR("Empty query/cursor");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	sqlca_initialize(st);

	int rc = _gixsqlCursorDeclare(st, conn, connection_id, cursor_name, with_hold, _query, 0);

	if (!rc && !conn && d_connection_id && connection_id_tl) {
		// This means we can try to resolve it later (possibly on open), so we save the COBOL field reference
		Cursor *c = cursor_manager.get(cursor_name);
		if (c) {
			c->setConnectionReference(d_connection_id, connection_id_tl);
		}

	}
	return rc;
}

static int  _gixsqlCursorDeclare(struct sqlca_t *st, Connection *conn, std::string connection_name, std::string cursor_name, int with_hold, char *query, int nParams)
{
	if (cursor_manager.exists(cursor_name)) {
		LOG_ERROR((std::string("Cursor exists: ") + std::string(cursor_name)).c_str());
		setStatus(st, NULL, DBERR_CURSOR_EXISTS);
		return RESULT_FAILED;
	}

	Cursor *c = cursor_manager.create();
	c->setConnection((IConnection *)conn);
	c->setConnectionName(connection_name);
	c->setName(std::string(cursor_name));
	c->setQuery(std::string(query));
	c->setNumParams(nParams);
	c->setWithHold(with_hold);

	if (nParams > 0) {
		c->setParameters(_current_sql_var_list);
	}

	if (conn) {
		IDbInterface* dbi = conn->getDbInterface();
		if (dbi->cursor_declare(c, with_hold, nParams)) {
			LOG_ERROR((std::string("Invalid cursor data: ") + std::string(cursor_name)).c_str());
			setStatus(st, NULL, DBERR_DECLARE_CURSOR_FAILED);
			delete c;
			return RESULT_FAILED;
		}
	}

	cursor_manager.add(c);

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLCursorOpen(struct sqlca_t *st, char *cname)
{
	int rc = 0;
	LOG_DEBUG(__FILE__, __func__, "GIXSQLCursorOpen start for cursor [%s]\n", cname);

	sqlca_initialize(st);

	// check argument
	if (cname == NULL || strlen(cname) == 0) {
		LOG_ERROR("Empty query/cursor");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	// search cursor
	Cursor *cursor = cursor_manager.get(std::string(cname));
	if (cursor == NULL) {
		LOG_ERROR("cursor %s not registered.\n", cname);
		setStatus(st, NULL, DBERR_NO_SUCH_CURSOR);
		return RESULT_FAILED;
	}

	if (cursor->getQuery().empty()) {
		LOG_ERROR("cursor %s has an empty query.\n", cname);
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	// This was not properly declared, we have to set it up
	if (!cursor->getConnection()) {
		std::string connection_id = cursor->getConnectionName();
		IConnection *cc = (IConnection *)connection_manager.get(connection_id);
		if (cc)
			cursor->setConnection(cc);
		else {
			// try to resolve the connection id through a reference to a COBOL variable
			connection_id = cursor->getConnectionNameFromReference();
			cc = (IConnection *)connection_manager.get(connection_id);
			if (cc)
				cursor->setConnection(cc);
		}

		if (cursor->getConnection()) {
			IDbInterface* cdbi = cursor->getConnection()->getDbInterface();
			if (!cdbi || cdbi->cursor_declare(cursor, cursor->isWithHold(), cursor->getNumParams())) {
				LOG_ERROR((std::string("Invalid cursor data: ") + std::string(cname)).c_str());
				setStatus(st, NULL, DBERR_DECLARE_CURSOR_FAILED);
				return RESULT_FAILED;
			}
		}
	}

	if (!cursor->getConnection()) {
		LOG_ERROR("Cannot find an active connection for cursor %s\n", cname);
		setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
		return RESULT_FAILED;
	}

	Connection *c = (Connection *)cursor->getConnection();
	IDbInterface *dbi = c->getDbInterface();

	if (cursor->isOpen()) {
		LOG_DEBUG(__FILE__, __func__, "cursor %s alredy opened.\n", cname);
		rc = dbi->close_cursor(cursor);
		FAIL_ON_ERROR(rc, st, dbi, DBERR_CLOSE_CURSOR_FAILED)

		cursor->setOpened(false);
	}

	//if (cursor->getConnection() == NULL) {		// USE_DEFAULT_CONNECTION
	//	Connection *conn = connection_manager.current();
	//	if (conn == NULL) {
	//		LOG_ERROR("connection id is not found\n");
	//		setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
	//		return RESULT_FAILED;
	//	}
	//	cursor->setConnection((IConnection *)conn);
	//}

	rc = dbi->cursor_open(cursor);
	FAIL_ON_ERROR(rc, st, dbi, DBERR_OPEN_CURSOR_FAILED)

	cursor->setOpened(true);

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int GIXSQLCursorFetchOne(struct sqlca_t *st, char *cname) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLCursorFetchOne start\n");

	sqlca_initialize(st);

	// check argument
	if (cname == NULL || strlen(cname) == 0) {
		setStatus(st, NULL, DBERR_NO_SUCH_CURSOR);
		return RESULT_FAILED;
	}

	LOG_DEBUG(__FILE__, __func__, "cname:%s\n", cname);

	Cursor *cursor = cursor_manager.get(cname);
	if (cursor == NULL) {
		LOG_ERROR("cursor %s not registered.\n", cname);
		setStatus(st, NULL, DBERR_NO_SUCH_CURSOR);
		return RESULT_FAILED;
	}

	if (!cursor->isOpen()) {
		LOG_ERROR("cursor closed\n", cname);
		setStatus(st, NULL, DBERR_CURSOR_CLOSED);
		return RESULT_FAILED;
	}

	IDbInterface *dbi = cursor->getConnection()->getDbInterface();
	int rc = dbi->fetch_one(cursor, FETCH_NEXT_ROW);
	if (rc == DBERR_NO_DATA) {
		setStatus(st, dbi, DBERR_NO_DATA);
		return DBERR_FETCH_ROW_FAILED;
	}
	FAIL_ON_ERROR(rc, st, dbi, DBERR_FETCH_ROW_FAILED)


	int bsize = _res_sql_var_list.getMaxLength() + VARLEN_LENGTH_SZ + 1;
	char *buffer = (char *) calloc(1, bsize);
	std::vector<SqlVar *>::iterator it;
	int i = 0;
	int datalen = 0;
	for (it = _res_sql_var_list.begin(); it != _res_sql_var_list.end(); it++) {
		if (!dbi->get_resultset_value(cursor, 0, i++, buffer, bsize, &datalen)) {
			setStatus(st, dbi, DBERR_INVALID_COLUMN_DATA);
			continue;
		}

		(*it)->createCobolData(buffer, datalen);

	}
	free(buffer);

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;

}

LIBGIXSQL_API int
GIXSQLCursorClose(struct sqlca_t *st, char *cname) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLCursorClose start\n");

	sqlca_initialize(st);

	Cursor *cursor = cursor_manager.get(cname);
	if (cursor == NULL) {
		LOG_ERROR("cursor %s not registered.\n", cname);
		setStatus(st, NULL, DBERR_NO_SUCH_CURSOR);
		return RESULT_FAILED;
	}

	if (!cursor->isOpen()) {
		LOG_ERROR("cursor already closed\n", cname);
		setStatus(st, NULL, DBERR_NO_ERROR);
		return RESULT_SUCCESS;
	}

	Connection *conn = (Connection *)cursor->getConnection();
	if (conn == NULL) {
		LOG_ERROR("No connection assigned, invalid cursor\n", cname);	// warning
		setStatus(st, NULL, DBERR_NO_ERROR);
		return RESULT_SUCCESS;
	}

	IDbInterface *dbi = cursor->getConnection()->getDbInterface();
	int rc = dbi->close_cursor(cursor);

	cursor->setOpened(false);

	if (cursor_manager.exists(cname))
		cursor_manager.remove(cursor);

	FAIL_ON_ERROR(rc, st, dbi, DBERR_CLOSE_CURSOR_FAILED)

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int GIXSQLPrepareStatement(sqlca_t *st, void *d_connection_id, int connection_id_tl, char *stmt_name, void *d_statement_src, int statement_src_tl)
{
	LOG_DEBUG(__FILE__, __func__, "GIXSQLPrepareStatement start\n");
	LOG_DEBUG(__FILE__, __func__, "Statement name:#%s#\n", stmt_name);

	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		LOG_ERROR("Can't find a connection");
		setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
		return RESULT_FAILED;
	}

	std::string statement_src = get_hostref_or_literal(d_statement_src, statement_src_tl);

	// check argument
	if (stmt_name == NULL || strlen(stmt_name) == 0) {
		LOG_ERROR("Empty statement name");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	IDbInterface *dbi = conn->getDbInterface();

	if (dbi->prepare(stmt_name, statement_src)) {
		LOG_ERROR("Cannot prepare statement (2)");
		setStatus(st, NULL, DBERR_SQL_ERROR);
		return RESULT_FAILED;
	}

	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLExecSelectIntoOne(struct sqlca_t *st, void *d_connection_id, int connection_id_tl, char *_query, int nParams, int nResParams) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLExecSelectIntoOne start\n");
	LOG_DEBUG(__FILE__, __func__, "SQL:#%s#\n", _query);

	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		LOG_ERROR("Can't find a connection");
		setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
		return RESULT_FAILED;
	}


	// check argument
	if (_query == NULL || strlen(_query) == 0) {
		LOG_ERROR("Empty query/cursor");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	IDbInterface *dbi = conn->getDbInterface();

	if (nParams > 0) {
		if (_gixsqlExecParams(conn, st, _query, nParams) != RESULT_SUCCESS)
			return RESULT_FAILED;
	}
	else {
		if (_gixsqlExec(conn, st, _query) != RESULT_SUCCESS)
			return RESULT_FAILED;
	}

	int rc = dbi->move_to_first_record();
	LOG_DEBUG(__FILE__, __func__, "************ move_to_first_record: %d\n", rc);
	FAIL_ON_ERROR(rc, st, dbi, DBERR_MOVE_TO_FIRST_FAILED)

	int nfields = dbi->get_num_fields();
	if (nfields != nResParams) {
		LOG_ERROR("ResParams(%d) and fields(%d) are different\n", nResParams, nfields);
		setStatus(st, NULL, DBERR_FIELD_COUNT_MISMATCH);
		return RESULT_FAILED;
	}

	// Some drivers (notably DB2, both natevely and under ODBC, do not support SQLNumRows, so we have to check here
	if (dbi->supports_num_rows()) {

		// check numtuples
		if (dbi->get_num_rows() < 1) {
			LOG_ERROR("No data");
			setStatus(st, NULL, DBERR_NO_DATA);
			return RESULT_FAILED;
		}

		if (dbi->get_num_rows() > 1) {
			LOG_ERROR("Too much data");
			setStatus(st, NULL, DBERR_TOO_MUCH_DATA);
			return RESULT_FAILED;
		}
	}

	// set params
	int datalen = 0;
	bool has_invalid_column_data = false;
	int bsize = _res_sql_var_list.getMaxLength() + VARLEN_LENGTH_SZ + 1;
	char* buffer = (char*)calloc(1, bsize);
	for (int i = 0; i < _res_sql_var_list.size(); i++) {
		SqlVar *v = _res_sql_var_list.at(i);
		if (!dbi->get_resultset_value(NULL, 0, i, buffer, bsize, &datalen)) {
			setStatus(st, dbi, DBERR_INVALID_COLUMN_DATA);
			return RESULT_FAILED;
		}

		v->createCobolData(buffer, datalen);

#if _DEBUG
		LOG_DEBUG(__FILE__, __func__, "Result parameter %d - addr: %016llx\n", i + 1, v->getAddr());
#endif
	}
	free(buffer);

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLDisconnect(struct sqlca_t *st, void *d_connection_id, int connection_id_tl) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLDisconnect start\n");

	std::string connection_id = get_hostref_or_literal(d_connection_id, connection_id_tl);
	Connection *conn = connection_manager.get(connection_id);
	if (conn == NULL) {
		LOG_ERROR("connection is not found\n");
		return RESULT_FAILED;
	}

	cursor_manager.clearConnectionCursors(conn->getId(), true);

	IDbInterface *dbi = conn->getDbInterface();
	int rc = dbi->terminate_connection();
	FAIL_ON_ERROR(rc, st, dbi, DBERR_DISCONNECT_FAILED)

		setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}
//
//static int
//_gixsqlDisconnect(struct sqlca_t *st, int id) {
//	OCDBFinish(id);
//	return 0;
//}

LIBGIXSQL_API int
GIXSQLStartSQL(void) {
	LOG_DEBUG(__FILE__, __func__, "#begin\n");
	init_sql_var_list();
	LOG_DEBUG(__FILE__, __func__, "#end\n");
	return 0;
}

LIBGIXSQL_API int
GIXSQLSetSQLParams(int type, int length, int scale, uint32_t flags, void *addr) {
	if (type < COBOL_TYPE_MIN || type > COBOL_TYPE_MAX) {
		LOG_ERROR("invalid argument 'type': %d\n", type);
		return RESULT_FAILED;
	}

	if (length < 0) {
		LOG_ERROR("invalid argument 'length': %d\n", length);
		return RESULT_FAILED;
	}

	if (!addr) {
		LOG_ERROR("invalid argument addr is NULL\n");
		return RESULT_FAILED;
	}

	if (_current_sql_var_list.AddVar(type, length, scale, flags, addr) == NULL) {
		LOG_ERROR("fail to add SQLVARLIST\n");
		return RESULT_FAILED;
	}

	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLSetResultParams(int type, int length, int scale, uint32_t flags, void *addr) {
	if (type < COBOL_TYPE_MIN || type > COBOL_TYPE_MAX) {
		LOG_ERROR("invalid arugument 'type' for variable: %d\n", type);
		return RESULT_FAILED;
	}

	if (length < 0) {
		LOG_ERROR("invalid argument 'length': %d\n", length);
		return RESULT_FAILED;
	}

	if (!addr) {
		LOG_ERROR("invalid argument addr: NULL\n");
		return RESULT_FAILED;
	}

	if (_res_sql_var_list.AddVar(type, length, scale, flags, addr) == NULL) {
		LOG_ERROR("fail to add SQLVARLIST\n");
		return RESULT_FAILED;
	}
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLEndSQL(void) {
#if _DEBUG
	LOG_DEBUG(__FILE__, __func__, "#debug start dump var_list\n");
	_current_sql_var_list.dump();
	LOG_DEBUG(__FILE__, __func__, "#debug start dump res_list\n");
	_res_sql_var_list.dump();
	LOG_DEBUG(__FILE__, __func__, "#debug end dump list\n");
#endif
	_current_sql_var_list.clear();
	_res_sql_var_list.clear();

	return RESULT_SUCCESS;
}

static void init_sql_var_list(void)
{
	_current_sql_var_list.clear();
	_res_sql_var_list.clear();
}

static int setStatus(struct sqlca_t * st, IDbInterface *dbi, int err)
{

	sqlca_initialize(st);

	switch (err) {
		case DBERR_NO_ERROR:
			memcpy(st->sqlstate, "     ", 5);
			st->sqlcode = 0;
			memset(st->sqlerrm.sqlerrmc, ' ', 69);
			st->sqlerrm.sqlerrmc[69] = 0;
			st->sqlerrm.sqlerrml = 69;
			return RESULT_SUCCESS;

		case DBERR_CONNECTION_FAILED:
			memcpy(st->sqlstate, "08001", 5);
			break;

		case DBERR_BEGIN_TX_FAILED:
			memcpy(st->sqlstate, "3B001", 5);
			break;

		case DBERR_END_TX_FAILED:
			memcpy(st->sqlstate, "2D521", 5);
			break;

		case DBERR_CONN_NOT_FOUND:
			memcpy(st->sqlstate, "08003", 5);
			break;

		case DBERR_CONN_RESET_FAILED:
			memcpy(st->sqlstate, "08005", 5);
			break;

		case DBERR_EMPTY_QUERY:
			memcpy(st->sqlstate, "42617", 5);
			break;

		case DBERR_SQL_ERROR:
			memcpy(st->sqlstate, "42617", 5);
			break;

		case DBERR_TOO_MANY_ARGUMENTS:
			memcpy(st->sqlstate, "07001", 5);
			break;

		case DBERR_TOO_FEW_ARGUMENTS:
			memcpy(st->sqlstate, "07001", 5);
			break;

		case DBERR_NO_PARAMETERS:
			memcpy(st->sqlstate, "07002", 5);
			break;

		case DBERR_CURSOR_EXISTS:
			memcpy(st->sqlstate, "24502", 5);
			break;

		case DBERR_NO_SUCH_CURSOR:
			memcpy(st->sqlstate, "24518", 5);
			break;

		case DBERR_CLOSE_CURSOR_FAILED:
			memcpy(st->sqlstate, "42887", 5);
			break;

		case DBERR_DISCONNECT_FAILED:
			memcpy(st->sqlstate, "08006", 5);
			break;

		case DBERR_OUT_OF_MEMORY:
			memcpy(st->sqlstate, "58900", 5);
			break;

		case DBERR_DECLARE_CURSOR_FAILED:
			memcpy(st->sqlstate, "34001", 5);
			break;

		case DBERR_OPEN_CURSOR_FAILED:
			memcpy(st->sqlstate, "245F0", 5);
			break;

		case DBERR_FETCH_ROW_FAILED:
			memcpy(st->sqlstate, "24591", 5);
			break;

		case DBERR_INVALID_COLUMN_DATA:
			memcpy(st->sqlstate, "225FF", 5);
			break;

		case DBERR_CURSOR_CLOSED:
			memcpy(st->sqlstate, "24501", 5);
			break;

		case DBERR_MOVE_TO_FIRST_FAILED:
			memcpy(st->sqlstate, "020F0", 5);
			break;

		case DBERR_FIELD_COUNT_MISMATCH:
			memcpy(st->sqlstate, "42886", 5);
			break;

		case DBERR_NO_DATA:
			memcpy(st->sqlstate, "02000", 5);
			break;

		case DBERR_TOO_MUCH_DATA:
			memcpy(st->sqlstate, "22537", 5);
			break;

		default:
			memcpy(st->sqlstate, "HV000", 5);
	}

	if (dbi) {
		char* m = dbi->get_error_message();
		if (m != NULL) {
			if (strlen(m) <= 70)
				strcpy(st->sqlerrm.sqlerrmc, m);
			else {
				strncpy(st->sqlerrm.sqlerrmc, m, 69);
				st->sqlerrm.sqlerrmc[69] = 0;
			}
			st->sqlerrm.sqlerrml = strlen(st->sqlerrm.sqlerrmc);
		}

		st->sqlcode = dbi->get_error_code();
		if (st->sqlcode == 0)
			st->sqlcode = err;

		// if the driver provides an sqlstate we use it instead of the generic one above
		std::string sqlstate = dbi->get_state();
		if (!sqlstate.empty()) {
			if (sqlstate.size() > 5)
				sqlstate = sqlstate.substr(0, 5);

			memset(st->sqlstate, ' ', 5);
			memcpy(st->sqlstate, sqlstate.c_str(), sqlstate.size());
		}
	}
	else {
		st->sqlcode = err;
		switch (err) {
			case -100:
				sprintf(st->sqlerrm.sqlerrmc, "%d : %s", err,  "CONNFAIL");
				break;

			case -103:
				sprintf(st->sqlerrm.sqlerrmc, "%d : %s", err, "CONNNOTFOUND");
				break;

			case -990099:
				sprintf(st->sqlerrm.sqlerrmc, "%d : %s", err, "NOTIMPL");
				break;

			default:
				sprintf(st->sqlerrm.sqlerrmc, "%d : %s", err, err != 0 ? "Generic GIXSQL error" : "No error");
		}
		
		st->sqlerrm.sqlerrml = strlen(st->sqlerrm.sqlerrmc);
	}

	if (err == DBERR_NO_DATA)
		st->sqlcode = 100;

	return RESULT_SUCCESS;
}

static bool get_autocommit(DataSourceInfo *ds)
{
	std::map<std::string, std::string> options = ds->getOptions();
	if (options.find("autocommit") != options.end()) {
		std::string o = options["autocommit"];
		return (o == "on" || o == "1") ? GIXSQL_AUTOCOMMIT_ON : GIXSQL_AUTOCOMMIT_OFF;
	}

	char *v = getenv("GIXSQL_AUTOCOMMIT");
	if (v) {
		if (strcmp(v, "1") == 0 || strcasecmp(v, "ON") == 0)
			return GIXSQL_AUTOCOMMIT_ON;

		if (strcmp(v, "0") == 0 || strcasecmp(v, "OFF") == 0)
			return GIXSQL_AUTOCOMMIT_OFF;
	}

	return GIXSQL_AUTOCOMMIT_DEFAULT;
}

static std::string get_client_encoding(DataSourceInfo *ds)
{
	std::map<std::string, std::string> options = ds->getOptions();
	if (options.find("client_encoding") != options.end()) {
		return options["client_encoding"];
	}

	char *v = getenv("GIXSQL_CLIENT_ENCODING");
	if (v) {
		return std::string(v);
	}

	return GIXSQL_CLIENT_ENCODING_DEFAULT;
}

std::string get_hostref_or_literal(void *data, int l)
{
	if (!data)
		return std::string();

	if (!l)
		return std::string((char *)data);

	std::string s = std::string((char *)data, l);
	return trim_copy(s);
}

bool prepare_statement(const std::string &s, std::string &s_out, std::vector<std::string>& params)
{
	return false;
}
