/*
* Copyright (C) 2013 Tokyo System House Co.,Ltd.
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
#include "ConnectionString.h"
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
using namespace std;

struct query_info {
	char *pname;  // default
	char *query;
	int nParams;
};

static ConnectionManager connection_manager;
static CursorManager cursor_manager;


static void sqlca_initialize(struct sqlca_t *);
static int setStatus(struct sqlca_t * st, IDbInterface *dbi, int err);
static bool env_get_autocommit();
static void init_sql_var_list(void);

/* sql var list */
SqlVarList _current_sql_var_list;
SqlVarList _res_sql_var_list;

static int _gixsqlExec(Connection *conn, struct sqlca_t *st, char *_query);
static int _gixsqlExecParams(Connection *conn, struct sqlca_t *st, char *_query, unsigned int nParams);
static int _gixsqlCursorDeclare(struct sqlca_t *st, Connection *conn, char *cname, int with_hold, char *query, int nParams);
static int _gixsqlConnectMain(struct sqlca_t *st, string name, string user, string passwd, int type, string conndbname);

//static int _gixsqlResolveCONNID(struct sqlca_t *, char *, int);

DECLARE_LOGGER_STATIC(logger);

static void
sqlca_initialize(struct sqlca_t * sqlca)
{
	memcpy((char *)sqlca, (char *)&sqlca_init, sizeof(struct sqlca_t));
}

#ifdef _DEBUG

map<string, char *> __cobol_vars;

LIBGIXSQL_API int GIXSQLExposeCobolVar(char *name, char *value)
{
	__cobol_vars[string(name)] = value;
	return 0;
}

#endif

LIBGIXSQL_API int
GIXSQLConnect(struct sqlca_t *st, char *user, int userlen, char *passwd, int passwdlen, char *name, int namelen) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLConnect start\n");
	char *dbuser, *dbpasswd, *dbname;
	char *tmpuser, *tmppasswd, *tmpname;
	int ret;
	int dbtype = DB_SET_RUNTIME;

	tmpname = getenv("GIXSQL_DB_NAME");
	tmpuser = getenv("GIXSQL_DB_USER");
	tmppasswd = getenv("GIXSQL_DB_PASS");

	if (((dbname = trim_end(oc_strndup(name, namelen))) == NULL) ||
		(strlen(dbname) <= 0)) {
		dbname = safe_strdup(tmpname);
	}

	if (((dbuser = trim_end(oc_strndup(user, userlen))) == NULL) ||
		(strlen(dbuser) <= 0)) {
		dbuser = safe_strdup(tmpuser);
	}

	if (((dbpasswd = trim_end(oc_strndup(passwd, passwdlen))) == NULL) ||
		(strlen(dbpasswd) <= 0)) {
		dbpasswd = safe_strdup(tmppasswd);
	}

	if (strcmp(dbuser, dbpasswd) == 0 && strstr(dbuser, ".")) {
		char *p = strdup(strstr(dbpasswd, ".") + 1);
		char *u = strndup(dbuser, strlen(dbuser) - (strlen(p) + 1));
		free(dbuser);
		free(dbpasswd);
		dbuser = u;
		dbpasswd = p;
	}

	if (!dbuser || !dbpasswd || !dbname) {
		LOG_ERROR("Invalid credentials, aborting");
		return RESULT_FAILED;
	}

	LOG_DEBUG(__FILE__, __func__, ">>>> DBNAME: %s\n", dbname);
	LOG_DEBUG(__FILE__, __func__, ">>>> USR   : %s\n", dbuser);
	LOG_DEBUG(__FILE__, __func__, ">>>> PWD   : %s\n", dbpasswd);

	string theuser = dbuser;
	string thepwd = dbpasswd;
	string thename = dbname;
	string s_dbtype = "";
	size_t type_spec = thename.find(";");

	if (type_spec != string::npos) {
		s_dbtype = thename.substr(0, type_spec);
		thename = thename.substr(type_spec + 1);
	}
	else {
		char *d = getenv("GIXSQL_DB_MODE");
		if (d) {
			s_dbtype = d;
		}
	}

	if (s_dbtype == "PGSQL")
		dbtype = DB_PGSQL;
	else
		if (s_dbtype == "ODBC")
			dbtype = DB_ODBC;
		else
			if (s_dbtype == "ORACLE")
				dbtype = DB_ORACLE;
			else
				if (s_dbtype == "MYSQL")
					dbtype = DB_MYSQL;
				else
					if (s_dbtype == "MSSQL")
						dbtype = DB_MSSQL;
					else
						if (s_dbtype == "DB2")
							dbtype = DB_DB2;




	ret = _gixsqlConnectMain(st, thename, theuser, thepwd, dbtype, GIXSQL_DEFAULT_DBNAME);
	LOG_DEBUG(__FILE__, __func__, ">>>> RES   : %d\n", ret);
	if (dbname) free(dbname);
	if (dbuser) free(dbuser);
	if (dbpasswd) free(dbpasswd);

	return ret;
}

static int
_gixsqlConnectMain(struct sqlca_t *st, string name, string user, string passwd, int type, string conndbname)
{

	const char *cencoding = "UTF8";

	sqlca_initialize(st);

	if (connection_manager.exists(conndbname)) {
		LOG_DEBUG(__FILE__, __func__, "Connection already defined: %s", conndbname.c_str());
		connection_manager.setCurrent(conndbname);
		setStatus(st, NULL, DBERR_NO_ERROR);
		return RESULT_FAILED;
	}

	ConnectionString *conn_string = new ConnectionString();
	int rc = conn_string->init(name, user, passwd);
	if (rc != 0) {
		LOG_ERROR("Cannot initialize connection parameters, aborting");
		setStatus(st, NULL, DBERR_CONN_INIT_ERROR);
		return RESULT_FAILED;
	}

	bool autocommit = env_get_autocommit();

	LOG_DEBUG(__FILE__, __func__, "Connection string : %s\n", conn_string->get().c_str());
	LOG_DEBUG(__FILE__, __func__, "Autocommit        : %d\n", autocommit);

	IDbInterface *dbi = DbInterfaceFactory::getInterface(type);
	if (dbi == NULL) {
		LOG_ERROR("Cannot determine or load database type (%s), aborting", type);
		setStatus(st, NULL, DBERR_CONN_INVALID_DBTYPE);
		return RESULT_FAILED;
	}

	rc = dbi->connect(conn_string, autocommit, cencoding);
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
	c->setName(conndbname);
	c->setAutoCommit(autocommit);
	c->setConnectionInfo(conn_string);
	c->setEncoding(cencoding);
	c->setDbInterface(dbi);
	c->setOpened(true);
	connection_manager.add(c);

	dbi->set_owner((IConnection *)c);

	LOG_DEBUG(__FILE__, __func__, "connection success. connectId = %d, dbname = %s\n", c->getId(), conndbname.c_str());

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLConnectReset(struct sqlca_t *st)
{
	Connection *conn = connection_manager.current();
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
GIXSQLExec(struct sqlca_t *st, char *_query)
{

	LOG_DEBUG(__FILE__, __func__, "GIXSQLExec start\n");
	LOG_DEBUG(__FILE__, __func__, "GIXSQLExec SQL: %s\n", _query);
	Connection *conn = connection_manager.current();
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

static int _gixsqlExec(Connection *conn, struct sqlca_t *st, char *_query)
{
	string query = _query;
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
GIXSQLExecParams(struct sqlca_t *st, char *_query, int nParams)
{
	LOG_DEBUG(__FILE__, __func__, "GIXSQLExecParams start\n");
	Connection *conn = connection_manager.current();
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
	vector<string> params;
	vector<int> param_types;
	vector<SqlVar *>::iterator it;

	// set parameters
	for (it = _current_sql_var_list.begin(); it != _current_sql_var_list.end(); it++) {
		params.push_back((*it)->getRealData());
		param_types.push_back((*it)->getType());
	}


	string query = _query;
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
			rc = dbi->exec_params(query, nParams, param_types.data(), params, NULL, NULL, 0);
			FAIL_ON_ERROR(rc, st, dbi, DBERR_SQL_ERROR)
		}
	}
	else {
		rc = dbi->exec_params(query, nParams, param_types.data(), params, NULL, NULL, 0);
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

LIBGIXSQL_API int
GIXSQLCursorDeclareParams(struct sqlca_t *st, char *cname, int with_hold, char *_query, int nParams) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLCursorDeclareParams start for cursor [%s]\n", cname);
	LOG_DEBUG(__FILE__, __func__, "SQL:#%s#\n", _query);

	int id;
	Connection *conn = connection_manager.current();
	if (conn == NULL) {
		LOG_DEBUG(__FILE__, __func__, "connection id is not found. Marking cursor for use with default connection\n");
		id = USE_DEFAULT_CONNECTION;
	}
	else {
		id = conn->getId();
	}

	// check argument
	if (_query == NULL || strlen(_query) == 0 ||
		cname == NULL || strlen(cname) == 0) {
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

	return _gixsqlCursorDeclare(st, conn, cname, with_hold, _query, nParams);
}


LIBGIXSQL_API int
GIXSQLCursorDeclare(struct sqlca_t *st, char *cname, int with_hold, char *_query) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLCursorDeclare start for cursor [%s]\n", cname);
	LOG_DEBUG(__FILE__, __func__, "SQL:#%s#\n", _query);

	int id;
	Connection *conn = connection_manager.current();
	if (conn == NULL) {
		LOG_DEBUG(__FILE__, __func__, "connection id is not found. Marking cursor for use with default connection\n");
		id = USE_DEFAULT_CONNECTION;
	}
	else {
		id = conn->getId();
	}

	// check argument
	if (_query == NULL || strlen(_query) == 0 ||
		cname == NULL || strlen(cname) == 0) {
		LOG_ERROR("Empty query/cursor");
		setStatus(st, NULL, DBERR_EMPTY_QUERY);
		return RESULT_FAILED;
	}

	sqlca_initialize(st);

	return _gixsqlCursorDeclare(st, conn, cname, with_hold, _query, 0);
}

static int  _gixsqlCursorDeclare(struct sqlca_t *st, Connection *conn, char *cname, int with_hold, char *query, int nParams)
{
	if (cursor_manager.exists(cname)) {
		LOG_ERROR((string("Cursor exists: ") + string(cname)).c_str());
		setStatus(st, NULL, DBERR_CURSOR_EXISTS);
		return RESULT_FAILED;
	}

	Cursor *c = cursor_manager.create();
	c->setConnection((IConnection *)conn);
	c->setName(string(cname));
	c->setQuery(string(query));
	c->setNumParams(nParams);
	c->setWithHold(with_hold);

	if (nParams > 0) {
		c->setParameters(_current_sql_var_list);
	}

	if (conn) {
		IDbInterface* dbi = conn->getDbInterface();
		if (dbi->cursor_declare(c, with_hold, nParams)) {
			LOG_ERROR((string("Invalid cursor data: ") + string(cname)).c_str());
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
	Cursor *cursor = cursor_manager.get(string(cname));
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
		cursor->setConnection((IConnection *)connection_manager.current());
		if (cursor->getConnection()) {
			IDbInterface* cdbi = cursor->getConnection()->getDbInterface();
			if (!cdbi || cdbi->cursor_declare(cursor, cursor->isWithHold(), cursor->getNumParams())) {
				LOG_ERROR((string("Invalid cursor data: ") + string(cname)).c_str());
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

	if (cursor->getConnection() == NULL) {		// USE_DEFAULT_CONNECTION
		Connection *conn = connection_manager.current();
		if (conn == NULL) {
			LOG_ERROR("connection id is not found\n");
			setStatus(st, NULL, DBERR_CONN_NOT_FOUND);
			return RESULT_FAILED;
		}
		cursor->setConnection((IConnection *)conn);
	}


	//if (cursor->getNumParams() > 0) {

	//	SqlVarList parameters = cursor->getParameters();
	//	char **arr;
	//	if ((arr = (char **)malloc(sizeof(char *) * cursor->getNumParams())) == NULL) {
	//		LOG_ERROR("memory allocation failed.\n");
	//		setStatus(st, NULL, DBERR_OUT_OF_MEMORY);
	//		return RESULT_FAILED;
	//	}

	//	cursor->createRealDataforParameters();

	//	SqlVarList& l = cursor->getParameters();
	//	vector<SqlVar *>::iterator it;
	//	int i = 0;
	//	for (it = l.begin(); it != l.end(); it++) {
	//		SqlVar *v = *it;
	//		arr[i] = v->getRealData();
	//		LOG_DEBUG(__FILE__, __func__, "params[%d]:#%s#\n", i++, v->getRealData());
	//	}

	//	rc = dbi->cursor_declare_with_params(cursor, arr, 0, 0);

	//	free(arr);
	//}
	//else {
	//	rc = dbi->cursor_declare(cursor, 0, 0);
	//}

	//FAIL_ON_ERROR(rc, st, dbi, DBERR_DECLARE_CURSOR_FAILED)

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


	int bsize = _res_sql_var_list.getMaxLength() + 2;
	char *buffer = (char *) calloc(1, bsize);
	vector<SqlVar *>::iterator it;
	int i = 0;
	for (it = _res_sql_var_list.begin(); it != _res_sql_var_list.end(); it++) {
		if (!dbi->get_resultset_value(cursor, 0, i++, buffer, bsize)) {
			setStatus(st, dbi, DBERR_INVALID_COLUMN_DATA);
			continue;
		}
		(*it)->createCobolData(buffer);

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

LIBGIXSQL_API int
GIXSQLExecSelectIntoOne(struct sqlca_t *st, char *_query, int nParams, int nResParams) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLExecSelectIntoOne start\n");
	LOG_DEBUG(__FILE__, __func__, "SQL:#%s#\n", _query);

	Connection *conn = connection_manager.current();
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
	int bsize = _res_sql_var_list.getMaxLength() + 2;
	char* buffer = (char*)calloc(1, bsize);
	for (int i = 0; i < _res_sql_var_list.size(); i++) {
		SqlVar *v = _res_sql_var_list.at(i);
		if (!dbi->get_resultset_value(NULL, 0, i, buffer, bsize)) {
			setStatus(st, dbi, DBERR_INVALID_COLUMN_DATA);
			continue;
		}

		v->createCobolData(buffer);

#if _DEBUG
		LOG_DEBUG(__FILE__, __func__, "Result parameter %d - addr: %016llx\n", i + 1, v->getAddr());
#endif
	}
	free(buffer);

	setStatus(st, NULL, DBERR_NO_ERROR);
	return RESULT_SUCCESS;
}

LIBGIXSQL_API int
GIXSQLDisconnect(struct sqlca_t *st) {
	LOG_DEBUG(__FILE__, __func__, "GIXSQLDisconnect start\n");

	Connection *conn = connection_manager.current();
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
		return RESULT_SUCCESS;

	case DBERR_CONNECTION_FAILED:
		strcpy(st->sqlstate, "08001");
		break;

	case DBERR_BEGIN_TX_FAILED:
		strcpy(st->sqlstate, "3B001");
		break;

	case DBERR_END_TX_FAILED:
		strcpy(st->sqlstate, "2D521");
		break;

	case DBERR_CONN_NOT_FOUND:
		strcpy(st->sqlstate, "08003");
		break;

	case DBERR_CONN_RESET_FAILED:
		strcpy(st->sqlstate, "08005");
		break;

	case DBERR_EMPTY_QUERY:
		strcpy(st->sqlstate, "42617");
		break;

	case DBERR_SQL_ERROR:
		strcpy(st->sqlstate, "42617");
		break;

	case DBERR_TOO_MANY_ARGUMENTS:
		strcpy(st->sqlstate, "07001");
		break;

	case DBERR_TOO_FEW_ARGUMENTS:
		strcpy(st->sqlstate, "07001");
		break;

	case DBERR_NO_PARAMETERS:
		strcpy(st->sqlstate, "07002");
		break;

	case DBERR_CURSOR_EXISTS:
		strcpy(st->sqlstate, "24502");
		break;

	case DBERR_NO_SUCH_CURSOR:
		strcpy(st->sqlstate, "24518");
		break;

	case DBERR_CLOSE_CURSOR_FAILED:
		strcpy(st->sqlstate, "42887");
		break;

	case DBERR_DISCONNECT_FAILED:
		strcpy(st->sqlstate, "08006");
		break;

	case DBERR_OUT_OF_MEMORY:
		strcpy(st->sqlstate, "58900");
		break;

	case DBERR_DECLARE_CURSOR_FAILED:
		strcpy(st->sqlstate, "34001");
		break;

	case DBERR_OPEN_CURSOR_FAILED:
		strcpy(st->sqlstate, "245F0");
		break;

	case DBERR_FETCH_ROW_FAILED:
		strcpy(st->sqlstate, "24591");
		break;

	case DBERR_INVALID_COLUMN_DATA:
		strcpy(st->sqlstate, "225FF");
		break;

	case DBERR_CURSOR_CLOSED:
		strcpy(st->sqlstate, "24501");
		break;

	case DBERR_MOVE_TO_FIRST_FAILED:
		strcpy(st->sqlstate, "020F0");
		break;

	case DBERR_FIELD_COUNT_MISMATCH:
		strcpy(st->sqlstate, "42886");
		break;

	case DBERR_NO_DATA:
		strcpy(st->sqlstate, "02000");
		break;

	case DBERR_TOO_MUCH_DATA:
		strcpy(st->sqlstate, "22537");
		break;

	default:
		strcpy(st->sqlstate, "FFFFF");
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
	}
	else {
		st->sqlcode = err;
		sprintf(st->sqlerrm.sqlerrmc, "%s : %d", err != 0 ? "Generic GIXSQL error" : "No error", err);
		st->sqlerrm.sqlerrml = strlen(st->sqlerrm.sqlerrmc);
	}

	if (err == DBERR_NO_DATA)
		st->sqlcode = 100;

	return RESULT_SUCCESS;
}

static bool env_get_autocommit()
{
	char *v = getenv("GIXSQL_AUTOCOMMIT");
	if (v) {
		if (strcmp(v, "1") == 0 || strcasecmp(v, "ON") == 0)
			return OCDB_AUTOCOMMIT_ON;

		if (strcmp(v, "0") == 0 || strcasecmp(v, "OFF") == 0)
			return OCDB_AUTOCOMMIT_OFF;
	}

	return OCDB_AUTOCOMMIT_DEFAULT;
}
