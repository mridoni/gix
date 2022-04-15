/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2022 Marco Ridoni

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

#include <cstring>

#include "DbInterfacePGSQL.h"
#include "IConnection.h"
#include "Logger.h"
#include "utils.h"

#define OID_TYPEA	17

DbInterfacePGSQL::DbInterfacePGSQL()
{}


DbInterfacePGSQL::~DbInterfacePGSQL()
{}

int DbInterfacePGSQL::init(ILogger *_logger)
{
	owner = NULL;
	connaddr = NULL;
	resaddr = NULL;
	last_rc = 0;
#if _DEBUG
	logger = _logger;
#endif
	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::connect(IDataSourceInfo *conn_info, int autocommit, string encoding)
{
	PGconn *conn;
	string connstr;

	connaddr = NULL;
	resaddr = NULL;

	connstr = (conn_info->getDbName().empty() ? "" : "dbname=" + conn_info->getDbName() + " ") +
		(conn_info->getHost().empty() ? "" : "host=" + conn_info->getHost() + " ") +
		(conn_info->getPort() == 0 ? "" : "port=" + std::to_string(conn_info->getPort()) + " ") +
		(conn_info->getUsername().empty() ? "" : "user=" + conn_info->getUsername() + " ") +
		(conn_info->getPassword().empty() ? "" : "password=" + conn_info->getPassword() + " ");

	conn = PQconnectdb(connstr.c_str());

	if (conn == NULL) {
		return DBERR_CONNECTION_FAILED;
	}
	else if (PQstatus(conn) != CONNECTION_OK) {
		LOG_ERROR("%s\n", PQerrorMessage(conn));
		PQfinish(conn);
		return DBERR_CONNECTION_FAILED;
	}

	if (!encoding.empty()) {
		if (PQsetClientEncoding(conn, encoding.c_str())) {
			last_rc = 1;
			last_error = PQerrorMessage(conn);
			LOG_ERROR("%s\n", last_error);
			PQfinish(conn);
			return DBERR_CONNECTION_FAILED;
		}
	}

	auto opts = conn_info->getOptions();
	if (opts.find("default_schema") != opts.end()) {
		std::string default_schema = opts["default_schema"];
		if (!default_schema.empty()) {
			std::string default_schema = opts["default_schema"];
			std::string spq = "set search_path to " + default_schema;
			auto r = PQexec(conn, spq.c_str());
			auto rc = PQresultStatus(r);
			if (rc != PGRES_COMMAND_OK) {
				last_rc = rc;
				last_error = PQresultErrorMessage(r);
				LOG_ERROR("%s\n", last_error);
				PQfinish(conn);
				return DBERR_CONNECTION_FAILED;
			}
		}
	}

	if (opts.find("decode_binary") != opts.end()) {
		std::string opt_decode_binary = opts["decode_binary"];
		if (!opt_decode_binary.empty()) {
			if (opt_decode_binary == "on" || opt_decode_binary == "1") {
				this->decode_binary = DECODE_BINARY_ON;
			}

			if (opt_decode_binary == "off" || opt_decode_binary == "0") {
				this->decode_binary = DECODE_BINARY_OFF;
			}
		}
	}

	connaddr = conn;
	resaddr = NULL;

	if (owner)
		owner->setOpened(true);

	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::reset()
{
	int rc = terminate_connection();
	if (rc == DBERR_NO_ERROR)
		return DBERR_NO_ERROR;
	else
		return DBERR_CONN_RESET_FAILED;
}

int DbInterfacePGSQL::terminate_connection()
{
	if (connaddr) {
		PQfinish(connaddr);
		connaddr = NULL;
	}

	if (owner)
		owner->setOpened(false);

	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::begin_transaction()
{
	int rc = exec("BEGIN TRANSACTION");
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_BEGIN_TX_FAILED;
}

int DbInterfacePGSQL::end_transaction(string completion_type)
{
	if (completion_type != "COMMIT" && completion_type != "ROLLBACK")
		return DBERR_END_TX_FAILED;

	int rc = exec(completion_type);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_END_TX_FAILED;
}

char *DbInterfacePGSQL::get_error_message()
{
	if (resaddr != NULL)
		return PQresultErrorMessage(resaddr);
	else
		return NULL;
}

int DbInterfacePGSQL::get_error_code()
{
	return last_rc;
}

void DbInterfacePGSQL::set_owner(IConnection *conn)
{
	owner = conn;
}

IConnection *DbInterfacePGSQL::get_owner()
{
	return owner;
}

std::string vector_join(const std::vector<std::string> &v, char sep)
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

std::string pgsql_prepare(const std::string &sql)
{
	int n = 1;
	std::vector<std::string> items;
	std::vector<std::string> out_items;
	split_in_args(items, sql, true);
	for (auto item : items) {
		if (starts_with(item, "\"") || item.back() == '\'') {
			out_items.push_back(item);
			continue;
		}

		int pos = item.find('?');
		if (pos == std::string::npos) {
			out_items.push_back(item);
			continue;
		}

		std::string s;
		for (auto c : item) {
			if (c == '?')
				s += ("$" + std::to_string(n++));
			else
				s += c;
		}
		out_items.push_back(s);
	}

	return vector_join(out_items, ' ');
}

int DbInterfacePGSQL::prepare(std::string stmt_name, std::string sql)
{
	LOG_DEBUG(__FILE__, __func__, "PGSQL::prepare - SQL: %s\n", sql.c_str());
	if (prepared_stmts.find(stmt_name) != prepared_stmts.end()) {
		
		return DBERR_PREPARE_FAILED;
	}

	std::string prepared_sql = pgsql_prepare(sql);

	LOG_DEBUG(__FILE__, __func__, "PGSQL::prepare - SQL(P): %s\n", prepared_sql.c_str());

	PGresult *res = PQprepare(connaddr, stmt_name.c_str(), prepared_sql.c_str(), 0, nullptr);
	last_rc = PQresultStatus(res);
	last_error = PQresultErrorMessage(res);

	LOG_DEBUG(__FILE__, __func__, "PGSQL::prepare - res: (%d) %s\n", last_rc, last_error.c_str());

	if (last_rc != PGRES_COMMAND_OK) {
		return DBERR_PREPARE_FAILED;
	}

	//std::tuple<std::vector<std::string>, void *> t(params, nullptr);
	//prepared_stmts[stmt_name] = t;
	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::exec_prepared(std::string stmt_name, std::vector<std::string> &paramValues, std::vector<int> paramLengths, std::vector<int> paramFormats)
{

	LOG_DEBUG(__FILE__, __func__, "statement: %s\n", stmt_name.c_str());

	int nParams = paramValues.size();
	char **pvals = nullptr;

	if (paramValues.size() > 0) {
		pvals = new char *[nParams];
		for (int i = 0; i < nParams; i++) {
			pvals[i] = (char *)paramValues.at(i).c_str();
		}
	}

	if (resaddr)
		PQclear(resaddr);

	resaddr = PQexecPrepared(connaddr, stmt_name.c_str(), nParams, pvals, NULL, NULL, 0);

	last_rc = PQresultStatus(resaddr);
	last_error = PQresultErrorMessage(resaddr);

	//if (last_rc == PGRES_COMMAND_OK) {
	//	q = trim_copy(q);
	//	if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
	//		int nrows = get_num_rows();
	//		if (nrows <= 0) {
	//			last_rc = 100;
	//			return DBERR_SQL_ERROR;
	//		}
	//	}
	//}

	if (last_rc == PGRES_COMMAND_OK || last_rc == PGRES_TUPLES_OK)
		return DBERR_NO_ERROR;
	else {
		last_rc = -(10000 + last_rc);
		return DBERR_SQL_ERROR;
	}
}

int DbInterfacePGSQL::exec(string query)
{
	string q = query;
	LOG_DEBUG(__FILE__, __func__, "SQL: %s\n", q.c_str());

	if (resaddr)
		PQclear(resaddr);

	//resaddr = PQexec(connaddr, q.c_str());
	resaddr = PQexecParams(connaddr, q.c_str(), 0, NULL, NULL, NULL, NULL, 0);

	last_rc = PQresultStatus(resaddr);
	last_error = PQresultErrorMessage(resaddr);

	if (last_rc == PGRES_COMMAND_OK) {
		q = trim_copy(q);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			int nrows = get_num_rows();
			if (nrows <= 0) {
				last_rc = 100;
				return DBERR_SQL_ERROR;
			}
		}
	}

	if (last_rc == PGRES_COMMAND_OK || last_rc == PGRES_TUPLES_OK)
		return DBERR_NO_ERROR;
	else {
		last_rc = -(10000 + last_rc);
		return DBERR_SQL_ERROR;
	}

}


int DbInterfacePGSQL::exec_params(string query, int nParams, int *paramTypes, vector<string> &paramValues, int *paramLengths, int *paramFormats)
{
	string q = query;

	LOG_DEBUG(__FILE__, __func__, "SQL: %s\n", q.c_str());

	char **pvals = new char *[nParams];

	for (int i = 0; i < nParams; i++) {
		pvals[i] = (char *)paramValues.at(i).c_str();
	}

	if (resaddr)
		PQclear(resaddr);

	resaddr = PQexecParams(connaddr, q.c_str(), nParams, NULL, pvals, paramLengths, paramFormats, 0);

	free(pvals);

	last_rc = PQresultStatus(resaddr);
	last_error = PQresultErrorMessage(resaddr);

	if (last_rc == PGRES_COMMAND_OK) {
		q = trim_copy(q);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			int nrows = get_num_rows();
			if (nrows <= 0) {
				last_rc = 100;
				return DBERR_SQL_ERROR;
			}
		}
	}

	if (last_rc == PGRES_COMMAND_OK || last_rc == PGRES_TUPLES_OK)
		return DBERR_NO_ERROR;
	else {
		last_rc = -(10000 + last_rc);
		return DBERR_SQL_ERROR;
	}
}


int DbInterfacePGSQL::close_cursor(ICursor *cursor)
{
	string query = "CLOSE " + cursor->getName();
	// execute query
	int rc = exec(query);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_CLOSE_CURSOR_FAILED;
}

int DbInterfacePGSQL::cursor_declare(ICursor *cursor, bool with_hold, int res_type)
{
	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<string, ICursor *>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::cursor_declare_with_params(ICursor *cursor, char **param_values, bool with_hold, int res_type)
{
	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<string, ICursor *>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::cursor_open(ICursor *cursor)
{
	if (!cursor)
		return DBERR_OPEN_CURSOR_FAILED;

	string sname = cursor->getName();
	string squery = cursor->getQuery();
	string full_query;

	if (cursor->isWithHold()) {
		full_query = "DECLARE " + sname + " CURSOR WITH HOLD FOR " + squery;
	}
	else {
		full_query = "DECLARE " + sname + " CURSOR FOR " + squery;
	}

	// execute query
	auto pvalues = cursor->getParameterValues();
	int rc = exec_params(full_query, cursor->getNumParams(), NULL, pvalues, NULL, NULL);

	cursor->setOpened(rc == DBERR_NO_ERROR);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_DECLARE_CURSOR_FAILED;
}

int DbInterfacePGSQL::fetch_one(ICursor *cursor, int fetchmode)
{
	if (owner == NULL) {
		return DBERR_CONN_NOT_FOUND;
	}

	LOG_DEBUG(__FILE__, __func__, "addr:%d, cname:%s, mode:%d\n", owner->getId(), cursor->getName().c_str(), FETCH_NEXT_ROW);

	string sname = cursor->getName();
	string query;

	// execute query
	if (fetchmode == FETCH_CUR_ROW) {
		query = "FETCH RELATIVE 0 FROM " + sname;
	}
	else if (fetchmode == FETCH_PREV_ROW) {
		query = "FETCH RELATIVE -1 FROM " + sname;
	}
	else { // NEXT
		query = "FETCH RELATIVE 1 FROM " + sname;
	}

	last_rc = exec(query);
	if (last_rc != DBERR_NO_ERROR)
		return DBERR_SQL_ERROR;


	int ntuples = get_num_rows();
	if (ntuples < 1) {
		LOG_DEBUG(__FILE__, __func__, "TUPLES NODATA\n");
		return DBERR_NO_DATA;
	}
	else if (ntuples > 1) {
		return DBERR_TOO_MUCH_DATA;
	}

	return DBERR_NO_ERROR;
}

bool DbInterfacePGSQL::get_resultset_value(ICursor *c, int row, int col, char *bfr, int bfrlen, int *value_len)
{
	size_t to_length = 0;
	*value_len = 0;

	const char *res = PQgetvalue(resaddr, row, col);
	if (!res) {
		return false;
	}

	auto type = PQftype(resaddr, col);
	if (type != OID_TYPEA || !this->decode_binary) {
		to_length = strlen(res);
		if (to_length > bfrlen) {
			return false;
		}

		*value_len = to_length;
		memcpy(bfr, res, to_length + 1);	// copy with trailing null
		// CHECKME: Is the data right-padded with appropriate space value later?
		//          When yes it may be null-padded for binary (see below) there, too.
	}
	else {
		unsigned char *tmp_bfr = PQunescapeBytea((const unsigned char *)res, &to_length);
		if (!tmp_bfr)
			return false;

		if (to_length > bfrlen) {
			PQfreemem(tmp_bfr);
			return false;
		}

		*value_len = to_length;
		memcpy(bfr, tmp_bfr, to_length);
		if (to_length < bfrlen) {
			memset(bfr + to_length, 0, bfrlen - to_length);
		}
		PQfreemem(tmp_bfr);
	}
	return true;
}

int DbInterfacePGSQL::move_to_first_record()
{
	// Nothing to do for PostgreSQL
	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::supports_num_rows()
{
	return 1;
}

int DbInterfacePGSQL::get_num_rows()
{
	if (!resaddr)
		return -1;

	const char *c_nrows = PQcmdTuples(resaddr);
	if (!strlen(c_nrows))
		return -1;

	return atoi(c_nrows);
}

int DbInterfacePGSQL::get_num_fields()
{
	if (resaddr)
		return  PQnfields(resaddr);
	else
		return -1;
}

