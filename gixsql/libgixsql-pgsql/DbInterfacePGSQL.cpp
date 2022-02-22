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

#include <cstring>

#include "DbInterfacePGSQL.h"
#include "IConnection.h"
#include "Logger.h"
#include "utils.h"

#ifdef _DEBUG
#define LOG_DEBUG(file, func, format, ...)	logger->log_debug(file, func, format, ##__VA_ARGS__)
#define LOG_ERROR(format, ...)	logger->log_error(format, ##__VA_ARGS__)
#else
#define LOG_DEBUG(file, func, format, ...)	
#define LOG_ERROR(format, ...)
#endif

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

	//return (last_rc == PGRES_COMMAND_OK || last_rc == PGRES_TUPLES_OK) ? DBERR_NO_ERROR : DBERR_SQL_ERROR;
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

	//return (last_rc == PGRES_COMMAND_OK || last_rc == PGRES_TUPLES_OK) ? DBERR_NO_ERROR : DBERR_SQL_ERROR;
	if (last_rc == PGRES_COMMAND_OK || last_rc == PGRES_TUPLES_OK)
		return DBERR_NO_ERROR;
	else {
		last_rc = -(10000 + last_rc);
		return DBERR_SQL_ERROR;
	}
}


int DbInterfacePGSQL::close_cursor(ICursor *cursor)
{
	const char *query_part[] = { "CLOSE " };
	char *true_query;

	string sname = cursor->getName();
	const char *cname = sname.c_str();

	true_query = (char *)malloc((strlen(query_part[0]) +
		strlen(cname) + 1) * sizeof(char));
	if (true_query == NULL) {
		return DBERR_OUT_OF_MEMORY;
	}

	// execute query
	sprintf(true_query, "%s%s", query_part[0], cname);

	int rc = exec(string(true_query));
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_CLOSE_CURSOR_FAILED;
}

int DbInterfacePGSQL::cursor_declare(ICursor *cursor, bool with_hold, int res_type)
{
	//const char *query_part_with_hold_on[] = { "DECLARE ", " CURSOR WITH HOLD FOR " };
	//const char *query_part[] = { "DECLARE ", " CURSOR FOR " };
	//char *true_query;

	//string sname = cursor->getName();
	//string squery = cursor->getQuery();
	//char *cname = (char *)sname.c_str();
	//char *query = (char *)squery.c_str();

	//if (with_hold) {
	//	true_query = (char *)malloc((strlen(query_part_with_hold_on[0]) +
	//		strlen(cname) +
	//		strlen(query_part_with_hold_on[1]) +
	//		strlen(query) + 1) * sizeof(char));
	//	if (true_query == NULL) {
	//		return DBERR_OUT_OF_MEMORY;
	//	}

	//	// execute query
	//	sprintf(true_query, "%s%s%s%s", query_part_with_hold_on[0], cname,
	//		query_part_with_hold_on[1], query);
	//}
	//else {
	//	true_query = (char *)malloc((strlen(query_part[0]) +
	//		strlen(cname) +
	//		strlen(query_part[1]) +
	//		strlen(query) + 1) * sizeof(char));
	//	if (true_query == NULL) {
	//		return DBERR_OUT_OF_MEMORY;
	//	}

	//	// execute query
	//	sprintf(true_query, "%s%s%s%s", query_part[0], cname, query_part[1], query);
	//}

	//int rc = exec(string(true_query));
	//return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_DECLARE_CURSOR_FAILED;

	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<string, ICursor *>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;

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

	const char *query_part_with_hold_on[] = { "DECLARE ", " CURSOR WITH HOLD FOR " };
	const char *query_part[] = { "DECLARE ", " CURSOR FOR " };
	char *true_query;

	string sname = cursor->getName();
	string squery = cursor->getQuery();
	char *cname = (char *)sname.c_str();
	char *query = (char *)squery.c_str();

	if (cursor->isWithHold()) {
		true_query = (char *)malloc((strlen(query_part_with_hold_on[0]) +
			strlen(cname) +
			strlen(query_part_with_hold_on[1]) +
			strlen(query) + 1) * sizeof(char));
		if (true_query == NULL) {
			return DBERR_OUT_OF_MEMORY;
		}

		// execute query
		sprintf(true_query, "%s%s%s%s", query_part_with_hold_on[0], cname,
			query_part_with_hold_on[1], query);
	}
	else {
		true_query = (char *)malloc((strlen(query_part[0]) +
			strlen(cname) +
			strlen(query_part[1]) +
			strlen(query) + 1) * sizeof(char));
		if (true_query == NULL) {
			return DBERR_OUT_OF_MEMORY;
		}

		// execute query
		sprintf(true_query, "%s%s%s%s", query_part[0], cname, query_part[1], query);
	}

    auto pvalues = cursor->getParameterValues();
	int rc = exec_params(string(true_query), cursor->getNumParams(), NULL, pvalues, NULL, NULL);

	cursor->setOpened(rc == DBERR_NO_ERROR);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_DECLARE_CURSOR_FAILED;
}

int DbInterfacePGSQL::fetch_one(ICursor *cursor, int fetchmode)
{
	const char *query_part[] = { "FETCH ", " RELATIVE ", " FROM " };
	const char next[] = "1";
	const char current[] = "0";
	const char previous[] = "-1";
	char *true_query;

	if (owner == NULL) {
		return DBERR_CONN_NOT_FOUND;
	}

	LOG_DEBUG(__FILE__, __func__, "addr:%d, cname:%s, mode:%d\n", owner->getId(), cursor->getName().c_str(), FETCH_NEXT_ROW);

	string sname = cursor->getName();
	const char *cname = sname.c_str();

	true_query = (char *)malloc((strlen(query_part[0]) + strlen(query_part[1]) + 1 +
		strlen(query_part[2]) + strlen(cname) + 1) * sizeof(char));

	if (true_query == NULL) {
		return DBERR_OUT_OF_MEMORY;
	}

	// execute query
	if (fetchmode == FETCH_CUR_ROW) {
		sprintf(true_query, "%s%s%s%s%s",
			query_part[0], query_part[1], current, query_part[2], cname);
	}
	else if (fetchmode == FETCH_PREV_ROW) {
		sprintf(true_query, "%s%s%s%s%s",
			query_part[0], query_part[1], previous, query_part[2], cname);
	}
	else { // NEXT
		sprintf(true_query, "%s%s%s%s%s",
			query_part[0], query_part[1], next, query_part[2], cname);
	}

	last_rc = exec(string(true_query));
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
	*value_len = 0;
	size_t to_length = 0;

	auto type = PQftype(resaddr, col);
	const char *res = PQgetvalue(resaddr, row, col);
	if (res) {
		if (type != OID_TYPEA || !this->decode_binary) {
			to_length = strlen(res);
			if (to_length > bfrlen) {
				return false;
			}

			strcpy(bfr, res);
			*value_len = strlen(bfr);
		}
		else {

			unsigned char *tmp_bfr = PQunescapeBytea((const unsigned char *) res, &to_length);
			if (!tmp_bfr)
				return false;

			if (to_length > bfrlen) {
				PQfreemem(tmp_bfr);
				return false;
			}

			memset(bfr, 0, bfrlen);
			memcpy(bfr, tmp_bfr, to_length);
			PQfreemem(tmp_bfr);
			*value_len = to_length;
		}
		return true;
	}

	return false;
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

