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


#include "Logger.h"
#include "DbInterfacePGSQL.h"
#include "IConnection.h"
#include "utils.h"

#define OID_TYPEA	17

std::string get_hostref_or_literal(void* data, int l)
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

	std::string t = std::string((char*)actual_data, -l);
	return trim_copy(t);
}

std::string pg_get_sqlstate(PGresult* r)
{
	char* c = PQresultErrorField(r, PG_DIAG_SQLSTATE);
	if (c)
		return std::string(c);
	else
		return "";

}

DbInterfacePGSQL::DbInterfacePGSQL()
{}

DbInterfacePGSQL::~DbInterfacePGSQL()
{}

int DbInterfacePGSQL::init(const std::shared_ptr<spdlog::logger>& _logger)
{
	owner = NULL;
	connaddr = NULL;
	current_resultset = NULL;
	last_rc = 0;
	last_state = "";

	auto lib_sink = _logger->sinks().at(0);
	lib_logger = std::make_shared<spdlog::logger>("libgixsql-pgsql", lib_sink);
	lib_logger->set_level(_logger->level());
	lib_logger->info("libgixsql-pgsql logger started");
	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::connect(IDataSourceInfo* conn_info, int autocommit, std::string encoding)
{
	PGconn* conn;
	std::string connstr;

	lib_logger->trace(FMT_FILE_FUNC "PGSQL::connect - autocommit: {:d}, encoding: {}", __FILE__, __func__, autocommit, encoding);

	connaddr = NULL;
	current_resultset = NULL;

	last_rc = 0;
	last_error = "";
	last_state = "";


	//connstr = (conn_info->getDbName().empty() ? "" : "dbname=" + conn_info->getDbName() + " ") +
	//	(conn_info->getHost().empty() ? "" : "host=" + conn_info->getHost() + " ") +
	//	(conn_info->getPort() == 0 ? "" : "port=" + std::to_string(conn_info->getPort()) + " ") +
	//	(conn_info->getUsername().empty() ? "" : "user=" + conn_info->getUsername() + " ") +
	//	(conn_info->getPassword().empty() ? "" : "password=" + conn_info->getPassword() + " ");

	connstr = "dbname=" + (conn_info->getDbName().empty() ? "''" :  conn_info->getDbName() + " ") +
			  "host=" + (conn_info->getHost().empty() ? "''" : conn_info->getHost() + " ") +
			  "port=" + (conn_info->getPort() == 0 ? "''" : std::to_string(conn_info->getPort()) + " ") +
			  "user=" + (conn_info->getUsername().empty() ? "''" : conn_info->getUsername() + " ") +
			  "password=" + (conn_info->getPassword().empty() ? "''" : conn_info->getPassword());

	lib_logger->trace("libpq - connection string: [{}]", connstr);

	conn = PQconnectdb(connstr.c_str());

	if (conn == NULL) {
		last_error = "Connection failed";
		last_rc = DBERR_CONNECTION_FAILED;
		return DBERR_CONNECTION_FAILED;
	}
	else if (PQstatus(conn) != CONNECTION_OK) {
		last_error = PQerrorMessage(conn);
		last_rc = PQstatus(conn);
		lib_logger->error("libpq: {}", last_error);
		PQfinish(conn);
		return DBERR_CONNECTION_FAILED;
	}

	if (!encoding.empty()) {
		if (PQsetClientEncoding(conn, encoding.c_str())) {
			last_rc = 1;
			last_error = PQerrorMessage(conn);
			lib_logger->error("libpq: {}", last_error);
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
				last_state = pg_get_sqlstate(r);
				lib_logger->error("libpq: {}", last_error);
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

	if (current_resultset)
		PQclear(current_resultset);

	if (owner)
		owner->setOpened(false);

	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::begin_transaction()
{
	int rc = exec("BEGIN TRANSACTION");
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_BEGIN_TX_FAILED;
}

int DbInterfacePGSQL::end_transaction(std::string completion_type)
{
	if (completion_type != "COMMIT" && completion_type != "ROLLBACK")
		return DBERR_END_TX_FAILED;

	int rc = exec(completion_type);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_END_TX_FAILED;
}

char* DbInterfacePGSQL::get_error_message()
{
	if (current_resultset != NULL)
		return PQresultErrorMessage(current_resultset);
	else
		if (connaddr != NULL)
			return PQerrorMessage(connaddr);
		else
			return NULL;
}

int DbInterfacePGSQL::get_error_code()
{
	return last_rc;
}

std::string DbInterfacePGSQL::get_state()
{
	return last_state;
}

void DbInterfacePGSQL::set_owner(IConnection* conn)
{
	owner = conn;
}

IConnection* DbInterfacePGSQL::get_owner()
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

std::string pgsql_prepare(const std::string& sql)
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

		size_t pos = item.find('?');
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
	lib_logger->trace(FMT_FILE_FUNC "PGSQL::prepare ({}) - SQL: {}", __FILE__, __func__, stmt_name, sql);

	if (prepared_stmts.find(stmt_name) != prepared_stmts.end()) {
		return DBERR_PREPARE_FAILED;
	}

	std::string prepared_sql = pgsql_prepare(sql);

	lib_logger->trace(FMT_FILE_FUNC "PGSQL::prepare ({}) - SQL(P): {}", __FILE__, __func__, stmt_name, prepared_sql);

	PGresult* res = PQprepare(connaddr, stmt_name.c_str(), prepared_sql.c_str(), 0, nullptr);
	last_rc = PQresultStatus(res);
	last_error = PQresultErrorMessage(res);
	last_state = pg_get_sqlstate(res);

	lib_logger->trace(FMT_FILE_FUNC "PGSQL::prepare ({} - res: ({}) {}", __FILE__, __func__, stmt_name, last_rc, last_error);

	if (last_rc != PGRES_COMMAND_OK) {
		return DBERR_PREPARE_FAILED;
	}

	//std::tuple<std::vector<std::string>, void *> t(params, nullptr);
	//prepared_stmts[stmt_name] = t;
	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::exec_prepared(std::string stmt_name, std::vector<std::string>& paramValues, std::vector<int> paramLengths, std::vector<int> paramFormats)
{

	lib_logger->trace(FMT_FILE_FUNC "statement name: {}", __FILE__, __func__, stmt_name);

	int nParams = (int)paramValues.size();
	char** pvals = nullptr;

	if (paramValues.size() > 0) {
		pvals = new char* [nParams];
		for (int i = 0; i < nParams; i++) {
			pvals[i] = (char*)paramValues.at(i).c_str();
		}
	}

	if (current_resultset)
		PQclear(current_resultset);

	current_resultset = PQexecPrepared(connaddr, stmt_name.c_str(), nParams, pvals, NULL, NULL, 0);

	last_rc = PQresultStatus(current_resultset);
	last_error = PQresultErrorMessage(current_resultset);
	last_state = pg_get_sqlstate(current_resultset);

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

int DbInterfacePGSQL::exec(std::string query)
{
	return _pgsql_exec(nullptr, query);
}

int DbInterfacePGSQL::_pgsql_exec(ICursor *crsr, std::string query)
{
	std::string q = query;
	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#", __FILE__, __func__, q);

	PGresult* wk_rs = (PGresult*)((crsr != NULL) ? crsr->getPrivateData() : current_resultset);

	if (wk_rs) {
		if (wk_rs == current_resultset)
			current_resultset = nullptr;

		PQclear(wk_rs);
	}

	wk_rs = PQexecParams(connaddr, q.c_str(), 0, NULL, NULL, NULL, NULL, 0);

	last_rc = PQresultStatus(wk_rs);
	last_error = PQresultErrorMessage(wk_rs);
	last_state = pg_get_sqlstate(wk_rs);

	if (last_rc == PGRES_COMMAND_OK) {
		q = trim_copy(q);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			int nrows = get_num_rows(wk_rs);
			if (nrows <= 0) {
				last_rc = 100;
				return DBERR_SQL_ERROR;
			}
		}
	}

	if (last_rc == PGRES_COMMAND_OK || last_rc == PGRES_TUPLES_OK) {
		if (crsr)
			crsr->setPrivateData(wk_rs);
		else
			current_resultset = wk_rs;

		return DBERR_NO_ERROR;
	}
	else {
		last_rc = -(10000 + last_rc);
		return DBERR_SQL_ERROR;
	}

}

bool DbInterfacePGSQL::retrieve_prepared_statement_source(const std::string& prep_stmt_name, std::string& src)
{
	lib_logger->trace(FMT_FILE_FUNC "Retrieving SQL source for prepared statement {}", __FILE__, __func__, prep_stmt_name);

	char** pvals = new char* [1];
	pvals[0] = (char*)prep_stmt_name.c_str();
	std::string qry = "select statement from pg_prepared_statements where name = $1";
	PGresult * tr = PQexecParams(connaddr, qry.c_str(), 1, NULL, pvals, NULL, NULL, 0);

	delete[] pvals;

	last_rc = PQresultStatus(tr);
	last_error = PQresultErrorMessage(tr);
	last_state = pg_get_sqlstate(tr);

	if (last_rc == PGRES_TUPLES_OK) {
		const char* res = PQgetvalue(tr, 0, 0);
		if (!res) {
			last_rc = PQresultStatus(tr);
			last_error = PQresultErrorMessage(tr);
			last_state = pg_get_sqlstate(tr);
			return false;
		}
		src = res;
		return true;
	}

	return false;
}

int DbInterfacePGSQL::exec_params(std::string query, int nParams, int* paramTypes, std::vector<std::string>& paramValues, int* paramLengths, int* paramFormats)
{
	return _pgsql_exec_params(nullptr, query, nParams, paramTypes, paramValues, paramLengths, paramFormats);
}


int DbInterfacePGSQL::_pgsql_exec_params(ICursor* crsr, std::string query, int nParams, int* paramTypes, std::vector<std::string>& paramValues, int* paramLengths, int* paramFormats)
{
	std::string q = query;

	lib_logger->trace(FMT_FILE_FUNC "SQL: #{}#", __FILE__, __func__, q);

	PGresult* wk_rs = (PGresult*)((crsr != NULL) ? crsr->getPrivateData() : current_resultset);

	char** pvals = new char* [nParams];

	for (int i = 0; i < nParams; i++) {
		pvals[i] = (char*)paramValues.at(i).c_str();
	}

	if (wk_rs) {
		if (wk_rs == current_resultset)
			current_resultset = nullptr;

		PQclear(wk_rs);
	}

	wk_rs = PQexecParams(connaddr, q.c_str(), nParams, NULL, pvals, paramLengths, paramFormats, 0);

	delete[] pvals;

	last_rc = PQresultStatus(wk_rs);
	last_error = PQresultErrorMessage(wk_rs);
	last_state = pg_get_sqlstate(wk_rs);

	if (last_rc == PGRES_COMMAND_OK) {
		q = trim_copy(q);
		if (starts_with(q, "delete ") || starts_with(q, "DELETE ") || starts_with(q, "update ") || starts_with(q, "UPDATE ")) {
			int nrows = get_num_rows(wk_rs);
			if (nrows <= 0) {
				last_rc = 100;
				return DBERR_SQL_ERROR;
			}
		}
	}

	if (last_rc == PGRES_COMMAND_OK || last_rc == PGRES_TUPLES_OK) {
		if (crsr)
			crsr->setPrivateData(wk_rs);
		else
			current_resultset = wk_rs;

		return DBERR_NO_ERROR;
	}
	else {
		last_rc = -(10000 + last_rc);
		return DBERR_SQL_ERROR;
	}
}


int DbInterfacePGSQL::close_cursor(ICursor* cursor)
{
	std::string query = "CLOSE " + cursor->getName();
	// execute query
	int rc = exec(query);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_CLOSE_CURSOR_FAILED;
}

int DbInterfacePGSQL::cursor_declare(ICursor* cursor, bool with_hold, int res_type)
{
	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::cursor_declare_with_params(ICursor* cursor, char** param_values, bool with_hold, int res_type)
{
	if (!cursor)
		return DBERR_DECLARE_CURSOR_FAILED;

	std::map<std::string, ICursor*>::iterator it = _declared_cursors.find(cursor->getName());
	if (it == _declared_cursors.end()) {
		_declared_cursors[cursor->getName()] = cursor;
	}

	return DBERR_NO_ERROR;
}

int DbInterfacePGSQL::cursor_open(ICursor* cursor)
{
	if (!cursor)
		return DBERR_OPEN_CURSOR_FAILED;

	std::string sname = cursor->getName();
	std::string full_query;

	std::string squery = cursor->getQuery();
	void* src_addr = nullptr;
	int src_len = 0;

	if (squery.size() == 0) {
		cursor->getQuerySource(&src_addr, &src_len);
		squery = get_hostref_or_literal(src_addr, src_len);
	}

	if (starts_with(squery, "@")) {
		if (!retrieve_prepared_statement_source(squery.substr(1), squery)) {
			// last_error, etc. set by retrieve_prepared_statement_source
			return DBERR_OPEN_CURSOR_FAILED;
		}
	}

	if (squery.empty()) {
		last_rc = -1;
		last_error = "Empty query";
		return DBERR_OPEN_CURSOR_FAILED;
	}

	if (cursor->isWithHold()) {
		full_query = "DECLARE " + sname + " CURSOR WITH HOLD FOR " + squery;
	}
	else {
		full_query = "DECLARE " + sname + " CURSOR FOR " + squery;
	}

	// execute query
	auto pvalues = cursor->getParameterValues();
	int rc = _pgsql_exec_params(cursor, full_query, cursor->getNumParams(), NULL, pvalues, NULL, NULL);

	cursor->setOpened(rc == DBERR_NO_ERROR);
	return (rc == DBERR_NO_ERROR) ? DBERR_NO_ERROR : DBERR_OPEN_CURSOR_FAILED;
}

int DbInterfacePGSQL::fetch_one(ICursor* cursor, int fetchmode)
{
	if (owner == NULL) {
		return DBERR_CONN_NOT_FOUND;
	}

	lib_logger->trace(FMT_FILE_FUNC "owner id: {}, cursor name: {}, mode: {}", __FILE__, __func__, owner->getId(), cursor->getName(), FETCH_NEXT_ROW);

	std::string sname = cursor->getName();
	std::string query;

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

	last_rc = _pgsql_exec(cursor, query);
	if (last_rc != DBERR_NO_ERROR)
		return DBERR_SQL_ERROR;

	int ntuples = get_num_rows(cursor);
	if (ntuples < 1) {
		lib_logger->trace(FMT_FILE_FUNC "TUPLES NODATA", __FILE__, __func__);
		return DBERR_NO_DATA;
	}
	else if (ntuples > 1) {
		return DBERR_TOO_MUCH_DATA;
	}

	return DBERR_NO_ERROR;
}

bool DbInterfacePGSQL::get_resultset_value(ICursor* c, int row, int col, char* bfr, int bfrlen, int* value_len)
{
	size_t to_length = 0;
	*value_len = 0;

	PGresult *wk_rs = (c != nullptr) ? ((PGresult*)c->getPrivateData()) : current_resultset;

	const char* res = PQgetvalue(wk_rs, row, col);
	if (!res) {
		return false;
	}

	auto type = PQftype(wk_rs, col);
	if (type != OID_TYPEA || !this->decode_binary) {
		to_length = strlen(res);
		if (to_length > bfrlen) {
			return false;
		}

		*value_len = (int)to_length;
		memcpy(bfr, res, to_length + 1);	// copy with trailing null
		// CHECKME: Is the data right-padded with appropriate space value later?
		//          When yes it may be null-padded for binary (see below) there, too.
	}
	else {
		unsigned char* tmp_bfr = PQunescapeBytea((const unsigned char*)res, &to_length);
		if (!tmp_bfr)
			return false;

		if (to_length > bfrlen) {
			PQfreemem(tmp_bfr);
			return false;
		}

		*value_len = (int)to_length;
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

int DbInterfacePGSQL::get_num_rows(ICursor* crsr)
{
	PGresult* wk_rs = (PGresult*)((crsr != NULL) ? crsr->getPrivateData() : current_resultset);

	if (!wk_rs)
		return -1;

	char* c = PQcmdTuples(wk_rs);
	if (!c)
		return -1;

	int n = atoi(c);
	return n;
}

int DbInterfacePGSQL::get_num_fields(ICursor* crsr)
{
	PGresult* wk_rs = (PGresult*)((crsr != NULL) ? crsr->getPrivateData() : current_resultset);

	if (wk_rs)
		return PQnfields(wk_rs);
	else
		return -1;
}

int DbInterfacePGSQL::get_num_rows(PGresult* r)
{
	if (!r)
		return -1;

	char* c = PQcmdTuples(r);
	if (!c)
		return -1;

	int n = atoi(c);
	return n;
}