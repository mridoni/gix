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

#pragma once

#include <string>
#include <vector>

#define TYPE_SQL_CHAR		 1ULL << 0
#define TYPE_SQL_VARCHAR	 1ULL << 1
#define TYPE_SQL_INT		 1ULL << 2
#define TYPE_SQL_FLOAT		 1ULL << 3
#define TYPE_SQL_DECIMAL	 1ULL << 4
#define TYPE_SQL_BINARY		 1ULL << 5
#define TYPE_SQL_VARBINARY	 1ULL << 6

#define IS_VARLEN(T) (T == TYPE_SQL_VARCHAR || T == TYPE_SQL_VARBINARY)
#define IS_BINARY(T) (T == TYPE_SQL_BINARY || T == TYPE_SQL_VARBINARY)
#define IS_NUMERIC(T) (T == TYPE_SQL_INT || T == TYPE_SQL_FLOAT || T == TYPE_SQL_DECIMAL)

enum class Usage
{
	None,
	Float,
	Double,
	Packed,
	Binary,
	NativeBinary
};

typedef std::string cb_sql_token_t;

struct cb_hostreference_t
{
	std::string hostreference;
	int hostno;
	int lineno;
};

typedef cb_hostreference_t *cb_hostreference_ptr;

struct cb_res_hostreference_t
{
	std::string hostreference;
	int lineno;
};

typedef cb_res_hostreference_t *cb_res_hostreference_ptr;

struct hostref_or_literal_t
{
	hostref_or_literal_t() {}
	hostref_or_literal_t(std::string n, bool l) { name = n; is_literal = l; is_set = true; }
	std::string name;
	bool is_literal = true;
	bool is_set = false;
};

struct esql_connection_info_t
{
	hostref_or_literal_t *id = nullptr;
	hostref_or_literal_t *data_source = nullptr;
	hostref_or_literal_t *username = nullptr;
	hostref_or_literal_t *password = nullptr;
};


struct cb_exec_sql_stmt_t
{
	int startLine;
	int endLine;
	int period;

	std::vector<cb_hostreference_ptr> *host_list;
	std::vector<cb_res_hostreference_ptr> *res_host_list;
	std::vector<cb_sql_token_t> *sql_list;

	esql_connection_info_t *conninfo = nullptr;

	bool conn_use_other_db;

	std::string  cursorName;
	std::string  commandName;
	bool command_putother;
	std::string  sqlName;
	std::string  incfileName;
	bool startup_item;
	bool cursor_hold;

	int sql_query_list_id;

	hostref_or_literal_t *connectionId = nullptr;

	std::string src_file;

	cb_exec_sql_stmt_t()
	{
		startLine = 0;
		endLine = 0;
		period = 0;
		sql_query_list_id = 0;

		host_list = new std::vector<cb_hostreference_ptr>;
		res_host_list = new std::vector<cb_res_hostreference_ptr>;
		sql_list = new std::vector<cb_sql_token_t>;

		command_putother = false;
		conn_use_other_db = false;
		startup_item = false;
		cursor_hold = false;
	}

	~cb_exec_sql_stmt_t()
	{
		delete host_list;
		delete res_host_list;
		delete sql_list;
	}
};

typedef cb_exec_sql_stmt_t *cb_exec_sql_stmt_ptr;

struct cb_field_t;

typedef cb_field_t *cb_field_ptr;

enum class DataSectionType
{
	Unknown = 0,
	WorkingStorage,
	LinkageSection,
	FileSection
};

struct cb_field_t
{
	std::string sname;
	std::string path;
	DataSectionType data_section = DataSectionType::Unknown;
	int		level;
	Usage	usage;
	int		occurs;

	bool is_varlen = false;

	cb_field_ptr parent = nullptr;
	cb_field_ptr children = nullptr;
	cb_field_ptr sister = nullptr;

	int pictype = 0;
	int picnsize = 0;
	int scale = 0;
	unsigned char have_sign = 0;
	bool sign_leading = false;
	bool separate = false;

	uint64_t sql_type = 0;	// 64 bits: 32-63 : type (TYPE_SQL_* constants) ; 16-31: precision ; 0-15 : scale

	std::string defined_at_source_file;
	int defined_at_source_line;
};


//}