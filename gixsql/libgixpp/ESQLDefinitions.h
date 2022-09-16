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

/*
	SQL TYPE INFO is a 64 bit unsigned int:
	bits 00-15: scale (16 bit unsigned int)
	bits 16-47: precision (32 bit unsigned int)
	bits 48-51: misc flags
	bits 52-59: unused/reserved
	bits 60-63: type (encoded with the TYPE_SQL_* consts)
*/

#define TYPE_SQL_CHAR		 1ULL
#define TYPE_SQL_VARCHAR	 2ULL
#define TYPE_SQL_INT		 3ULL
#define TYPE_SQL_FLOAT		 4ULL
#define TYPE_SQL_DECIMAL	 5ULL
#define TYPE_SQL_BINARY		 6ULL
#define TYPE_SQL_VARBINARY	 7ULL
// 8-15 are currently free for expansion

#define FLAG_PICX_AS_VARCHAR	 1
#define FLAG_EMIT_VAR			 2

#define IS_VARLEN(T) (T == TYPE_SQL_VARCHAR || T == TYPE_SQL_VARBINARY)
#define IS_BINARY(T) (T == TYPE_SQL_BINARY || T == TYPE_SQL_VARBINARY)
#define IS_NUMERIC(T) (T == TYPE_SQL_INT || T == TYPE_SQL_FLOAT || T == TYPE_SQL_DECIMAL)
#define HAS_PICX_AS_VARCHAR(F) (F & FLAG_PICX_AS_VARCHAR)
#define HAS_PICX_AS_CHARF(F) ((F & FLAG_PICX_AS_VARCHAR) == 0))
#define HAS_FLAG_EMIT_VAR(F) (F & FLAG_EMIT_VAR)

// EXEC SQL WHENEVER stuff

#define WHENEVER_CLAUSE_NOT_FOUND	1
#define WHENEVER_CLAUSE_SQLWARNING	2
#define WHENEVER_CLAUSE_SQLERROR	3

#define WHENEVER_ACTION_CONTINUE	1
#define WHENEVER_ACTION_GOTO		2
#define WHENEVER_ACTION_PERFORM		3

struct esql_whenever_data_t
{
	int clause = 0;
	int action = 0;
	std::string host_label;
};

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
	
	// for compatibility
	hostref_or_literal_t *dbname = nullptr;
};

struct cb_exec_sql_stmt_t
{
	int startLine;
	int endLine;
	int period;

	std::vector<cb_hostreference_ptr> *host_list;
	std::vector<cb_res_hostreference_ptr> *res_host_list;
	std::vector<cb_sql_token_t> *sql_list;
	std::vector<hostref_or_literal_t *> *hostref_or_literal_list;

	esql_connection_info_t *conninfo = nullptr;
	esql_whenever_data_t *whenever_data = nullptr;

	bool conn_use_other_db;

	std::string  cursorName;
	std::string  commandName;
	std::string  textContent;
	bool command_putother;
	std::string  sqlName;
	std::string  incfileName;
	std::string  statementName;
	hostref_or_literal_t *statementSource = nullptr;
	bool startup_item;
	bool cursor_hold;
	bool transaction_release;

	int sql_query_list_id;

	hostref_or_literal_t *connectionId = nullptr;

	std::string src_file;
	std::string src_abs_path;

	cb_exec_sql_stmt_t()
	{
		startLine = 0;
		endLine = 0;
		period = 0;
		sql_query_list_id = 0;

		host_list = new std::vector<cb_hostreference_ptr>;
		res_host_list = new std::vector<cb_res_hostreference_ptr>;
		sql_list = new std::vector<cb_sql_token_t>;
		hostref_or_literal_list = new std::vector<hostref_or_literal_t *>;

		command_putother = false;
		conn_use_other_db = false;
		startup_item = false;
		cursor_hold = false;
		transaction_release = false;
	}

	~cb_exec_sql_stmt_t()
	{
		delete host_list;
		delete res_host_list;
		delete sql_list;
		delete hostref_or_literal_list;
	}
};

typedef cb_exec_sql_stmt_t *cb_exec_sql_stmt_ptr;

struct cb_field_t;

typedef cb_field_t *cb_field_ptr;

enum class DataSectionType
{
	Unknown = 0,
	WorkingStorage,
	LocalStorage,
	LinkageSection,
	FileSection
};

struct cb_field_t
{
	std::string sname;
	std::string path;
	DataSectionType data_section = DataSectionType::Unknown;
	int		level = 0;
	Usage	usage;
	int		occurs = 0;

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

	// See definitions above
	uint64_t sql_type = 0;

	std::string defined_at_source_file;
	int defined_at_source_line;

	int group_levels_count = 0;
};




// Parser helper
struct connect_to_info_t
{
	int type = 0;
	hostref_or_literal_t *t1 = nullptr;
	hostref_or_literal_t *t2 = nullptr;
};

enum class PreprocessedBlockType {
	ESQL = 1
};

struct PreprocessedBlockInfo {
	std::string module_name;

	std::string orig_source_file;
	int orig_start_line = 0;
	int orig_end_line = 0;

	std::string pp_source_file;
	int pp_start_line = 0;
	int pp_end_line = 0;
	int pp_gen_start_line = 0;
	int pp_gen_end_line = 0;
	
	PreprocessedBlockType type;
	std::string command;
};

