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

%skeleton "lalr1.cc" /* -*- c++ -*- */
%require "3.7"
%defines
%define api.parser.class {gix_esql_parser}

%define api.token.constructor
%define api.value.type variant
%define parse.assert

%code requires
{
#include <string>
#include <cstring>
#include <vector>

#include "ESQLDefinitions.h"

#define SIGN_LEADING 1
#define SIGN_SEPARATE 1

class gix_esql_driver;
}

// The parsing context.
%param { gix_esql_driver& driver }

// Location tracking
%locations
%initial-action
{
    // Initialize the initial location.
    @$.begin.filename = @$.end.filename = &driver.file;
};

// Enable tracing and verbose errors (which may be wrong!)
%define parse.trace
%define parse.error verbose

// Parser needs to know about the driver:
%code
{
#include "gix_esql_driver.hh"
#define yylex driver.lexer.yylex

static std::string to_std_string(std::string *sp) { return (sp != NULL) ? *sp : "(NULL)"; }
static std::string to_std_string(const std::string s) { return s; }
static std::string to_std_string(const std::vector<std::string> *slp) { if (!slp) return "(NULL-LIST)"; int n = slp->size() > 3 ? 3 : slp->size(); std::string res; for (int i = 0; i < n; i++) res += slp->at(i); return (res + " ..."); }
static std::string to_std_string(const int i) { char buffer [33]; sprintf(buffer, "%d", i); char *res = (char*) malloc(strlen(buffer) + 1); strcpy (res, buffer); return res; }
static std::string to_std_string(hostref_or_literal_t *hl) { return hl->name; }

}

// Tokens:
%define api.token.prefix {TOK_}

// Use variant-based semantic values: %type and %token expect genuine types
%token
  PERIOD "."
;

%token<std::string> SELECT
%token<std::string> SELECTFROM
%token<std::string> TOKEN
%token<std::string> HOSTTOKEN
%token<std::string> WORD
%token<std::string> PICTURE
%token<std::string> INSERT
%token<std::string> UPDATE
%token<std::string> DISCONNECT
%token CONNECT_RESET
%token<std::string> DELETE
%token<std::string> EXECUTE
%token<std::string> OTHERFUNC
%token<std::string> INTO
%token<long> NUMERIC
%token END_EXEC
%token EXECSQL
%token EXECSQL_INCLUDE
%token FROM
%token DECLARE
%token CURSOR
%token FOR
%token WORKINGBEGIN
%token WORKINGEND
%token LINKAGEBEGIN
%token LINKAGEEND
%token FILEBEGIN
%token FILEEND
%token PROCEDURE_DIVISION
%token HOSTVARIANTBEGIN
%token HOSTVARIANTEND
%token INCLUDE_FILE
%token INCLUDE_SQLCA
%token SQLCA
%token IDENTIFIED_BY
%token COMMIT_WORK
%token ROLLBACK_WORK
%token CONNECT
%token TO
%token AS
%token AT
%token IS
%token DECLARE_VAR
%token USING
%token OPEN
%token CLOSE
%token FETCH
%token TRAILING
%token COMP_1
%token COMP_2
%token COMP_3
%token COMP_5
%token COMP
%token<uint64_t> CHAR
%token<uint64_t> VARCHAR
%token<uint64_t> BINARY
%token<uint64_t> VARBINARY
%token<uint64_t> FLOAT
%token<uint64_t> INTEGER
%token<uint64_t> DECIMAL
%token USAGE
%token SIGN
%token LEADING
%token SEPARATE
%token SQL_TYPE_IS
%token ARE
%token VALUE
%token ALL
%token OCCURS
%token EXTERNAL
%token TIMES
%token CONST
%token USER
%token TABLE
%token BEGIN_DECLARE_SPECIAL
%token COPY
%token COPY_FILE
%token<int> WITH_HOLD
%token WHERE_CURRENT_OF

%type <std::vector<std::string> *> token_list declaresql includesql incfile 
%type <std::vector<std::string> *> opensql selectintosql select insertsql insert updatesql declare_cursor
%type <std::string> declare_special declare_table
%type <std::vector<std::string> *> update deletesql delete disconnect disconnectsql othersql
%type <std::string> host_reference expr
//%type <uint32_t> opt_size

%type <hostref_or_literal_t *> strliteral_or_hostref dbid opt_connect_as opt_at opt_using opt_dbid

%type <uint64_t> opt_sql_type_def sql_type

// No %destructors are needed, since memory will be reclaimed by the
// regular destructors.
%printer { yyoutput << to_std_string($$); } <*>;

// Grammar:
%%
sqlstate_list: 
%empty
| sqlstate_list sqlstate;

sqlstate: 
/*%empty
| */ sqlvariantstates
| incfile
| connectsql
| opensql
| closesql
| fetchsql
| commitsql
| rollbacksql
| selectintosql
| insertsql
| deletesql
| updatesql
| disconnectsql
| resetsql
| othersql
| declaresql
;

execsql_with_opt_at: EXECSQL opt_at {
	driver.connectionid = $2;
};

updatesql:
execsql_with_opt_at update token_list END_EXEC
{
	$$ = driver.cb_concat_text_list ($2, $3);
	driver.put_exec_list();
}
| execsql_with_opt_at update token_list WHERE_CURRENT_OF expr END_EXEC
{
	driver.cb_set_cursorname($5);
	$$ = driver.cb_concat_text_list ($2, $3);
	driver.cb_concat_text_list($$, driver.cb_text_list_add(NULL, "WHERE CURRENT OF"));
	driver.cb_concat_text_list($$, driver.cb_text_list_add(NULL, driver.cursorname));
	driver.put_exec_list();
}

update:
UPDATE {$$ = driver.cb_text_list_add (NULL, $1);}


disconnectsql:
EXECSQL disconnect token_list END_EXEC
{
	$$ = driver.cb_concat_text_list ($2, $3);
	driver.put_exec_list();
}

disconnect:
DISCONNECT {$$ = driver.cb_text_list_add (NULL, $1);}

deletesql:
execsql_with_opt_at delete token_list END_EXEC
{
	$$ = driver.cb_concat_text_list ($2, $3);
	driver.put_exec_list();
}


delete:
DELETE {$$ = driver.cb_text_list_add (NULL, $1);}

insertsql:
execsql_with_opt_at insert token_list END_EXEC
{
	$$ = driver.cb_concat_text_list ($2, $3);
	driver.put_exec_list();
}

insert:
INSERT {$$ = driver.cb_text_list_add (NULL, $1);}
| insert INTO {$$ = driver.cb_text_list_add ($1, $2);}



rollbacksql:
execsql_with_opt_at ROLLBACK_WORK END_EXEC {
	driver.put_exec_list();
}

commitsql:
execsql_with_opt_at COMMIT_WORK END_EXEC {
	driver.put_exec_list();
}


fetchsql:
EXECSQL fetch INTO res_host_references END_EXEC {
	driver.put_exec_list();
}
fetch:
FETCH expr { driver.cb_set_cursorname($2);}

host_references:
host_reference {driver.cb_host_list_add (driver.host_reference_list, $1);}
| host_references TOKEN
| host_references host_reference {driver.cb_host_list_add (driver.host_reference_list, $2);}

res_host_references:
host_reference {driver.cb_res_host_list_add (driver.res_host_reference_list, $1);}
| res_host_references TOKEN
| res_host_references host_reference {driver.cb_res_host_list_add (driver.res_host_reference_list, $2);}

closesql:
EXECSQL CLOSE expr END_EXEC {
	driver.cb_set_cursorname($3);
	driver.put_exec_list();
}

opensql:
EXECSQL OPEN expr END_EXEC {
	driver.cb_set_cursorname($3);
	driver.put_exec_list();
}
| EXECSQL OPEN expr USING host_references END_EXEC {
	driver.cb_set_cursorname($3);
	driver.put_exec_list();
}

connectsql:
// EXEC SQL CONNECT TO :db_data_source [ AS :db_conn_id ] USER :username.:opt_password [ USING password ];
EXECSQL CONNECT TO strliteral_or_hostref opt_connect_as USER strliteral_or_hostref opt_using END_EXEC {
	driver.conninfo = new esql_connection_info_t();
	driver.conninfo->id = $5;
	driver.conninfo->data_source = $4;
	driver.conninfo->username = $7;
	driver.conninfo->password = $8;
	driver.put_exec_list();
}
// EXEC SQL CONNECT :username IDENTIFIED BY :password [ AT :db_conn_id ] USING :db_data_source
| EXECSQL CONNECT strliteral_or_hostref IDENTIFIED_BY strliteral_or_hostref opt_at USING strliteral_or_hostref END_EXEC {
	driver.conninfo = new esql_connection_info_t();
	driver.conninfo->id = $6;
	driver.conninfo->data_source = $8;
	driver.conninfo->username = $3;
	driver.conninfo->password = $5;
	driver.put_exec_list();
}
;

declaresqlvar:
DECLARE_VAR TOKEN IS sql_type END_EXEC {
	
	cb_field_ptr x;

	std::string var_name = $2;
	uint64_t t = $4;

	uint32_t length = t & 0xffffffff;
	uint16_t precision = (length >> 16);
	uint16_t scale = (length & 0xffff);
	int type = t >> 32;

	if (driver.field_map.find(var_name) == driver.field_map.end()) {
		std::tuple<uint64_t, int, int> d = std::make_tuple(t, driver.startlineno, driver.endlineno);
		driver.field_sql_type_info[var_name] = d;
	}
	else {
		x = driver.field_map[var_name];
		x->sql_type = t;

		x->is_varlen = IS_VARLEN(type);
		x->usage = IS_BINARY(type) ? Usage::Binary : Usage::None;

		x->pictype = -1;	// Preprocessor will build the correct PIC
		x->picnsize = precision;
		x->scale = scale;

		driver.cb_set_commandname("DECLARE_VAR");
		driver.cb_host_list_add (driver.host_reference_list, x->sname);
		driver.put_exec_list(); 
		
		// This will be rewritten by the preprocessor, so
		// we comment out the original variable definition
		cb_exec_sql_stmt_ptr stmt = new cb_exec_sql_stmt_t();
		stmt->commandName = "COMMENT";
		stmt->src_file = x->defined_at_source_file;
		stmt->startLine = x->defined_at_source_line;
		stmt->endLine = x->defined_at_source_line;
		driver.exec_list->push_back(stmt);
	}
}
;

dbid: strliteral_or_hostref { $$ = $1; }

opt_using:
USING strliteral_or_hostref  { $$ = $2; }
| %empty { $$ = new hostref_or_literal_t(); }
;

opt_connect_as:
AS dbid  { $$ = $2; }
| %empty { $$ = new hostref_or_literal_t(); }
;

opt_at:
AT dbid { $$ = $2; }
| %empty { $$ = new hostref_or_literal_t(); }
;

opt_dbid:
%empty { $$ = new hostref_or_literal_t(); }
| dbid { $$ = $1; }
;

strliteral_or_hostref : 
TOKEN { $$ = new hostref_or_literal_t($1, true); }
| host_reference { $$ = new hostref_or_literal_t($1, false); }
;

resetsql: 
EXECSQL CONNECT_RESET opt_dbid END_EXEC { 
	driver.connectionid = $3;
	driver.put_exec_list();
}
;

othersql:
execsql_with_opt_at OTHERFUNC token_list END_EXEC {
	$$ = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, $2), $3);
	driver.put_exec_list();
}
;

incfile:
EXECSQL_INCLUDE INCLUDE_FILE END_EXEC{
	driver.put_exec_list();
	driver.lexer.pushNewFile(driver.incfilename, &driver, true, true);
}
| COPY { 
	driver.put_exec_list(); 

	driver.lexer.pushNewFile(driver.incfilename, &driver, true, false);
}

includesql:
EXECSQL_INCLUDE INCLUDE_SQLCA END_EXEC{
	driver.put_exec_list();
	driver.lexer.pushNewFile("SQLCA", &driver, true, true);
}

selectintosql:
execsql_with_opt_at SELECT token_list INTO res_host_references SELECTFROM token_list END_EXEC  {
	$$ = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, $2), $3);
	driver.cb_concat_text_list($$, driver.cb_text_list_add(NULL, $6));
	driver.cb_concat_text_list($$, $7);
	driver.put_exec_list();
}
| execsql_with_opt_at SELECT token_list INTO res_host_references END_EXEC  {
	$$ = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, $2), $3);
	driver.put_exec_list();
}


declaresql:
execsql_with_opt_at declare_for select END_EXEC { driver.put_exec_list(); }

select:
SELECT token_list{ $$ = driver.cb_concat_text_list (driver.cb_text_list_add (NULL, $1), $2);}

declare_for:
DECLARE expr CURSOR FOR { driver.cb_set_cursorname($2); driver.cb_set_cursor_hold(0); }
| DECLARE expr CURSOR WITH_HOLD FOR { driver.cb_set_cursorname($2); driver.cb_set_cursor_hold(1); }

token_list:
expr				{      $$ = driver.cb_text_list_add (NULL, $1);}
| token_list expr	{      $$ = driver.cb_text_list_add ($1, $2);}
| token_list host_reference   {
	$$ = driver.cb_text_list_add ($1, driver.cb_host_list_add (driver.host_reference_list, $2));
}

host_reference:
HOSTTOKEN { $$ = $1; }

expr: TOKEN { $$ = $1; }
|SELECT{}
|FOR {}
|UPDATE {}
;

sqlvariantstates: WORKINGBEGIN {
	driver.current_field = NULL;
	driver.description_field = NULL;
	driver.put_exec_list();
}
sqlvariantstate_list
WORKINGEND {
	// check host_variable
	driver.put_exec_list();
}
|LINKAGEBEGIN {
	driver.current_field = NULL;
	driver.description_field = NULL;
	driver.put_exec_list();
}
sqlvariantstate_list
LINKAGEEND {
	// check host_variable
	driver.put_exec_list();
}
|FILEBEGIN {
	driver.current_field = NULL;
	driver.description_field = NULL;
	driver.put_exec_list();
}
sqlvariantstate_list
FILEEND {
	// check host_variable
	driver.put_exec_list();
}
|PROCEDURE_DIVISION {
	driver.put_exec_list();
}
;


sqlvariantstate_list:
%empty
|sqlvariantstate_list incfile
|sqlvariantstate_list includesql
|sqlvariantstate_list declare_cursor
|sqlvariantstate_list declare_table
|sqlvariantstate_list sqlvariantstate PERIOD
|sqlvariantstate_list HOSTVARIANTBEGIN { driver.put_exec_list(); }
|sqlvariantstate_list HOSTVARIANTEND { driver.put_exec_list(); }
|sqlvariantstate_list declaresqlvar
;

declare_cursor:
	 declare_special CURSOR FOR select END_EXEC { 
		$$ = $4;
		driver.cb_set_cursorname($1); 
		driver.cb_set_cursor_hold(0); 
		driver.put_startup_exec_list(); 
} | declare_special CURSOR WITH_HOLD FOR select END_EXEC { 
		$$ = $5;
		driver.cb_set_cursorname($1); 
		driver.cb_set_cursor_hold(1); 
		driver.put_startup_exec_list(); 
}
;

declare_table:
	declare_special TABLE token_list END_EXEC { 
	driver.cb_set_commandname("DECLARE_TABLE");
	driver.put_exec_list(); 
}
;

declare_special:
	BEGIN_DECLARE_SPECIAL expr { $$ = $2; }
	
sqlvariantstate:
NUMERIC WORD opt_sql_type_def {
	cb_field_ptr x;

	x =  driver.cb_build_field_tree( $1, $2 , driver.current_field);
	if(x != NULL)
	{
		if( x->level != 78)
			driver.current_field = x;

		if ($3 != 0) {
			uint64_t t = $3;
			uint32_t length = t & 0xffffffff;
			uint16_t precision = (length >> 16);
			uint16_t scale = (length & 0xffff);
			int type = t >> 32;

			x->sql_type = type;
			x->is_varlen = IS_VARLEN(type);
			x->usage = IS_BINARY(type) ? Usage::Binary : Usage::None;

			x->pictype = -1;	// Preprocessor will build the correct PIC for "SQL TYPE IS" defs
			x->picnsize = precision;
			x->scale = scale;
			driver.cb_set_commandname("DECLARE_VAR");
			driver.cb_host_list_add (driver.host_reference_list, x->sname);
			driver.put_exec_list(); 
		}
	}
}
data_description_clause_sequence
{
	if (driver.description_field == NULL)
		driver.description_field = driver.current_field;
}
|NUMERIC {
	cb_field_ptr x;

	x =  driver.cb_build_field_tree( $1, "" , driver.current_field); // regist dummy name
	if( x != NULL){
	}
}
data_description_clause_sequence
{
	if (driver.description_field == NULL)
		driver.description_field = driver.current_field;
}
;

opt_sql_type_def:
%empty { $$ = 0; }
| SQL_TYPE_IS sql_type {
		$$ = $2;
}
;

sql_type:
  BINARY	{ $$ = (TYPE_SQL_BINARY << 32) + $1; }
| VARBINARY { $$ = (TYPE_SQL_VARBINARY << 32) + $1; }
| CHAR		{ $$ = (TYPE_SQL_CHAR << 32) + $1; }
| VARCHAR	{ $$ = (TYPE_SQL_VARCHAR << 32) + $1; }
| INTEGER  	{ $$ = (TYPE_SQL_INT << 32) + $1; }
| FLOAT   	{ $$ = (TYPE_SQL_FLOAT << 32) + $1; }
| DECIMAL  	{ $$ = (TYPE_SQL_DECIMAL << 32) + $1; }
;

data_description_clause_sequence:
%empty {}
| data_description_clause_sequence data_description_clause 
{}
;

data_description_clause:
picture_clause
| usage_clause
| sign_clause
| occurs_clause
| value_clause
| external_clause
;

picture_clause:
PICTURE         {  driver.build_picture( $1,driver.current_field);  }
;

usage_clause:
usage
| USAGE _is usage
;

usage:
COMP				{ driver.current_field->usage = Usage::Binary;  }
| BINARY			{ driver.current_field->usage = Usage::Binary;  }
| COMP_1			{ driver.current_field->usage = Usage::Float;   }
| COMP_2			{ driver.current_field->usage = Usage::Double;  }
| COMP_3			{ driver.current_field->usage = Usage::Packed;  }
| COMP_5			{ driver.current_field->usage = Usage::NativeBinary;  }
| WORD              { driver.current_field->usage = Usage::None;    }
;

value_clause: VALUE _is_are _all const_clause {} 

const_clause: 
NUMERIC {}
|WORD { }
|CONST {}

sign_clause:
_sign_is LEADING flag_separate
{
	driver.current_field->sign_leading = SIGN_LEADING;
}
| _sign_is TRAILING flag_separate
{

}
;

_sign_is:	 SIGN  {}
| SIGN IS {}
;
flag_separate:
%empty 
| SEPARATE { driver.current_field->separate = SIGN_SEPARATE; }
;

occurs_clause:
OCCURS NUMERIC _times
{
	driver.current_field->occurs = (int)$2;
}
;

external_clause:
_is EXTERNAL {}
;

_is:		%empty | IS;
_is_are:    %empty | IS | ARE;
_all:       %empty | ALL;
_times:		%empty | TIMES;


%%

// Register errors to the driver:
void yy::gix_esql_parser::error (const location_type& l, const std::string& m)
{
    driver.error(l, m);
}
