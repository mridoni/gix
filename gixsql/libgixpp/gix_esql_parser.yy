/*
* Copyright (C) 2021,2022 Marco Ridoni
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
#include "libcpputils.h"

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
static std::string to_std_string(connect_to_info_t *i) { if (i) { char buffer [33]; sprintf(buffer, "%d", i->type); char *res = (char*) malloc(strlen(buffer) + 1); strcpy (res, buffer); return res; } else return "N/A"; }

}

// Tokens:
%define api.token.prefix {TOK_}

// Use variant-based semantic values: %type and %token expect genuine types
/* %token
  PERIOD "."
;
*/
%token PERIOD
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
%token INCLUDE
%token FROM
%token IMMEDIATE
%token DECLARE
%token CURSOR
%token FOR
%token COMMA
%token STATEMENT
%token WORKINGBEGIN
%token WORKINGEND
%token LINKAGEBEGIN
%token LINKAGEEND
%token LOCALSTORAGEBEGIN
%token LOCALSTORAGEEND
%token FD
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
%token SAVEPOINT
%token CONNECT
%token TO
%token AS
%token AT
%token IS
%token VARYING
%token IGNORE
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
%token UNBOUNDED
%token DEPENDING_ON
%token ASCENDING_KEY_IS
%token INDEXED_BY
%token EXTERNAL
%token TIMES
%token CONST
%token USER
%token TABLE
%token COPY
%token COPY_FILE
%token<int> WITH_HOLD
%token WHERE_CURRENT_OF
%token PREPARE

%type <std::vector<std::string> *> token_list declaresql includesql incfile opt_othersql_tokens
%type <std::vector<std::string> *> opensql selectintosql select insertsql insert updatesql cursor_declaration
%type <std::vector<std::string> *> update deletesql delete disconnect disconnectsql othersql executesql ignoresql
%type <std::string> host_reference expr othersql_token

%type <hostref_or_literal_t *> strliteral_or_hostref dbid opt_connect_as opt_at opt_using opt_dbid
%type <int> opt_with_hold
%type <uint64_t> opt_sql_type_def sql_type

%type <connect_to_info_t *> opt_auth_info opt_identified_by

%nonassoc error

// No %destructors are needed, since memory will be reclaimed by the
// regular destructors.
%printer { yyoutput << to_std_string($$); } <*>;

// Grammar:
%%
sqlstate_list: 
%empty
| sqlstate_list sqlstate;

sqlstate: 
sqlvariantstates
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
| preparesql
| executesql
| ignoresql
| badsql
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
EXECSQL disconnect opt_dbid END_EXEC
{
	//$$ = driver.cb_concat_text_list ($2, $3);
	driver.connectionid = $3;
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
	// The necessary fields have been populated by the lexer code
	driver.put_exec_list();
}
| execsql_with_opt_at ROLLBACK_WORK TO SAVEPOINT TOKEN END_EXEC {
	// We intercept the ROLLBACK TO SAVEPOINT and pass it back as an SQL statement
	// of type "other" (unknown).

	driver.commandname = "PASSTHRU";
	driver.sqlnum++;
	driver.sqlname = string_format("SQ%04d", driver.sqlnum);
	driver.sql_list->push_back("ROLLBACK");
	driver.sql_list->push_back("TO");
	driver.sql_list->push_back("SAVEPOINT");
	driver.sql_list->push_back($5);
	driver.put_exec_list();
}

commitsql:
execsql_with_opt_at COMMIT_WORK END_EXEC {
	driver.put_exec_list();
}


fetchsql:
EXECSQL unexpected_at fetch INTO res_host_references END_EXEC {
	driver.put_exec_list();
}
;

fetch:
FETCH expr { 
	driver.cb_set_cursorname($2);
}
;

host_references:
host_reference {driver.cb_host_list_add (driver.host_reference_list, $1);}
| host_references TOKEN
| host_references host_reference {driver.cb_host_list_add (driver.host_reference_list, $2);}

res_host_references:
host_reference {driver.cb_res_host_list_add (driver.res_host_reference_list, $1);}
| res_host_references TOKEN
| res_host_references host_reference {driver.cb_res_host_list_add (driver.res_host_reference_list, $2);}

closesql:
EXECSQL unexpected_at CLOSE expr END_EXEC {
	driver.cb_set_cursorname($4);
	driver.put_exec_list();
}

opensql:
EXECSQL unexpected_at OPEN  expr END_EXEC {
	driver.cb_set_cursorname($4);
	driver.put_exec_list();
}
| EXECSQL unexpected_at OPEN expr USING host_references END_EXEC {
	driver.cb_set_cursorname($4);
	driver.put_exec_list();
}

connectsql:
// mode 1/2-6 : 
// EXEC SQL CONNECT TO :db_data_source [ AS :db_conn_id ] USER :username.:opt_password [ USING password ];
// EXEC SQL CONNECT TO :dbname         [ AS :db_conn_id ] USER :username                 USING :db_data_source IDENTIFIED BY :password
EXECSQL CONNECT TO strliteral_or_hostref opt_connect_as USER strliteral_or_hostref opt_auth_info END_EXEC {
	driver.conninfo = new esql_connection_info_t();

	switch ($8->type) {
		case 0:	// [ USING :password ] omitted
			driver.conninfo->id = $5;
			driver.conninfo->data_source = $4;
			driver.conninfo->username = $7;
			driver.conninfo->password = new hostref_or_literal_t();
			driver.conninfo->dbname = new hostref_or_literal_t();			
			break;

		case 1:	// USING :password (no IDENTIFIED BY... follows)
			driver.conninfo->id = $5;
			driver.conninfo->data_source = $4;
			driver.conninfo->username = $7;
			driver.conninfo->password = $8->t1;
			driver.conninfo->dbname = new hostref_or_literal_t();			
			break;

		case 2:	// USING :db_data_source IDENTIFIED BY :password
			driver.conninfo->id = $5;
			driver.conninfo->data_source = $8->t1;
			driver.conninfo->username = $7;
			driver.conninfo->password = $8->t2;
			driver.conninfo->dbname = $4;			
			break;

	}

	driver.put_exec_list();
}
// mode 3/4: EXEC SQL CONNECT :username IDENTIFIED BY :password [ AT :db_conn_id ] [ USING :db_data_source] (mode 4 is unsupported)
| EXECSQL CONNECT strliteral_or_hostref IDENTIFIED_BY strliteral_or_hostref opt_at opt_using END_EXEC {
	if (!$7->is_set) {
		driver.warning(@$, "Unsupported connection mode, data source information not provided. Connection will fail."); 
	}
	
	driver.conninfo = new esql_connection_info_t();
	driver.conninfo->id = $6;
	driver.conninfo->data_source = $7;
	driver.conninfo->username = $3;
	driver.conninfo->password = $5;
	driver.put_exec_list();

}
// mode 5: EXEC SQL CONNECT USING :db_data_source (credentials must be embedded to be able to connect)
| EXECSQL CONNECT USING strliteral_or_hostref END_EXEC {
	driver.conninfo = new esql_connection_info_t();
	driver.conninfo->data_source = $4;
	driver.put_exec_list();
}
;

opt_auth_info:
%empty { $$ = new connect_to_info_t(); $$->type = 0; }
| USING strliteral_or_hostref opt_identified_by {
	if ($3 == nullptr) {
		$$ = new connect_to_info_t(); 
		$$->type = 1;
		$$->t1 = $2;
	}
	else {
		$$ = $3;
		$$->t1 = $2;
	}
}
;

opt_identified_by:
%empty { $$ = nullptr; }
| IDENTIFIED_BY strliteral_or_hostref {
	$$ = new connect_to_info_t(); 
	$$->type = 2;
	$$->t2 = $2;
}
;

declaresqlvar:
DECLARE_VAR TOKEN IS sql_type END_EXEC {
	
	cb_field_ptr x;

	std::string var_name = $2;
	uint64_t type_info = $4;

	uint64_t length = type_info & 0xffffffffffff;	// 48 bits
	uint32_t precision = (length >> 16);
	uint16_t scale = (length & 0xffff);
	int sql_type = (type_info >> 60);

	if (driver.field_map.find(var_name) == driver.field_map.end()) {
		std::string src_file = driver.lexer.src_location_stack.top().filename;
		std::tuple<uint64_t, int, int, std::string> d = std::make_tuple(type_info, driver.startlineno, driver.endlineno, src_file);
		driver.field_sql_type_info[var_name] = d;
	}
	else {
		x = driver.field_map[var_name];
		x->sql_type = type_info;

		x->is_varlen = IS_VARLEN(sql_type);
		x->usage = IS_BINARY(sql_type) ? Usage::Binary : Usage::None;

		x->pictype = -1;	// Preprocessor will build the correct PIC
		
		// We do not want to overwrite precision and scale as defined in the COBOL source
		// x->picnsize = precision;
		// x->scale = scale;

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

unexpected_at:
AT dbid {  
	driver.warning(@$, "AT DB-NAME is not allowed for CURSOR access, always used from CURSOR DECLARE"); 
}
| %empty { /* everything fine */ }
;

opt_dbid:
%empty { $$ = new hostref_or_literal_t(); }
| dbid { $$ = $1; }
;

strliteral_or_hostref: 
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
execsql_with_opt_at OTHERFUNC opt_othersql_tokens END_EXEC {
	driver.commandname = "PASSTHRU";
	$$ = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, $2), $3);
	driver.put_exec_list();
}
;

opt_othersql_tokens:
%empty { $$ = nullptr; }
| othersql_token opt_othersql_tokens {
#if 0
	$$ = driver.cb_text_list_add (NULL, $1);
	if ($2)
		$$ = driver.cb_concat_text_list ($$, $2);
#else
	// This is faster
	$$ = ($2 != nullptr) ? $2 : new std::vector<cb_sql_token_t>();
	$$->insert($$->begin(), $1);
#endif
}
;

othersql_token:
host_reference  { $$ = driver.cb_host_list_add (driver.host_reference_list, $1); }
|TOKEN			{ $$ = $1; }
;

incfile:
EXECSQL INCLUDE INCLUDE_FILE END_EXEC{
	driver.put_exec_list();
	driver.lexer.pushNewFile(driver.incfilename, &driver, true, true);
}
| COPY { 
	driver.put_exec_list(); 

	driver.lexer.pushNewFile(driver.incfilename, &driver, true, false);
}

includesql:
EXECSQL INCLUDE INCLUDE_SQLCA END_EXEC{
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

badsql:
execsql_with_opt_at error END_EXEC
{
	yyerrok;
};

declaresql:
execsql_with_opt_at DECLARE sql_declaration END_EXEC {
	//driver.put_exec_list();
}
;

sql_declaration:
  TOKEN statement_declaration	{ driver.declared_statements.push_back($1); }
| TOKEN cursor_declaration		{ 
	driver.cb_set_cursorname($1); 
	if (!driver.procedure_division_started)
 		driver.put_startup_exec_list(); 
	else
		driver.put_exec_list();	
}
| TOKEN table_declaration		{ }
;

statement_declaration:
STATEMENT {
	
}
;

cursor_declaration:
CURSOR opt_with_hold FOR select { driver.cb_set_cursor_hold($2); }
;

table_declaration:
TABLE token_list {
 	driver.cb_set_commandname("DECLARE_TABLE");
 	driver.put_exec_list(); 
}
;

opt_with_hold:
%empty		{ $$ = 0; }
| WITH_HOLD { $$ = 1; }
;

preparesql:
execsql_with_opt_at PREPARE TOKEN FROM strliteral_or_hostref END_EXEC {

	driver.cb_set_commandname("PREPARE_STATEMENT");
	driver.statement_name = $3;

	if ($5->is_literal) {
		driver.sql_list->push_back(unquote($5->name));
		driver.sqlnum++;
		driver.sqlname = string_format("SQ%04d", driver.sqlnum);
		driver.statement_source = nullptr;
	}
	else {
		driver.statement_source = $5;
		driver.sql_list->clear();
	}

	driver.put_exec_list();
}
;

executesql:
execsql_with_opt_at EXECUTE IMMEDIATE strliteral_or_hostref END_EXEC {
	driver.commandname = "EXECUTE_IMMEDIATE";

	if ($4->is_literal) {
		driver.sql_list->push_back(unquote($4->name));
		driver.sqlnum++;
		driver.sqlname = string_format("SQ%04d", driver.sqlnum);
		driver.statement_source = nullptr;
	}
	else {
		driver.statement_source = $4;
		driver.sql_list->clear();
	}
	driver.put_exec_list();
}
| execsql_with_opt_at EXECUTE TOKEN opt_using_hostref_list END_EXEC {

	driver.commandname = "EXECUTE_PREPARED";
	driver.statement_name = $3;
	driver.put_exec_list();
}
;

ignoresql:
EXECSQL IGNORE token_list END_EXEC
{
	driver.put_exec_list();
}
;


opt_using_hostref_list:
%empty
| USING host_references { }
;

select:
SELECT token_list{ $$ = driver.cb_concat_text_list (driver.cb_text_list_add (NULL, $1), $2);}
;

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
|FILEBEGIN  {
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

fd_def:
FD token_list PERIOD  {}
;


sqlvariantstate_list:
%empty
|sqlvariantstate_list fd_def
|sqlvariantstate_list incfile
|sqlvariantstate_list includesql
|sqlvariantstate_list declaresql
|sqlvariantstate_list sqlvariantstate PERIOD
|sqlvariantstate_list HOSTVARIANTBEGIN { driver.put_exec_list(); }
|sqlvariantstate_list HOSTVARIANTEND { driver.put_exec_list(); }
|sqlvariantstate_list ignoresql
|sqlvariantstate_list declaresqlvar
;

sqlvariantstate:
NUMERIC WORD opt_sql_type_def {
	cb_field_ptr x;

	x =  driver.cb_build_field_tree( $1, $2 , driver.current_field);
	if(x != NULL)
	{
		if( x->level != 78)
			driver.current_field = x;

		if ($3 != 0) {
			uint64_t type_info = $3;
			uint64_t length = type_info & 0xffffffffffff;	// 48 bits
			uint32_t precision = (length >> 16);
			uint16_t scale = (length & 0xffff);
			int sql_type = type_info >> 60;

			x->sql_type = type_info;
			x->is_varlen = IS_VARLEN(sql_type);
			x->usage = IS_BINARY(sql_type) ? Usage::Binary : Usage::None;

			x->pictype = -1;	// Preprocessor will build the correct PIC for "SQL TYPE IS" defs

			// We do not want to overwrite precision and scale as defined in the COBOL source
			// x->picnsize = precision;
			// x->scale = scale;
			
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
  BINARY	{ $$ = (TYPE_SQL_BINARY << 60) + $1; }
| VARBINARY { $$ = (TYPE_SQL_VARBINARY << 60) + $1; }
| CHAR		{ $$ = (TYPE_SQL_CHAR << 60) + $1; }
| VARCHAR	{ $$ = (TYPE_SQL_VARCHAR << 60) + $1; }
| INTEGER  	{ $$ = (TYPE_SQL_INT << 60) + $1; }
| FLOAT   	{ $$ = (TYPE_SQL_FLOAT << 60) + $1; }
| DECIMAL  	{ $$ = (TYPE_SQL_DECIMAL << 60) + $1; }
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
| varying_clause
;

picture_clause:
PICTURE         {  driver.build_picture( $1,driver.current_field);  }
;

usage_clause:
usage
| USAGE _is usage
;

varying_clause:
VARYING {
	auto x = driver.current_field;
	
	uint64_t type_info = (TYPE_SQL_VARCHAR << 60);
	type_info |= (((uint64_t)x->picnsize << 16));

	x->sql_type = type_info;
	x->is_varlen = true;
	x->usage = Usage::None;

	x->pictype = PIC_ALPHANUMERIC;	// Preprocessor will build the correct PIC for "SQL TYPE IS" defs

	// We do not want to overwrite precision and scale as defined in the COBOL source
	// x->picnsize = precision;
	// x->scale = scale;
			
	driver.cb_set_commandname("DECLARE_VAR");
	driver.cb_host_list_add (driver.host_reference_list, x->sname);
	driver.put_exec_list(); 

	// We need to store the variable data, so we can fix it up before the output source is generated
	std::string src_file = driver.lexer.src_location_stack.top().filename;
	std::tuple<uint64_t, int, int, std::string> d = std::make_tuple(type_info, driver.startlineno, driver.endlineno, src_file);
	driver.field_sql_type_info[x->sname] = d;
}
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
OCCURS NUMERIC occurs_numeric_data occurs_sort_opts
| OCCURS UNBOUNDED occurs_unbounded_data occurs_sort_opts
;

occurs_numeric_data:
TO numeric_or_word TIMES DEPENDING_ON WORD
| DEPENDING_ON WORD
| TIMES opt_depending_on
| %empty
;

opt_depending_on:
%empty
| DEPENDING_ON WORD
;

occurs_unbounded_data:
%empty
| NUMERIC TIMES DEPENDING_ON TOKEN
| DEPENDING_ON WORD
;

occurs_sort_opts:
opt_ascending_key_is opt_indexed_by
;

opt_ascending_key_is:
%empty
| ASCENDING_KEY_IS WORD
;

opt_indexed_by:
%empty
| INDEXED_BY WORD
;


numeric_or_word:
NUMERIC
| WORD
;

external_clause:
_is EXTERNAL {}
;

_is:		%empty | IS;
_is_are:    %empty | IS | ARE;
_all:       %empty | ALL;


%%

// Register errors to the driver:
void yy::gix_esql_parser::error (const location_type& l, const std::string& m)
{
    driver.error(l, m, ERR_SYNTAX_ERROR);
}
