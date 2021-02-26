%skeleton "lalr1.cc" /* -*- c++ -*- */
%require "3.2"
%defines
%define parser_class_name {gix_esql_parser}

%define api.token.constructor
%define api.value.type variant
%define parse.assert

%code requires
{
#include <QString>
#include <QList>

#include "ESQLDefinitions.h"

#define  SIGNLEADING 1
#define  FLAGVARYING 1

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

static std::string to_std_string(QString *sp) { return (sp != NULL) ? sp->toStdString() : "(NULL)"; }
static std::string to_std_string(const QString s) { return s.toStdString(); }
static std::string to_std_string(const QList<QString> *slp) { if (!slp) return "(NULL-LIST)"; int n = slp->size() > 3 ? 3 : slp->size(); QString res; for (int i = 0; i < n; i++) res += slp->at(i); return (res + " ...").toStdString(); }
static std::string to_std_string(const int i) { return ""; }

}

// Tokens:
%define api.token.prefix {TOK_}

// Use variant-based semantic values: %type and %token expect genuine types
%token
  PERIOD "."
;

%token<QString> SELECT
%token<QString> SELECTFROM
%token<QString> TOKEN
%token<QString> HOSTTOKEN
%token<QString> WORD
%token<QString> PICTURE
%token<QString> INSERT
%token<QString> UPDATE
%token<QString> DISCONNECT
%token CONNECT_RESET
%token<QString> DELETE
%token<QString> EXECUTE
%token<QString> OTHERFUNC
%token<QString> INTO
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
%token USING
%token OPEN
%token CLOSE
%token FETCH
%token TRAILING
%token COMP_1
%token COMP_2
%token COMP_3
%token COMP
%token BINARY
%token USAGE
%token SIGN
%token LEADING
%token SEPARATE
%token IS
%token ARE
%token VALUE
%token ALL
%token OCCURS
%token EXTERNAL
%token TIMES
%token CONST

%token CONNECT_TO
%token USER
%token TABLE
%token TO
%token BEGIN_DECLARE_SPECIAL
%token COPY
%token COPY_FILE
%token<int> WITH_HOLD
%token WHERE_CURRENT_OF

%type <QList<QString> *> token_list declaresql includesql incfile
%type <QList<QString> *> opensql selectintosql select insertsql insert updatesql declare_cursor
%type <QString> declare_special declare_table
%type <QList<QString> *> update deletesql delete disconnect disconnectsql othersql
%type <QString> host_reference expr


// No %destructors are needed, since memory will be reclaimed by the
// regular destructors.
%printer { yyoutput << to_std_string($$); } <*>;

// Grammar:
%%
sqlstate_list:
| sqlstate_list sqlstate;

sqlstate: 
| sqlvariantstates
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

updatesql:
EXECSQL update token_list END_EXEC
{
	$$ = driver.cb_concat_text_list ($2, $3);
	driver.put_exec_list();
}
| EXECSQL update token_list WHERE_CURRENT_OF expr END_EXEC
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
EXECSQL delete token_list END_EXEC
{
	$$ = driver.cb_concat_text_list ($2, $3);
	driver.put_exec_list();
}


delete:
DELETE {$$ = driver.cb_text_list_add (NULL, $1);}

insertsql:
EXECSQL insert token_list END_EXEC
{
	$$ = driver.cb_concat_text_list ($2, $3);
	driver.put_exec_list();
}

insert:
INSERT {$$ = driver.cb_text_list_add (NULL, $1);}
| insert INTO {$$ = driver.cb_text_list_add ($1, $2);}



rollbacksql:
EXECSQL ROLLBACK_WORK END_EXEC {
	driver.put_exec_list();
}

commitsql:
EXECSQL COMMIT_WORK END_EXEC {
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
EXECSQL connect identified using END_EXEC { driver.put_exec_list(); }
| EXECSQL connect END_EXEC { driver.put_exec_list(); }
| EXECSQL connect_to END_EXEC { driver.put_exec_list(); } 

connect_to:
CONNECT_TO HOSTTOKEN USER HOSTTOKEN { 
	driver.cb_host_list_add (driver.host_reference_list, $4);
	driver.cb_host_list_add_force (driver.host_reference_list, $4);
	driver.cb_host_list_add (driver.host_reference_list, $2);
}


resetsql: 
EXECSQL CONNECT_RESET END_EXEC { 
 driver.put_exec_list();
 }

othersql:
EXECSQL OTHERFUNC token_list END_EXEC {
	$$ = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, $2), $3);
	driver.put_exec_list();
}

connect:
CONNECT host_reference {
	driver.cb_host_list_add (driver.host_reference_list, $2);
}


identified:
IDENTIFIED_BY host_reference {
	driver.cb_host_list_add (driver.host_reference_list, $2);
}

using:
USING host_reference {
	driver.cb_host_list_add (driver.host_reference_list, $2);
}


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
}

selectintosql:
EXECSQL SELECT token_list INTO res_host_references SELECTFROM token_list END_EXEC  {
	$$ = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, $2), $3);
	driver.cb_concat_text_list($$, driver.cb_text_list_add(NULL, $6));
	driver.cb_concat_text_list($$, $7);
	driver.put_exec_list();
}
| EXECSQL SELECT token_list INTO res_host_references END_EXEC  {
	$$ = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, $2), $3);
	driver.put_exec_list();
}


declaresql:
EXECSQL declare_for select END_EXEC { driver.put_exec_list(); }

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
|sqlvariantstate_list incfile
|sqlvariantstate_list includesql
|sqlvariantstate_list declare_cursor
|sqlvariantstate_list declare_table
|sqlvariantstate_list sqlvariantstate PERIOD
|sqlvariantstate_list HOSTVARIANTBEGIN { driver.put_exec_list(); }
|sqlvariantstate_list HOSTVARIANTEND { driver.put_exec_list(); }
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
NUMERIC WORD {
	cb_field_ptr x;

	x =  driver.cb_build_field_tree( $1, $2 , driver.current_field);
	if( x != NULL)
	{
		if( x->level != 78)
			driver.current_field = x;
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


data_description_clause_sequence:
{}
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
	driver.current_field->sign_leading = SIGNLEADING;
}
| _sign_is TRAILING flag_separate
{

}
;

_sign_is:	 SIGN  {}
| SIGN IS {}
;
flag_separate:
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

_is:		| IS;
_is_are:    | IS | ARE;
_all:       | ALL;
_times:		| TIMES;


%%

// Register errors to the driver:
void yy::gix_esql_parser::error (const location_type& l, const std::string& m)
{
    driver.error(l, m);
}
