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

/* -*- c++ -*- */
%{
#include <cerrno>
#include <climits>
#include <cstdlib>
#include <string>

#include "gix_esql_driver.hh"
#include "gix_esql_parser.hh"
#include "libcpputils.h"

int flag_insqlstring = 0;
int flag_selectcommand = 0;
int flag_select_from_passed = 0;
int cursor_hold = 0;

int find_last_space(char * s);
int count_crlf(char *s);
uint32_t extract_len(char * s);
void extract_precision_scale(char * s, uint32_t *precision, uint16_t *scale);

#ifdef _MSC_VER 
#define strncasecmp _strnicmp
#define strcasecmp _stricmp
#endif

#define UNPUT_TOKEN() { \
	int i; \
	char *yycopy = strdup( yytext ); \
	for ( i = yyleng - 1; i >= 0; --i ) { \
		unput( yycopy[i] ); \
	} \
	free( yycopy ); \
	}

// This "fakes" being at the beginning of the line, so we can correctly
// parse expressions with '^' after unputting a token to the input buffer
#define SET_AT_BOL() YY_CURRENT_BUFFER->yy_at_bol = 1;

#define FLAG_LINKAGE_START	128

// Work around an incompatibility in flex (at least versions
// 2.5.31 through 2.5.33): it generates code that does
// not conform to C89.  See Debian bug 333231
// <http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=333231>.
# undef yywrap

// The location of the current token.
static yy::location loc;

// CHANGE: "Code run each time a pattern is matched" moved from its
// own block below (this change was not strictly necessary).
#define YY_USER_ACTION  loc.columns (yyleng);

// CHANGE: We must exclude unistd.h or the compiler will choke on the
// `isatty()` declaration emitted by `flex` having a different
// exception specifier from the one in `unistd.h`:
#define YY_NO_UNISTD_H

// This should be maintained
#if defined (_DEBUG) && defined (VERBOSE)
const char *GixEsqlLexer::yy_state_descs[NUM_YY_STATES] = { "INITIAL", "PICTURE_STATE", "DATA_DIVISION_STATE", "ESQL_FUNC_STATE", "ESQL_INCLUDE_STATE", 
													"ESQL_SELECT_STATE", "ESQL_STATE", "INCLUDE_STATE", "FD_STATE", "ESQL_DBNAME_STATE", "VAR_DECLARE_STATE", 
													"ESQL_PREPARE_STATE", "ESQL_DECLARE_STATE", "ESQL_EXECUTE_STATE", "ESQL_CONNECT_STATE", "ESQL_IGNORE_STATE", "ESQL_WHENEVER_STATE"  };
#endif

%}

/* Options: */
%option 8bit
%option caseless
%option never-interactive
%option yylineno
%option stack

/* This works around a win-flex + MSVC bug (unnecessary warnings about macro redefinitions) */
%top{
#include <stdint.h>
}

/* Regex abbreviations: */

%s PICTURE_STATE DATA_DIVISION_STATE

%x ESQL_FUNC_STATE ESQL_INCLUDE_STATE ESQL_SELECT_STATE ESQL_STATE INCLUDE_STATE FD_STATE ESQL_DBNAME_STATE VAR_DECLARE_STATE ESQL_PREPARE_STATE ESQL_DECLARE_STATE ESQL_EXECUTE_STATE ESQL_CONNECT_STATE ESQL_IGNORE_STATE ESQL_WHENEVER_STATE

JPNWORD [\xA0-\xDF]|([\x81-\x9F\xE0-\xFC][\x40-\x7E\x80-\xFC])
DIGIT [0-9]
WORD ([A-Za-z\+\-0-9_]|[(]|[)]|[\']|[`])
INCFILE [A-Za-z0-9_\+\-]+
FILENAME [A-Za-z0-9_\+\-\.]+
/* STRVALUE "\""[^\"]+"\""|"\'"[^\'\n]*"\'" */
STRVALUE "\""[^\"]+"\""|"\'"[^\n]*"\'"
HEXVALUE "X\""[^\"]+"\""|"X\'"[^\']+"\'"
SELF [,()\[\].;\:\+\-\*\/\%\^\<\>\=]
OP_CHARS [\~\!\@\#\^\&\|\`\?\+\-\*\/\%\<\>\=]
OPERATOR {OP_CHARS}+
COMPARISON "="|"<>"|"<"|">"|"<="|">="
COMMA ","
PGSQL_CAST_OP "::"
HOSTWORD ":"([A-Za-z\-0-9_]*([\xA0-\xDF]|([\x81-\x9F\xE0-\xFC][\x40-\x7E\x80-\xFC]))*[A-Za-z\-0-9_]*)
INT_CONSTANT {digit}+
LOW_VALUE "LOW\-VALUE"

%%

"EXEC"[ \r\n]+"SQL"		{ 
		__yy_push_state(ESQL_FUNC_STATE); 

		driver.startlineno = yylineno - count_crlf(yytext);;
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();
		driver.sql_list->clear();
		driver.hostref_or_literal_list->clear();	
		
		driver.commandname = "";
		driver.cursorname = "";
		driver.sqlname = "";
		driver.incfilename = "";

		driver.hostreferenceCount = 0;
		driver.period = 0;
		driver.cursor_hold = 0;
		driver.command_putother = 0;

		if (driver.lexer.src_location_stack.size() > 0 && !driver.lexer.src_location_stack.top().is_included)
			driver.has_esql_in_cbl_copybooks = true;

		return yy::gix_esql_parser::make_EXECSQL(loc);
}


"DATA"[ ]+"DIVISION"[ ]*"." {
	__yy_push_state(DATA_DIVISION_STATE);
}

<FD_STATE>{

	({WORD}|{JPNWORD})+ {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}

	"." {    
		__yy_pop_state();
		return yy::gix_esql_parser::make_PERIOD(loc);
	}


	(\r\n|\n) { }

	. {}
}

	
<ESQL_DBNAME_STATE>{
	{HOSTWORD} {
		driver.connectionid = new hostref_or_literal_t(yytext, false);
		__yy_pop_state();

		return yy::gix_esql_parser::make_HOSTTOKEN(yytext, loc);
	}

	({WORD}|{JPNWORD})+ {
		driver.connectionid = new hostref_or_literal_t(yytext, true);
		__yy_pop_state();

		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
}

<ESQL_PREPARE_STATE>{

	"FROM" {
		return yy::gix_esql_parser::make_FROM(loc);
	}

	"END-EXEC" {
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		driver.endlineno = yylineno;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	{HOSTWORD} {
		return yy::gix_esql_parser::make_HOSTTOKEN(yytext, loc);
	}

	{STRVALUE} {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}

	({WORD}|{JPNWORD})+ {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
}

<ESQL_EXECUTE_STATE>{

	"IMMEDIATE" {
		return yy::gix_esql_parser::make_IMMEDIATE(loc);
	}

	"USING" {
		return yy::gix_esql_parser::make_USING(loc);
	}

	"INTO" {
		return yy::gix_esql_parser::make_INTO(yytext, loc);
	}

	"END-EXEC"[ \r\n]*"." {
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		driver.period = 1;
		driver.endlineno = yylineno;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	"END-EXEC" {
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		driver.period = 0;
		driver.endlineno = yylineno;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	{HOSTWORD} {
		//driver.connectionid = new hostref_or_literal_t(yytext, false);
		return yy::gix_esql_parser::make_HOSTTOKEN(yytext, loc);
	}

	{STRVALUE} {
		//driver.connectionid = new hostref_or_literal_t(yytext, true);
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}

	({WORD}|{JPNWORD})+ {
		//driver.connectionid = new hostref_or_literal_t(yytext, true);
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
}

<ESQL_FUNC_STATE>{

	"INCLUDE" {
		driver.period = 0;
		int n = count_crlf(yytext);
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();

		driver.cursorname = "";
		driver.sqlname = "";
		driver.incfilename = "";
	
		driver.hostreferenceCount = 0;
		driver.command_putother = 0;
		driver.sql_list->clear();

		__yy_push_state(ESQL_INCLUDE_STATE); 
		return yy::gix_esql_parser::make_INCLUDE(loc);
	}

	"DECLARE" {
		__yy_push_state(ESQL_DECLARE_STATE);
		return yy::gix_esql_parser::make_DECLARE(loc);
	}

	"PREPARE" {
		__yy_push_state(ESQL_PREPARE_STATE);
		driver.commandname = "PREPARE_STATEMENT";
		driver.statement_name = "";
		driver.statement_source = nullptr;
		return yy::gix_esql_parser::make_PREPARE(loc);
	}
     
	"EXECUTE" {
		__yy_push_state(ESQL_EXECUTE_STATE);
		driver.commandname = "EXECUTE";
		driver.statement_name = "";
		driver.statement_source = nullptr;
		return yy::gix_esql_parser::make_EXECUTE(yytext, loc);
	}
      

	"AT" {
		__yy_push_state(ESQL_DBNAME_STATE);
		return yy::gix_esql_parser::make_AT(loc);
	}
	

	"SELECT" {
		__yy_push_state(ESQL_STATE);
		flag_insqlstring = 1;
		flag_selectcommand = 1;

		driver.commandname = yytext;

		driver.sqlnum++;
		driver.sqlname = string_format("SQ%04d", driver.sqlnum);

		return yy::gix_esql_parser::make_SELECT(yytext, loc);
	}

	"INSERT" {
		__yy_push_state(ESQL_STATE);
		flag_insqlstring = 1;

		driver.commandname = yytext;
					
		driver.sqlnum++;
		driver.sqlname = string_format("SQ%04d", driver.sqlnum);

		return yy::gix_esql_parser::make_INSERT(yytext, loc);
	}

	"DELETE" {
		__yy_push_state(ESQL_STATE);
		flag_insqlstring = 1;

		driver.commandname = yytext;
					
		driver.sqlnum++;
		driver.sqlname = string_format("SQ%04d", driver.sqlnum);

		return yy::gix_esql_parser::make_DELETE(yytext, loc);
	}		

	"CONNECT" {
		__yy_push_state(ESQL_CONNECT_STATE);

		driver.commandname = "CONNECT";
		return yy::gix_esql_parser::make_CONNECT(loc);
	}

	"CONNECT"[ ]+"RESET" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "CONNECT_RESET";
		return yy::gix_esql_parser::make_CONNECT_RESET(loc);
	}	

     
	"DISCONNECT" {
		__yy_push_state(ESQL_STATE);
		//flag_insqlstring = 1;
		driver.commandname = yytext;		
		return yy::gix_esql_parser::make_DISCONNECT(yytext, loc);
	}

	"UPDATE" {
		__yy_push_state(ESQL_STATE);

		flag_insqlstring = 1;

		driver.commandname = yytext;
					
		driver.sqlnum++;
		driver.sqlname = string_format("SQ%04d", driver.sqlnum);
					
		return yy::gix_esql_parser::make_UPDATE(yytext, loc);
	}	

	"OPEN" {
			__yy_push_state(ESQL_STATE);
    	    driver.commandname = "OPEN";
			return yy::gix_esql_parser::make_OPEN(loc);
    }

	"CLOSE" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "CLOSE";
		return yy::gix_esql_parser::make_CLOSE(loc);
	}  
            
	"FETCH" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "FETCH";
		return yy::gix_esql_parser::make_FETCH(loc);
	}
      
	"COMMIT"[ ]+"WORK"+[ ]+"RELEASE" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "COMMIT";
		driver.transaction_release = true;
		return yy::gix_esql_parser::make_COMMIT_WORK(loc);
	}

	"COMMIT"[ ]+"WORK"+[ ]+"WITH"+[ ]+"RELEASE" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "COMMIT";
		driver.transaction_release = true;
		return yy::gix_esql_parser::make_COMMIT_WORK(loc);
	}

	"COMMIT"[ ]+"WORK" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "COMMIT";
		driver.transaction_release = false;
		return yy::gix_esql_parser::make_COMMIT_WORK(loc);
	}
     
	"COMMIT" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "COMMIT";
		driver.transaction_release = false;
		return yy::gix_esql_parser::make_COMMIT_WORK(loc);
	}
     
	"ROLLBACK"[ ]+"WORK"+[ ]+"RELEASE" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "ROLLBACK";
		driver.transaction_release = true;
		return yy::gix_esql_parser::make_ROLLBACK_WORK(loc);
	}

	"ROLLBACK"[ ]+"WORK"+[ ]+"WITH"+[ ]+"RELEASE" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "ROLLBACK";
		driver.transaction_release = true;
		return yy::gix_esql_parser::make_ROLLBACK_WORK(loc);
	}

	"ROLLBACK"[ ]+"WORK" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "ROLLBACK";
		driver.transaction_release = false;
		return yy::gix_esql_parser::make_ROLLBACK_WORK(loc);
	}     

	"ROLLBACK" {
		__yy_push_state(ESQL_STATE);

		driver.commandname = "ROLLBACK";
		driver.transaction_release = false;
		return yy::gix_esql_parser::make_ROLLBACK_WORK(loc);
	}     

	"IGNORE" {
		__yy_push_state(ESQL_IGNORE_STATE);

		driver.commandname = "IGNORE";
		driver.text_content = "";
		driver.in_ignore_string = true;
		return yy::gix_esql_parser::make_IGNORE(loc);	
	}

	"WHENEVER" {
		__yy_push_state(ESQL_WHENEVER_STATE);
		driver.commandname = "WHENEVER";
		return yy::gix_esql_parser::make_WHENEVER(loc);
	}

	({WORD}|{JPNWORD})+ {
		__yy_push_state(ESQL_STATE);

		flag_insqlstring = 1;

		driver.commandname = "PASSTHRU";

		driver.sqlnum++;
		driver.sqlname = string_format("SQ%04d", driver.sqlnum);
		
		driver.command_putother = 1;
		return yy::gix_esql_parser::make_OTHERFUNC(yytext, loc);
	}

}

<ESQL_IGNORE_STATE>{

	"END-EXEC"[ \r\n]*"." {
		driver.endlineno = yylineno;
		driver.in_ignore_string = false;
		driver.period = 1;
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	"END-EXEC" {
		driver.endlineno = yylineno;
		driver.in_ignore_string = false;
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	(\r\n|\n|\t) {   
		driver.text_content += yytext;
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);	
	}

	. { 
		driver.text_content += yytext;
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}

}

<ESQL_WHENEVER_STATE>{

	"END-EXEC"[ \r\n]*"." {
		driver.endlineno = yylineno;
		driver.in_ignore_string = false;
		driver.period = 1;
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	"END-EXEC" {
		driver.endlineno = yylineno;
		driver.in_ignore_string = false;
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	"NOT"[ \r\n]+"FOUND" {
		return yy::gix_esql_parser::make_NOT_FOUND(loc);
	}

	"SQLERROR" {
		return yy::gix_esql_parser::make_SQLERROR(loc);
	}

	"SQLWARNING" {
		return yy::gix_esql_parser::make_SQLWARNING(loc);
	}

	"CONTINUE" {
		return yy::gix_esql_parser::make_CONTINUE(loc);
	}

	"PERFORM" {
		return yy::gix_esql_parser::make_PERFORM(loc);
	}

	"GOTO"|"GO"[ \r\n]+"TO" {
		return yy::gix_esql_parser::make_GOTO(loc);
	}

	{HOSTWORD} { 
		return yy::gix_esql_parser::make_HOSTTOKEN(yytext, loc);
	}

	([A-Za-z\-0-9_]|{JPNWORD})+ {
		return yy::gix_esql_parser::make_WORD(yytext, loc);
    }

	/*
	(\r\n|\n|\t) {   
		driver.text_content += yytext;
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);	
	}

	. { 
		driver.text_content += yytext;
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
	*/
}

<ESQL_CONNECT_STATE>{
	"TO" { return yy::gix_esql_parser::make_TO(loc); }
	"AT" { return yy::gix_esql_parser::make_AT(loc); }
	"AS" { return yy::gix_esql_parser::make_AS(loc); }

	"USER" { return yy::gix_esql_parser::make_USER(loc); }

	"USING" { return yy::gix_esql_parser::make_USING(loc); }

	"IDENTIFIED"[ \r\n]+"BY" { return yy::gix_esql_parser::make_IDENTIFIED_BY(loc); }

	"END-EXEC"[ \r\n]*"." {
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		driver.endlineno = yylineno;
		driver.period = 1;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	"END-EXEC" {
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		driver.endlineno = yylineno;
		driver.period = 0;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	
	{HOSTWORD} { 
		return yy::gix_esql_parser::make_HOSTTOKEN(yytext, loc);
	}

	{STRVALUE} {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}

	({WORD}|{JPNWORD})+ {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
	          
	(\r\n|\n) {   }

	[;]?(\r\n|\n)		{ 
				ECHO; 
	} 

}

<ESQL_STATE>{

	{COMMA}   {
					return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	          }
	(\r\n|\n) {   }
         


	[;]?(\r\n|\n)		{ 
				return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	} 

	"SELECT" {
			if(flag_insqlstring){
					return yy::gix_esql_parser::make_TOKEN(yytext, loc);
			}
			flag_insqlstring = 1;

			driver.commandname = yytext;
					
			driver.sqlnum++;
			driver.sqlname = string_format("SQ%04d", driver.sqlnum);

			return yy::gix_esql_parser::make_SELECT(yytext, loc);
	}
	
	"FROM" {
			if(flag_insqlstring){
				if(!flag_selectcommand){
						return yy::gix_esql_parser::make_TOKEN(yytext, loc);
				} else {
					if (!flag_select_from_passed) {
						flag_select_from_passed = 1;
		      			return yy::gix_esql_parser::make_SELECTFROM(yytext, loc);
					}
					else {
						return yy::gix_esql_parser::make_TOKEN(yytext, loc);
					}
				}
			}
			return yy::gix_esql_parser::make_FROM(loc);
	}  

	"TO" {
		if (driver.commandname == "ROLLBACK")
			return yy::gix_esql_parser::make_TO(loc);
		else
			return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
	
	"SAVEPOINT" {
		if (driver.commandname == "ROLLBACK")
			return yy::gix_esql_parser::make_SAVEPOINT(loc);
		else
			return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
     
	"CURSOR" {
			if(flag_insqlstring){ 
					return yy::gix_esql_parser::make_TOKEN(yytext, loc);
			}
			return yy::gix_esql_parser::make_CURSOR(loc);
	 }

	 "WITH"[ ]+"HOLD" {
		return yy::gix_esql_parser::make_WITH_HOLD(1, loc);
	 }

	"FOR" {
			if(flag_insqlstring){
				return yy::gix_esql_parser::make_TOKEN(yytext, loc);   
			}
			return yy::gix_esql_parser::make_FOR(loc);
	}      

	"IDENTIFIED"[ ]+"BY" {
			if(flag_insqlstring){   
				return yy::gix_esql_parser::make_TOKEN(yytext, loc);  
			}
			return yy::gix_esql_parser::make_IDENTIFIED_BY(loc);

	}
  
	"USING" {
			if(flag_insqlstring){  
				return yy::gix_esql_parser::make_TOKEN(yytext, loc);
			}
			return yy::gix_esql_parser::make_USING(loc);
	} 
     
	"INTO" {
			if(flag_insqlstring && !flag_selectcommand){
				return yy::gix_esql_parser::make_TOKEN(yytext, loc);
			}
			return yy::gix_esql_parser::make_INTO(yytext, loc);
	} 

	"ALL" {
			if(flag_insqlstring && !flag_selectcommand){
				return yy::gix_esql_parser::make_TOKEN(yytext, loc);
			}
			return yy::gix_esql_parser::make_ALL(loc);
	} 

	"WHERE CURRENT OF" {
		return yy::gix_esql_parser::make_WHERE_CURRENT_OF(loc);
	}

	{OPERATOR} {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
	
	{HOSTWORD} {
			driver.hostlineno = yylineno;
			return yy::gix_esql_parser::make_HOSTTOKEN(yytext, loc);
	}
	
	{STRVALUE} {
			driver.hostlineno = yylineno;   
			return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}	

	{PGSQL_CAST_OP} {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
	
	/*
	{FILENAME} {
			driver.hostlineno = yylineno;   
			return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}	
	*/
	
	"END-EXEC"[ \r\n]*"." {
			flag_insqlstring = 0;
			flag_selectcommand = 0;
			flag_select_from_passed = 0;
			driver.period = 1;
			driver.endlineno = yylineno;
			__yy_pop_state();	// Not an error, we pop twice
			__yy_pop_state();

			return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	"END-EXEC" {
			flag_insqlstring = 0;
			flag_selectcommand = 0;
			flag_select_from_passed = 0;
			driver.endlineno = yylineno;
			__yy_pop_state();	// Not an error, we pop twice
			__yy_pop_state();

			return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	({WORD}|{JPNWORD})+("."(("*")|({WORD}|{JPNWORD})+))? { 
			  return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}

	{SELF} {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
}

"COPY"[ ]+({INCFILE})[ ]*"." {

    if (driver.opt_preprocess_copy_files) {
		driver.startlineno = yylineno;
		driver.endlineno = yylineno;
		
		driver.commandname = "INCFILE";		
		
		int p = find_last_space(yytext);
		if (p < 0)
			p = 5;
		
		std::string tts = std::string(yytext).substr(p);
		tts = string_chop(tts, 1);
		
		driver.incfilename = tts;

		return yy::gix_esql_parser::make_COPY(loc);
	}
}
	/*
	"EXEC"[ ]+"SQL"[ \r\n]+"INCLUDE" {
		driver.period = 0;
		int n = count_crlf(yytext);
		driver.startlineno = yylineno - n; 
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();

		driver.cursorname = "";
		driver.sqlname = "";
		driver.incfilename = "";
	
		driver.hostreferenceCount = 0;
		driver.command_putother = 0;
		driver.sql_list->clear();

		__yy_push_state(ESQL_INCLUDE_STATE); 
		return yy::gix_esql_parser::make_EXECSQL_INCLUDE(loc);
	}
	*/

<ESQL_INCLUDE_STATE>{
	(\r\n|\n) {   } 

	"SQLCA" {
		driver.commandname = "INCSQLCA";		
		return yy::gix_esql_parser::make_INCLUDE_SQLCA(loc);
	}

	"END-EXEC"[ \r\n]*"." {
		driver.period = 1;
		driver.endlineno = yylineno;
		flag_insqlstring = 0;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state(); 
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	"END-EXEC" {
		driver.endlineno = yylineno;
		flag_insqlstring = 0;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state(); 
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	{INCFILE} {
		driver.commandname = "INCFILE";		
		driver.incfilename = yytext;
		return yy::gix_esql_parser::make_INCLUDE_FILE(loc);
	}
}

"PROCEDURE"[ ]+"DIVISION"[^\.]*"." {

	if (driver.data_division_section != DD_SECTION_INITIAL) {
		yy_pop_state();
		switch (driver.data_division_section) {
			case DD_SECTION_WS:
				driver.data_division_section = DD_SECTION_INITIAL;
				driver.commandname ="WORKING_END";
				driver.startlineno = yylineno - 1;
				driver.endlineno = yylineno - 1;
				UNPUT_TOKEN();
				return yy::gix_esql_parser::make_WORKINGEND(loc); 

			case DD_SECTION_LL:
				driver.data_division_section = DD_SECTION_INITIAL;
				driver.commandname ="LOCALSTORAGE_END";
				driver.startlineno = yylineno - 1;
				driver.endlineno = yylineno - 1;
				UNPUT_TOKEN();
				return yy::gix_esql_parser::make_LOCALSTORAGEEND(loc); 

			case DD_SECTION_LS:
				driver.data_division_section = DD_SECTION_INITIAL;
				driver.commandname ="LINKAGE_END";
				driver.startlineno = yylineno - 1;
				driver.endlineno = yylineno - 1;
				UNPUT_TOKEN();
				return yy::gix_esql_parser::make_LINKAGEEND(loc); 

			case DD_SECTION_FS:
				driver.data_division_section = DD_SECTION_INITIAL;
				driver.commandname ="FILE_END";
				driver.startlineno = yylineno - 1;
				driver.endlineno = yylineno - 1;
				UNPUT_TOKEN();
				return yy::gix_esql_parser::make_FILEEND(loc); 
		}
	}
	
	driver.startlineno = yylineno;
	driver.endlineno = yylineno;

	driver.startlineno -= count_crlf(yytext);

	driver.host_reference_list->clear();
	driver.res_host_reference_list->clear();
	driver.cursorname = "";		
	driver.sqlname = "";		
	driver.incfilename = "";				

	driver.commandname = "PROCEDURE_DIVISION";		

	driver.procedure_division_started = true;
	driver.data_division_section = DD_SECTION_INITIAL;

	driver.hostreferenceCount = 0;
	driver.command_putother = 0;
	driver.sql_list->clear();


	return yy::gix_esql_parser::make_PROCEDURE_DIVISION(loc);
}

<ESQL_DECLARE_STATE>{
	(\r\n|\n) { }

	{COMMA} {    
		return yy::gix_esql_parser::make_TOKEN(strdup (yytext), loc);
	}

	[;]?(\r\n|\n) { 
		ECHO; 
	}
				
	"TABLE" { 
		return yy::gix_esql_parser::make_TABLE(loc);
	}
	
	"SELECT" {
		if(flag_insqlstring){
			return yy::gix_esql_parser::make_TOKEN(strdup (yytext), loc);
		}
		flag_insqlstring = 1;

		driver.commandname = yytext;
						
		driver.sqlnum++;
     	driver.sqlname = string_format("SQ%04d", driver.sqlnum);

		return yy::gix_esql_parser::make_SELECT(yytext, loc); 
	}
	
	"FROM" {
		if(flag_insqlstring){
			if(!flag_selectcommand){
		      		return yy::gix_esql_parser::make_TOKEN(yytext, loc);      
			} else {
		      		return yy::gix_esql_parser::make_SELECTFROM(strdup (yytext), loc);
			}
		}
		return yy::gix_esql_parser::make_FROM(loc);
	}  
     
	"CURSOR" {
		if(flag_insqlstring){ 
			return yy::gix_esql_parser::make_TOKEN(strdup (yytext), loc);
		}
		return yy::gix_esql_parser::make_CURSOR(loc);
	 }

	 "WITH"[ ]+"HOLD" {
		return yy::gix_esql_parser::make_WITH_HOLD(1, loc);
	 }

	"FOR" {
		if(flag_insqlstring){
			return yy::gix_esql_parser::make_TOKEN(strdup (yytext), loc);
		}
		return yy::gix_esql_parser::make_FOR(loc);
	}     
	
	"STATEMENT" {
		if(flag_insqlstring){
			return yy::gix_esql_parser::make_TOKEN(strdup (yytext), loc);
		}
		return yy::gix_esql_parser::make_STATEMENT(loc);
	}  

     
	"INTO" {
		if(flag_insqlstring && !flag_selectcommand){ 
			return yy::gix_esql_parser::make_TOKEN(yytext, loc);
		}

		return yy::gix_esql_parser::make_INTO(yytext, loc); 
	} 

	{OPERATOR} {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
	
	{HOSTWORD} {
		driver.hostlineno = yylineno;
		return yy::gix_esql_parser::make_HOSTTOKEN(yytext, loc);
	}

	{PGSQL_CAST_OP} {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
	
	"END-EXEC"[ \r\n]*"." {
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		driver.period = 1;
		driver.endlineno = yylineno;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	"END-EXEC" {
		flag_insqlstring = 0;
		flag_selectcommand = 0;
		driver.endlineno = yylineno;
		__yy_pop_state();	// Not an error, we pop twice
		__yy_pop_state();
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	({WORD}|{JPNWORD})+("."(("*")|({WORD}|{JPNWORD})+))? {
		return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
}



<VAR_DECLARE_STATE>{

	"IS" { return yy::gix_esql_parser::make_IS(loc); }

	
    "BINARY"(\([0-9]+\))?  { 
		uint32_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_BINARY(l, loc);        
	}
    "VARBINARY"(\([0-9]+\))?  { 
		uint32_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_VARBINARY(l, loc);          
	}
    "CHAR"(\([0-9]+\))?  { 
		uint32_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_CHAR(l, loc);            
	}
    "VARCHAR2"(\([0-9]+\))?  { 
		uint32_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_VARCHAR(l, loc);      
	}
    "VARCHAR"(\([0-9]+\))?  { 
		uint32_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_VARCHAR(l, loc);      
	}
    "CHARACTER"[\-]VARYING(\([0-9]+\))?  { 
		uint32_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_VARCHAR(l, loc);      
	}

    "FLOAT"(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|"REAL"(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?  { 
		uint64_t ps = 0;
		uint32_t precision;
		uint16_t scale;
		extract_precision_scale(yytext, &precision, &scale);
		ps = precision;
		ps = (ps << 16) | scale;
        return yy::gix_esql_parser::make_FLOAT(ps, loc);      
	}

    "INTEGER"(\([0-9]+\))?  { 
		uint64_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_INTEGER(l, loc);      
	}

    "DECIMAL"(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|"NUMERIC"(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?  { 
		uint64_t ps = 0;
		uint32_t precision;
		uint16_t scale;
		extract_precision_scale(yytext, &precision, &scale);
		ps = precision;
		ps = (ps << 16) | scale;
        return yy::gix_esql_parser::make_DECIMAL(ps, loc); 
	}

    "RAW"  { 
        return yy::gix_esql_parser::make_VARBINARY(0, loc);      
	}

	"END-EXEC" {
			driver.endlineno = yylineno;
			flag_insqlstring = 0;
			__yy_pop_state();
			return yy::gix_esql_parser::make_END_EXEC(loc);
	}

	"END-EXEC"[ \r\n]*"." {
			driver.period = 1;
			driver.endlineno = yylineno;
			flag_insqlstring = 0;
			__yy_pop_state();

			return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	

	({WORD}|{JPNWORD})+("."(("*")|({WORD}|{JPNWORD})+))? {
			  return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
}

<DATA_DIVISION_STATE>{  

	"EXEC"[ ]+"SQL"[ \r\n]+"VAR" {
        driver.startlineno = yylineno;
        driver.endlineno = yylineno;
		driver.host_reference_list->clear();
        driver.res_host_reference_list->clear();     
		__yy_push_state(VAR_DECLARE_STATE);
		return yy::gix_esql_parser::make_DECLARE_VAR(loc);
	}	

    "EXEC"[ ]+"SQL"[ ]+"BEGIN"[ ]+"DECLARE"[ ]+"SECTION"[ ]+"END-EXEC"[ ]*(".")? {
		driver.startlineno = yylineno;
		driver.endlineno = yylineno;
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();

		driver.commandname = "HOST_BEGIN";		
		driver.cursorname = "";
		driver.sqlname = "";
		driver.incfilename = "";

		driver.period = yytext[strlen(yytext)-1] == '.' ? 1 : 0;

		driver.hostreferenceCount = 0;
		driver.command_putother = 0;
		driver.sql_list->clear();

		return yy::gix_esql_parser::make_HOSTVARIANTBEGIN(loc);
    }

    "EXEC"[ ]+"SQL"[ ]+"END"[ ]+"DECLARE"[ ]+"SECTION"[ ]+"END-EXEC"[ ]*(".")? {
		driver.startlineno = yylineno;
		driver.endlineno = yylineno;
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();

		driver.commandname = "HOST_END";		
		driver.cursorname = "";		
		driver.sqlname = "";		
		driver.incfilename = "";		

		driver.period = yytext[strlen(yytext)-1] == '.' ? 1 : 0;

		driver.hostreferenceCount = 0;
		driver.command_putother = 0;
		driver.sql_list->clear();

		return yy::gix_esql_parser::make_HOSTVARIANTEND(loc);
    }

	^[ ]*"WORKING-STORAGE"[ ]+"SECTION"[ ]*"." |
	^[ ]*"LOCAL-STORAGE"[ ]+"SECTION"[ ]*"." |
	^[ ]*"LINKAGE"[ ]+"SECTION"[ ]*"." |
	^[ ]*"FILE"[ ]+"SECTION"[ ]*"." {
			driver.startlineno = yylineno;
			driver.endlineno = yylineno;
			driver.host_reference_list->clear();
			driver.res_host_reference_list->clear();	
			driver.cursorname = "";
			driver.sqlname = "";
			driver.incfilename = "";
			driver.hostreferenceCount = 0;
			driver.command_putother = 0;
			driver.sql_list->clear();

			if (driver.data_division_section != DD_SECTION_INITIAL) {
				switch (driver.data_division_section) {
					case DD_SECTION_WS:
						driver.data_division_section = DD_SECTION_INITIAL;
						driver.commandname ="WORKING_END";
						driver.startlineno = yylineno - 1;
						driver.endlineno = yylineno - 1;
						UNPUT_TOKEN();
						SET_AT_BOL();
						return yy::gix_esql_parser::make_WORKINGEND(loc); 

					case DD_SECTION_LL:
						driver.data_division_section = DD_SECTION_INITIAL;
						driver.commandname ="LOCALSTORAGE_END";
						driver.startlineno = yylineno - 1;
						driver.endlineno = yylineno - 1;
						UNPUT_TOKEN();
						SET_AT_BOL();
						return yy::gix_esql_parser::make_LOCALSTORAGEEND(loc); 

					case DD_SECTION_LS:
						driver.data_division_section = DD_SECTION_INITIAL;
						driver.commandname ="LINKAGE_END";
						driver.startlineno = yylineno - 1;
						driver.endlineno = yylineno - 1;
						UNPUT_TOKEN();
						SET_AT_BOL();
						return yy::gix_esql_parser::make_LINKAGEEND(loc); 

					case DD_SECTION_FS:
						driver.data_division_section = DD_SECTION_INITIAL;
						driver.commandname ="FILE_END";
						driver.startlineno = yylineno - 1;
						driver.endlineno = yylineno - 1;
						UNPUT_TOKEN();
						SET_AT_BOL();
						return yy::gix_esql_parser::make_FILEEND(loc); 
				}
			}

			if (strncasecmp(yytext,"WORKING-STORAGE", 15) == 0) {
					driver.commandname ="WORKING_BEGIN";
					driver.data_division_section = DD_SECTION_WS;
					return yy::gix_esql_parser::make_WORKINGBEGIN(loc); 
				}
				else 
					if (strncasecmp(yytext,"LOCAL-STORAGE", 13) == 0) {
						driver.commandname ="LOCALSTORAGE_BEGIN";
						driver.data_division_section = DD_SECTION_LL;
						return yy::gix_esql_parser::make_LINKAGEBEGIN(loc);
					}
					else 
						if (strncasecmp(yytext,"LINKAGE", 7) == 0) {
							driver.commandname ="LINKAGE_BEGIN";
							driver.data_division_section = DD_SECTION_LS;
							return yy::gix_esql_parser::make_LINKAGEBEGIN(loc);
						}
						else 
							if (strncasecmp(yytext,"FILE", 4) == 0) {
								driver.commandname ="FILE_BEGIN";
								driver.data_division_section = DD_SECTION_FS;
								return yy::gix_esql_parser::make_FILEBEGIN(loc);
							}		

			return yy::gix_esql_parser::make_WORD(yytext, loc);	// should never happen
	}


    ("66"|"77"|"78"|"88")[^\.]*"." {}

    "OBJECT-STORAGE"[ ]+"SECTION"[ ]*"." |
    "COMMUNICATION"[ ]+"SECTION"[ ]*"." |
    "REPORT"[ ]+"SECTION"[ ]*"." |
    "SCREEN"[ ]+"SECTION"[ ]*"." {
		driver.startlineno = yylineno - 1;
		driver.endlineno = yylineno - 1;
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();
		
		driver.commandname = "WORKING_END";		
		driver.cursorname = "";		
		driver.sqlname = "";		
		driver.incfilename = "";		
		driver.data_division_section = DD_SECTION_INITIAL;

		driver.hostreferenceCount = 0;
		driver.command_putother = 0;
		driver.sql_list->clear();
   
		__yy_pop_state();
		return yy::gix_esql_parser::make_WORKINGEND(loc);
    }

	"FD" {
		if (driver.data_division_section == DD_SECTION_FS) {
			__yy_push_state(FD_STATE);
		}
		return yy::gix_esql_parser::make_FD(loc);
	}

    "PIC" |
    "PICTURE" {
		__yy_push_state(PICTURE_STATE);
    }

    "OCCURS"  {
		return yy::gix_esql_parser::make_OCCURS(loc);
	}

    "UNBOUNDED"  {
		return yy::gix_esql_parser::make_UNBOUNDED(loc);
	}

    "TO"  {
		return yy::gix_esql_parser::make_TO(loc);
	}

	"DEPENDING"([ \r\n]+"ON")? {
		return yy::gix_esql_parser::make_DEPENDING_ON(loc);
	}	
	
	"ASCENDING"([ \r\n]+"KEY"([ \r\n]+"IS")?)? {
		return yy::gix_esql_parser::make_ASCENDING_KEY_IS(loc);
	}

	"INDEXED"([ \r\n]+"BY")? {
		return yy::gix_esql_parser::make_INDEXED_BY(loc);
	}
	
	([0-9]+)|([0-9]+\.[0-9]+) {
		return yy::gix_esql_parser::make_NUMERIC(atoi(yytext), loc);
	}

    "USAGE"  {return yy::gix_esql_parser::make_USAGE(loc);}
    "COMP"  { 
        return yy::gix_esql_parser::make_COMP(loc);     
	} 

	"VARYING" {
		return yy::gix_esql_parser::make_VARYING(loc);
	}

    "BINARY"(\([0-9]+\))?  { 
		uint64_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_BINARY(l, loc);      
	}

    "VARBINARY"(\([0-9]+\))?  { 
		uint64_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_VARBINARY(l, loc);      
	}

    "CHAR"(\([0-9]+\))?  { 
		uint64_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_CHAR(l, loc);      
	}

    "VARCHAR2"(\([0-9]+\))?  { 
		uint64_t l = extract_len(yytext);
		l = l << 16; 
        return yy::gix_esql_parser::make_VARCHAR(l, loc);      
	}

    "VARCHAR"(\([0-9]+\))?  { 
		uint64_t l = extract_len(yytext);
		l = l << 16; 
        return yy::gix_esql_parser::make_VARCHAR(l, loc);      
	}

    "CHARACTER"[\-]VARYING(\([0-9]+\))?  { 
		uint64_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_VARCHAR(l, loc);      
	}

    "FLOAT"(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|"REAL"(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?  { 
		uint64_t ps = 0;
		uint32_t precision;
		uint16_t scale;
		extract_precision_scale(yytext, &precision, &scale);
		ps = precision;
		ps = (ps << 16) | scale;
        return yy::gix_esql_parser::make_FLOAT(ps, loc);      
	}

    "INTEGER"(\([0-9]+\))?  { 
		uint64_t l = extract_len(yytext);
		l = l << 16;
        return yy::gix_esql_parser::make_INTEGER(l, loc);      
	}

    "DECIMAL"(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|"NUMERIC"(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?|(\([0-9]+[ ]*(,[ ]*[0-9]+)?\))?  { 
		uint64_t ps = 0;
		uint32_t precision;
		uint16_t scale;
		extract_precision_scale(yytext, &precision, &scale);
		ps = precision;
		ps = (ps << 16) | scale;
        return yy::gix_esql_parser::make_DECIMAL(ps, loc); 
	}

    "RAW"  { 
        return yy::gix_esql_parser::make_VARBINARY(0, loc);      
	}

    "COMP-1"  { 
        return yy::gix_esql_parser::make_COMP_1(loc);        
    }

    "COMP-2"  { 
        return yy::gix_esql_parser::make_COMP_2(loc);
    }

    "COMP-3" {
        return yy::gix_esql_parser::make_COMP_3(loc);
    }

    "COMP-5" {
        return yy::gix_esql_parser::make_COMP_5(loc);
    }

	"SQL"[ ]+"TYPE"[ ]+"IS" {
		driver.startlineno = yylineno;
		driver.endlineno = yylineno;
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();

		driver.cursorname = "";		
		driver.sqlname = "";		
		driver.incfilename = "";		

		driver.hostreferenceCount = 0;
		driver.command_putother = 0;

		return yy::gix_esql_parser::make_SQL_TYPE_IS(loc);
	}
        
    "SIGN"  { return yy::gix_esql_parser::make_SIGN(loc) ;} 
    "LEADING" { return yy::gix_esql_parser::make_LEADING(loc);}
    "SEPARATE" { return yy::gix_esql_parser::make_SEPARATE(loc); }
    "TRAILING" { return yy::gix_esql_parser::make_TRAILING(loc); }
    "EXTERNAL"  { return yy::gix_esql_parser::make_EXTERNAL(loc);}
    "IS"  { return yy::gix_esql_parser::make_IS(loc);}
    "ARE"  { return yy::gix_esql_parser::make_ARE(loc);}
    "TIMES"  { return yy::gix_esql_parser::make_TIMES(loc);}
    "VALUE"|"VALUES" { return yy::gix_esql_parser::make_VALUE(loc);}
    "ALL"  { return yy::gix_esql_parser::make_ALL(loc);} 
    {STRVALUE}|{HEXVALUE}|{LOW_VALUE} { return yy::gix_esql_parser::make_CONST(loc); }
	([A-Za-z\-0-9_]|{JPNWORD})+ {
		return yy::gix_esql_parser::make_WORD(yytext, loc);
    }

	"." {    
		return yy::gix_esql_parser::make_PERIOD(loc);
	}

	(\r\n|\n) { }

	. {}
}

<INCLUDE_STATE>{
	(\r\n|\n) {   }

	{INCFILE}[ ]*"." {
		driver.commandname = "INCFILE";
		driver.incfilename = std::string(yytext) + ".";
		__yy_pop_state();
	    return yy::gix_esql_parser::make_COPY_FILE(loc);
	}
}


<PICTURE_STATE>{
  "IS" {
	/* ignore */
  }

  [^ \r\n;\.]+(\.[^ \r\n;\.]+)* {
	
	__yy_pop_state();
	return yy::gix_esql_parser::make_PICTURE(yytext, loc);
  }
  
}

	/* default rules */

<*>(\r\n|\n) {
	
}

<*>[ \t]+ {
     
	//Ignore 
}
 
. {
	if (strlen(yytext) == 1 && yytext[0] == '.') {
		if (!driver.procedure_division_started && string_contains(cur_line_content, "PROGRAM-ID", true)) {
            std::string pid = string_replace_regex(cur_line_content, "PROGRAM-ID", "", true);
			pid = trim_copy(string_replace(pid, ".", ""));
			driver.program_id = pid;
		}
		else
			if (isParagraph(cur_line_content)) {
				srcLocation *loc = new srcLocation();
				loc->filename = driver.lexer.src_location_stack.top().filename;
				loc->line = yylineno;
				loc->is_included = driver.lexer.src_location_stack.size() > 1;
				std::string paragraph_name = trim_copy(string_chop(trim_copy(cur_line_content), 1));
			
				driver.paragraphs[paragraph_name] = *loc;
			}
		}
}


<<EOF>> {
	return yy::gix_esql_parser::make_YYEOF(loc);
	//yyterminate ();
}


%%

// CHANGE: The "parts of the driver that need lexer data" have been
// moved to gix_esql_driver.cc (where they really belong) and access the
// new lexer object via its public interface.

// CHANGE: The linker will choke if there's no implementation of the
// default `yylex` even if it's never called.
int yyFlexLexer::yylex() {
    std::cerr << "'int yyFlexLexer::yylex()' should never be called." << std::endl;
    exit(1);
}


int find_last_space(char * s)
{
	char *e = (s + strlen(s)) - 1;
	char * p;
	
	for (p = e; p >= s; p--) {
		if (*p == ' ')
			return (p - s) + 1;
	}
	return -1;

}

int count_crlf(char *s)
{
	int n = 0;
	char *e = (s + strlen(s)) - 1;
	//while (*e) {
	while (e >= s) {
		if (*e == '\n')
			n++;
			
		e--;
	}
	return n;
}

uint32_t extract_len(char * s)
{
	std::string st = s;
	
	auto pos = st.find("(");
	if (pos == std::string::npos)
		return 0;

	st = st.substr(pos + 1);

	pos = st.find(")");
	if (pos == std::string::npos)
		return 0;

	st = st.substr(0, pos);

	return (uint32_t) atoi(st.c_str());
}

void extract_precision_scale(char *s, uint32_t *precision, uint16_t *scale)
{
	*precision = 0;
	*scale = 0;

	std::string st = s;
	auto pos = st.find("(");
	if (pos == std::string::npos)
		return;

	st = st.substr(pos + 1);

	pos = st.find(")");
	if (pos == std::string::npos)
		return;

	st = st.substr(0, pos);

	pos = st.find(",");
	if (pos == std::string::npos) {
		trim(st);
		*precision = atoi(st.c_str());
		return;
	}

	std::string sn = st.substr(0, pos);
	trim(sn);
	*precision = (uint32_t) atoi(sn.c_str());

	sn = st.substr(pos + 1);
	trim(sn);
	*scale = (uint16_t) atoi(sn.c_str());
}
