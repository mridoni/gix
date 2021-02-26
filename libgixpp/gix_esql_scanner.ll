/* -*- c++ -*- */
%{
#include <cerrno>
#include <climits>
#include <cstdlib>
#include <string>

#include "gix_esql_driver.hh"
#include "gix_esql_parser.hh"

int flag_insqlstring = 0;
int flag_selectcommand = 0;
int cursor_hold = 0;

int find_last_space(char * s);
int count_crlf(char *s);

//#define UNPUT_TOKEN() yyless(strlen(yytext));
#define UNPUT_TOKEN() { int i; char *yycopy = strdup( yytext ); for ( i = yyleng - 1; i >= 0; --i ) unput( yycopy[i] ); free( yycopy ); }

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

%}

/* Options: */
%option 8bit
%option caseless
%option never-interactive
%option yylineno
%option stack


/* Regex abbreviations: */

%s PICTURE_STATE SPECIAL_DECLARE_STATE DATA_SECTION_STATE

%x ESQL_FUNC_STATE ESQL_INCLUDE_STATE ESQL_SELECT_STATE ESQL_STATE INCLUDE_STATE FD_STATE

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
HOSTWORD ":"([A-Za-z\-0-9_]*([\xA0-\xDF]|([\x81-\x9F\xE0-\xFC][\x40-\x7E\x80-\xFC]))*[A-Za-z\-0-9_]*)
INT_CONSTANT {digit}+
LOW_VALUE "LOW\-VALUE"

%%

"EXEC"[ ]+"SQL"		{ 
		BEGIN ESQL_FUNC_STATE; 

		driver.startlineno = yylineno;
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();
					
		driver.commandname = "";
		driver.cursorname = "";
		driver.sqlname = "";
		driver.incfilename = "";

		driver.hostreferenceCount = 0;
		driver.sql_list->clear();
		driver.period = 0;
		driver.cursor_hold = 0;
		driver.command_putother = 0;

		if (driver.lexer.src_location_stack.size() > 0 && !driver.lexer.src_location_stack.top().is_included)
			driver.has_esql_in_cbl_copybooks = true;

		return yy::gix_esql_parser::make_EXECSQL(loc);
}

"FD" {
	if (driver.in_file_section) {
		yy_push_state(FD_STATE);
	}
	//return yy::gix_esql_parser::make_WORD(yytext, loc);
}

<FD_STATE>{

	"." {    
		yy_pop_state();
		//return yy::gix_esql_parser::make_PERIOD(loc);
	}

	(\r\n|\n) { }

	. {}
}


<ESQL_FUNC_STATE>{
	"SELECT" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;
		flag_selectcommand = 1;

		driver.commandname = yytext;

		driver.sqlnum++;
		driver.sqlname.sprintf("SQ%04d", driver.sqlnum);

		return yy::gix_esql_parser::make_SELECT(yytext, loc);
	}

	"INSERT" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		driver.commandname = yytext;
					
		driver.sqlnum++;
		driver.sqlname.sprintf("SQ%04d", driver.sqlnum);

		return yy::gix_esql_parser::make_INSERT(yytext, loc);
	}

	"DELETE" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		driver.commandname = yytext;
					
		driver.sqlnum++;
		driver.sqlname.sprintf("SQ%04d", driver.sqlnum);

		return yy::gix_esql_parser::make_DELETE(yytext, loc);
	}		

	"CONNECT" {
		BEGIN ESQL_STATE;

		driver.commandname = "CONNECT";
		return yy::gix_esql_parser::make_CONNECT(loc);
	}
	
	"CONNECT"[ ]+"TO" {
		BEGIN ESQL_STATE;

		driver.commandname = "CONNECT_TO";
		return yy::gix_esql_parser::make_CONNECT_TO(loc);
	}		
	
	"CONNECT"[ ]+"RESET" {
		BEGIN ESQL_STATE;

		driver.commandname = "CONNECT_RESET";
		return yy::gix_esql_parser::make_CONNECT_RESET(loc);
	}	

     
	"DISCONNECT" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		driver.commandname = yytext;
					
		driver.sqlnum++;
		driver.sqlname.sprintf("SQ%04d", driver.sqlnum);
		
		return yy::gix_esql_parser::make_DISCONNECT(yytext, loc);
	}

	"UPDATE" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		driver.commandname = yytext;
					
		driver.sqlnum++;
		driver.sqlname.sprintf("SQ%04d", driver.sqlnum);
					
		return yy::gix_esql_parser::make_UPDATE(yytext, loc);
	}	
	
	"DECLARE" {
		BEGIN ESQL_STATE;

		//return DECLARE;
		return yy::gix_esql_parser::make_DECLARE(loc);
	}

	"OPEN" {
		BEGIN ESQL_STATE;
			driver.endlineno = yylineno;
    	    driver.commandname = "OPEN";
			return yy::gix_esql_parser::make_OPEN(loc);
    }
     
	"EXECUTE" {
		BEGIN ESQL_STATE;

		driver.commandname = "EXECUTE";
		return yy::gix_esql_parser::make_EXECUTE(yytext, loc);
	}
      
	"CLOSE" {
		BEGIN ESQL_STATE;

		driver.commandname = "CLOSE";
		return yy::gix_esql_parser::make_CLOSE(loc);
	}  
            
	"FETCH" {
		BEGIN ESQL_STATE;

		driver.commandname = "FETCH";
		return yy::gix_esql_parser::make_FETCH(loc);
	}
      
	"COMMIT"[ ]+"WORK"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		driver.commandname = "COMMIT_RELEASE";
		return yy::gix_esql_parser::make_COMMIT_WORK(loc);
	}

	"COMMIT"[ ]+"WORK"+[ ]+"WITH"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		driver.commandname = "COMMIT_RELEASE";
		return yy::gix_esql_parser::make_COMMIT_WORK(loc);
	}

	"COMMIT"[ ]+"WORK" {
		BEGIN ESQL_STATE;

		driver.commandname = "COMMIT";
		return yy::gix_esql_parser::make_COMMIT_WORK(loc);
	}
     
	"COMMIT" {
		BEGIN ESQL_STATE;

		driver.commandname = "COMMIT";
		return yy::gix_esql_parser::make_COMMIT_WORK(loc);
	}
     
	"ROLLBACK"[ ]+"WORK"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		driver.commandname = "ROLLBACK_RELEASE";
		return yy::gix_esql_parser::make_ROLLBACK_WORK(loc);
	}

	"ROLLBACK"[ ]+"WORK"+[ ]+"WITH"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		driver.commandname = "ROLLBACK_RELEASE";
		return yy::gix_esql_parser::make_ROLLBACK_WORK(loc);
	}

	"ROLLBACK"[ ]+"WORK" {
		BEGIN ESQL_STATE;

		driver.commandname = "ROLLBACK";
		return yy::gix_esql_parser::make_ROLLBACK_WORK(loc);
	}     

	"ROLLBACK" {
		BEGIN ESQL_STATE;

		driver.commandname = "ROLLBACK";
		return yy::gix_esql_parser::make_ROLLBACK_WORK(loc);
	}     

	({WORD}|{JPNWORD})+ {
		BEGIN ESQL_STATE;

		driver.commandname = yytext;
					
		driver.sqlnum++;
		driver.sqlname.sprintf("SQ%04d", driver.sqlnum);
		
		driver.command_putother = 1;
		return yy::gix_esql_parser::make_OTHERFUNC(yytext, loc);
	}
}

<ESQL_STATE>{

	"USER" { return yy::gix_esql_parser::make_USER(loc); }
	"TO" { return yy::gix_esql_parser::make_TO(loc); }
	
	{COMMA}   {
					return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	          }
	(\r\n|\n) {   }
         


	[;]?(\r\n|\n)		{ 
				ECHO; 
				}
				
	
	"SELECT" {
			if(flag_insqlstring){
	      			//yylval.s = strdup (yytext);
	      			//return TOKEN;       
					return yy::gix_esql_parser::make_TOKEN(yytext, loc);
			}
			flag_insqlstring = 1;

			driver.commandname = yytext;
					
			driver.sqlnum++;
			driver.sqlname.sprintf("SQ%04d", driver.sqlnum);

			return yy::gix_esql_parser::make_SELECT(yytext, loc);
	}
	
	"FROM" {
			if(flag_insqlstring){
				if(!flag_selectcommand){
						return yy::gix_esql_parser::make_TOKEN(yytext, loc);
				} else {
		      			return yy::gix_esql_parser::make_SELECTFROM(yytext, loc);
				}
			}
			return yy::gix_esql_parser::make_FROM(loc);
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
			//return USING;
			return yy::gix_esql_parser::make_USING(loc);
	} 
     
	"INTO" {
			if(flag_insqlstring && !flag_selectcommand){
	      		//yylval.s = strdup (yytext);
	      		//return TOKEN;      
				return yy::gix_esql_parser::make_TOKEN(yytext, loc);
			}
		    //yylval.s = strdup (yytext);
		    //return INTO;
			return yy::gix_esql_parser::make_INTO(yytext, loc);
	} 

	"WHERE CURRENT OF" {
		return yy::gix_esql_parser::make_WHERE_CURRENT_OF(loc);
	}

	{OPERATOR} {
	    //yylval.s = strdup (yytext);
	    //return TOKEN;      
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
	
	
	"END-EXEC"[ \r\n]*"." {
			flag_insqlstring = 0;
			flag_selectcommand = 0;
			driver.period = 1;
			driver.endlineno = yylineno;
			BEGIN INITIAL;
			//return END_EXEC;
			return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	"END-EXEC" {
			flag_insqlstring = 0;
			flag_selectcommand = 0;
			driver.endlineno = yylineno;
			BEGIN INITIAL;
			//return END_EXEC;
			return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	({WORD}|{JPNWORD})+("."(("*")|({WORD}|{JPNWORD})+))? {
		      //yylval.s = strdup (yytext);
		      //return TOKEN;  
			  return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}

	{SELF} {
	    //yylval.s = strdup (yytext);
	    //return TOKEN;      
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
		
		QString tts = QString(yytext).mid(p);
		tts = tts.left(tts.length() - 1);
		
		driver.incfilename = tts;

		return yy::gix_esql_parser::make_COPY(loc);
	}
}

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

	yy_push_state(ESQL_INCLUDE_STATE); 

	return yy::gix_esql_parser::make_EXECSQL_INCLUDE(loc);
}

<ESQL_INCLUDE_STATE>{
	(\r\n|\n) {   } 
	"SQLCA" {
		driver.commandname = "INCSQLCA";		
		return yy::gix_esql_parser::make_INCLUDE_SQLCA(loc);
	}
	{INCFILE} {
		driver.commandname = "INCFILE";		
		driver.incfilename = yytext;
		return yy::gix_esql_parser::make_INCLUDE_FILE(loc);
	}
	"END-EXEC"[ \r\n]*"." {
		driver.period = 1;
		driver.endlineno = yylineno;
		yy_pop_state(); 
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	"END-EXEC" {
		driver.endlineno = yylineno;
		yy_pop_state(); 
		return yy::gix_esql_parser::make_END_EXEC(loc);
	}
}

"WORKING-STORAGE"[ ]+"SECTION"[ ]*"." |
"LINKAGE"[ ]+"SECTION"[ ]*"." |
"FILE"[ ]+"SECTION"[ ]*"." {
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

		switch (YYSTATE) {
			case INITIAL:
				yy_push_state(DATA_SECTION_STATE);
				if (strncmp(yytext,"WORKING-STORAGE", 15) == 0) {
					driver.commandname ="WORKING_BEGIN";
					driver.in_ws_section = true;
					return yy::gix_esql_parser::make_WORKINGBEGIN(loc); 
				}
				else 
					if (strncmp(yytext,"LINKAGE", 7) == 0) {
						driver.commandname ="LINKAGE_BEGIN";
						driver.in_linkage_section = true;
						return yy::gix_esql_parser::make_LINKAGEBEGIN(loc);
					}
					else 
						if (strncmp(yytext,"FILE", 4) == 0) {
							driver.commandname ="FILE_BEGIN";
							driver.in_file_section = true;
							return yy::gix_esql_parser::make_FILEBEGIN(loc);
						}
				
				break;

			case DATA_SECTION_STATE:
				yy_pop_state();
				if (driver.in_ws_section) {
					driver.in_ws_section = false;
					driver.commandname ="WORKING_END";
					driver.startlineno = yylineno - 1;
					driver.endlineno = yylineno - 1;
					UNPUT_TOKEN();
					return yy::gix_esql_parser::make_WORKINGEND(loc); 
				}
				else
					if (driver.in_linkage_section) {
						driver.in_linkage_section = false; 
						driver.commandname ="LINKAGE_END";
						driver.startlineno = yylineno - 1;
						driver.endlineno = yylineno - 1;
						UNPUT_TOKEN();
						return yy::gix_esql_parser::make_LINKAGEEND(loc); 
					}
					else
						if (driver.in_file_section) {
							driver.in_file_section = false;
							driver.commandname ="FILE_END";
							driver.startlineno = yylineno - 1;
							driver.endlineno = yylineno - 1;
							UNPUT_TOKEN();
							return yy::gix_esql_parser::make_FILEEND(loc); 
						}

				break;

		}

		return yy::gix_esql_parser::make_FILEEND(loc); 
}

"PROCEDURE"[ ]+"DIVISION"[^\.]*"." {

	if (YYSTATE == DATA_SECTION_STATE) {
		yy_pop_state();
		if (driver.in_ws_section) {
			driver.in_ws_section = false;
			driver.commandname ="WORKING_END";
			driver.startlineno = yylineno - 1;
			driver.endlineno = yylineno - 1;
			UNPUT_TOKEN();
			return yy::gix_esql_parser::make_WORKINGEND(loc); 
		}
		else
			if (driver.in_linkage_section) {
				driver.in_linkage_section = false; 
				driver.commandname ="LINKAGE_END";
				driver.startlineno = yylineno - 1;
				driver.endlineno = yylineno - 1;
				UNPUT_TOKEN();
				return yy::gix_esql_parser::make_LINKAGEEND(loc); 
			}
			else
				if (driver.in_file_section) {
					driver.in_file_section = false;
					driver.commandname ="FILE_END";
					driver.startlineno = yylineno - 1;
					driver.endlineno = yylineno - 1;
					UNPUT_TOKEN();
					return yy::gix_esql_parser::make_FILEEND(loc); 
				}	
	}
	
	driver.startlineno = yylineno;
	driver.endlineno = yylineno;
	driver.host_reference_list->clear();
	driver.res_host_reference_list->clear();
	driver.cursorname = "";		
	driver.sqlname = "";		
	driver.incfilename = "";				

	driver.commandname = "PROCEDURE_DIVISION";		

	driver.procedure_division_started = true;
	driver.in_ws_section = false;
	driver.in_linkage_section = false;
	driver.in_file_section = false;

	driver.hostreferenceCount = 0;
	driver.command_putother = 0;
	driver.sql_list->clear();


	return yy::gix_esql_parser::make_PROCEDURE_DIVISION(loc);
}



<SPECIAL_DECLARE_STATE>{
	(\r\n|\n) {   }
         
	{COMMA}   {
	      		//yylval.s = strdup (yytext);
	      		//return TOKEN;      
				return yy::gix_esql_parser::make_TOKEN(strdup (yytext), loc);
	          }

	[;]?(\r\n|\n)		{ 
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
     		driver.sqlname.sprintf("SQ%04d", driver.sqlnum);

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
		    //return CURSOR;
			return yy::gix_esql_parser::make_CURSOR(loc);
	 }

	 "WITH"[ ]+"HOLD" {
		//return WITH_HOLD;
		return yy::gix_esql_parser::make_WITH_HOLD(1, loc);
	 }

	"FOR" {
			if(flag_insqlstring){
	      		//yylval.s = strdup (yytext);
	      		//return TOKEN;      
				return yy::gix_esql_parser::make_TOKEN(strdup (yytext), loc);
			}
			return yy::gix_esql_parser::make_FOR(loc);
	}      

     
	"INTO" {
			if(flag_insqlstring && !flag_selectcommand){
	      		//yylval.s = strdup (yytext);
	      		//return TOKEN;      
				return yy::gix_esql_parser::make_TOKEN(yytext, loc);
			}
		     	//yylval.s = strdup (yytext);
		     	//return INTO;
				return yy::gix_esql_parser::make_INTO(yytext, loc); 
	} 

	{OPERATOR} {
	      		//yylval.s = strdup (yytext);
	      		//return TOKEN;      
				return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
	
	{HOSTWORD} {
			//yylval.s = strdup (yytext + 1);
			driver.hostlineno = yylineno;
			//return HOSTTOKEN;
			return yy::gix_esql_parser::make_HOSTTOKEN(yytext, loc);
	}
	
	"END-EXEC"[ \r\n]*"." {
			flag_insqlstring = 0;
			flag_selectcommand = 0;
			driver.period = 1;
			driver.endlineno = yylineno;
			yy_pop_state();
			return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	"END-EXEC" {
			flag_insqlstring = 0;
			flag_selectcommand = 0;
			driver.endlineno = yylineno;
			yy_pop_state();
			//return END_EXEC;
			return yy::gix_esql_parser::make_END_EXEC(loc);
	}
	
	({WORD}|{JPNWORD})+("."(("*")|({WORD}|{JPNWORD})+))? {
		      //yylval.s = strdup (yytext);
		      //return TOKEN;       
			  return yy::gix_esql_parser::make_TOKEN(yytext, loc);
	}
}

<DATA_SECTION_STATE>{

	"EXEC"[ ]+"SQL"[ ]+"DECLARE"[ ]+ {
		yy_push_state(SPECIAL_DECLARE_STATE);
                driver.startlineno = yylineno;
                driver.endlineno = yylineno;
                driver.host_reference_list->clear();
                driver.res_host_reference_list->clear();        
		return yy::gix_esql_parser::make_BEGIN_DECLARE_SPECIAL(loc);
	}	  
	
	"EXEC"[ ]+"SQL"[ \r\n]+"DECLARE"[ ]+ {
		yy_push_state(SPECIAL_DECLARE_STATE);
                driver.startlineno = yylineno-1;
                driver.endlineno = yylineno;
                driver.host_reference_list->clear();
                driver.res_host_reference_list->clear();                
		return yy::gix_esql_parser::make_BEGIN_DECLARE_SPECIAL(loc);
	}		


    "EXEC"[ ]+"SQL"[ ]+"BEGIN"[ ]+"DECLARE"[ ]+"SECTION"[ ]+"END-EXEC"[ ]*"." {
		driver.startlineno = yylineno;
		driver.endlineno = yylineno;
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();

		driver.commandname = "HOST_BEGIN";		
		driver.cursorname = "";
		driver.sqlname = "";
		driver.incfilename = "";

		driver.hostreferenceCount = 0;
		driver.command_putother = 0;
		driver.sql_list->clear();

		//return HOSTVARIANTBEGIN;
		return yy::gix_esql_parser::make_HOSTVARIANTBEGIN(loc);
    }

    "EXEC"[ ]+"SQL"[ ]+"END"[ ]+"DECLARE"[ ]+"SECTION"[ ]+"END-EXEC"[ ]*"." {
		driver.startlineno = yylineno;
		driver.endlineno = yylineno;
		driver.host_reference_list->clear();
		driver.res_host_reference_list->clear();

		driver.commandname = "HOST_END";		
		driver.cursorname = "";		
		driver.sqlname = "";		
		driver.incfilename = "";		

		driver.hostreferenceCount = 0;
		driver.command_putother = 0;
		driver.sql_list->clear();

		//return HOSTVARIANTEND;
		return yy::gix_esql_parser::make_HOSTVARIANTEND(loc);
    }

    "COPY"[ ]+({INCFILE})[ ]*"." {
		driver.startlineno = yylineno;
		driver.endlineno = yylineno;
		
		driver.commandname = "INCFILE";		
		
		int p = find_last_space(yytext);
		if (p < 0)
			p = 5;
		
		QString tts = QString(yytext).mid(p);
		tts = tts.left(tts.length() - 1);
		
		driver.incfilename = tts;

		return yy::gix_esql_parser::make_COPY(loc);
	}



    ("66"|"77"|"78"|"88")[^\.]*"." {}

    "OBJECT-STORAGE"[ ]+"SECTION"[ ]*"." |
    "LOCAL-STORAGE"[ ]+"SECTION"[ ]*"." |
    "FILE"[ ]+"SECTION"[ ]*"." |
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
		driver.in_ws_section = false;

		driver.hostreferenceCount = 0;
		driver.command_putother = 0;
		driver.sql_list->clear();
   
		BEGIN INITIAL;
		return yy::gix_esql_parser::make_WORKINGEND(loc);
    }


    "PIC" |
    "PICTURE" {
		yy_push_state(PICTURE_STATE);
    }

    "OCCURS"  {
		return yy::gix_esql_parser::make_OCCURS(loc);
	}

	([0-9]+)|([0-9]+\.[0-9]+) {
		return yy::gix_esql_parser::make_NUMERIC(atoi(yytext), loc);
	}

    "USAGE"  {return yy::gix_esql_parser::make_USAGE(loc);}
    "COMP"  { 
        return yy::gix_esql_parser::make_COMP(loc);     
	} 
    "BINARY"  { 
        return yy::gix_esql_parser::make_BINARY(loc);      
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
		driver.incfilename = QString(yytext) + ".";
		yy_pop_state();
	    return yy::gix_esql_parser::make_COPY_FILE(loc);
	}
}


<PICTURE_STATE>{
  "IS" {
	/* ignore */
  }
  [^ \r\n;\.]+(\.[^ \r\n;\.]+)* {
	
	yy_pop_state();

	return yy::gix_esql_parser::make_PICTURE(yytext, loc);
  }
  
}

<*>(\r\n|\n) {
	
}

<*>[ \t]+ {
     
	//Ignore 
}
(\r\n|\n) { 

}
 
. {
	if (strlen(yytext) == 1 && yytext[0] == '.') {
		if (!driver.procedure_division_started && cur_line_content.contains("PROGRAM-ID")) {
			QString pid = cur_line_content.replace("PROGRAM-ID", "");
			pid = pid.replace(".", "").trimmed();
			driver.program_id = pid;
		}
		else
			if (isParagraph(cur_line_content)) {
				srcLocation *loc = new srcLocation();
				loc->filename = driver.lexer.src_location_stack.top().filename;
				loc->line = yylineno;
				loc->is_included = driver.lexer.src_location_stack.size() > 1;
				QString paragraph_name = cur_line_content.trimmed().chopped(1).trimmed();
			
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
