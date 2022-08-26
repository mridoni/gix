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

#pragma once

#include <string>

#define SIGN_LENGTH 1
#define TERMINAL_LENGTH 1
#define DECIMAL_LENGTH 1

// We repeat these here
#define COBOL_TYPE_UNSIGNED_NUMBER		1         
#define COBOL_TYPE_SIGNED_NUMBER_TS		2        // (trailing separate)
#define COBOL_TYPE_SIGNED_NUMBER_TC		3        // (trailing combined)
#define COBOL_TYPE_SIGNED_NUMBER_LS		4        // (leading separate)
#define COBOL_TYPE_SIGNED_NUMBER_LC		5        // (leading combined)
#define COBOL_TYPE_UNSIGNED_NUMBER_PD	8		 // packed decimal
#define COBOL_TYPE_SIGNED_NUMBER_PD		9		 // packed decimal
#define COBOL_TYPE_ALPHANUMERIC			16
#define COBOL_TYPE_UNSIGNED_BINARY		22
#define COBOL_TYPE_SIGNED_BINARY		23
#define COBOL_TYPE_JAPANESE				24     
#define COBOL_TYPE_GROUP        		25            
#define COBOL_TYPE_FLOAT				26                                        
#define COBOL_TYPE_DOUBLE				27                                        
#define COBOL_TYPE_NATIONAL				28     

#define COBOL_TYPE_MIN					1 
#define COBOL_TYPE_MAX					28

#define COBOL_TYPE_IS_NUMERIC(T)	(T == COBOL_TYPE_UNSIGNED_NUMBER || T == COBOL_TYPE_SIGNED_NUMBER_TS || \
									T == COBOL_TYPE_SIGNED_NUMBER_TC || T == COBOL_TYPE_SIGNED_NUMBER_LS || \
									T == COBOL_TYPE_SIGNED_NUMBER_LC || T == COBOL_TYPE_UNSIGNED_NUMBER_PD || \
									T == COBOL_TYPE_SIGNED_NUMBER_PD || T == COBOL_TYPE_UNSIGNED_BINARY || \
									T == COBOL_TYPE_SIGNED_BINARY || T == COBOL_TYPE_FLOAT || T == COBOL_TYPE_DOUBLE)


#define BUFFSIZE 256

char* trim_end(char*);
int strim(char* buf);
char* safe_strdup(char* s);
bool is_commit_or_rollback_statement(std::string query);
bool is_dml_statement(std::string query);
bool is_begin_transaction_statement(std::string query);
bool is_update_or_delete_statement(std::string query);
bool has_where_current_of(const std::string query, std::string& cursor_name);
void ltrim(std::string& s);

// trim from end (in place)
void rtrim(std::string& s);

// trim from both ends (in place)
void trim(std::string& s);

// trim from start (copying)
std::string ltrim_copy(std::string s);

// trim from end (copying)
std::string rtrim_copy(std::string s);

// trim from both ends (copying)
std::string trim_copy(std::string s);

bool starts_with(std::string s, std::string s1);

bool caseInsensitiveStringCompare(const std::string& str1, const std::string& str2);

std::string to_lower(const std::string s);
std::string to_upper(const std::string s);


