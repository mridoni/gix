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

#ifndef OCDBUTIL_H
#define OCDBUTIL_H

#include <string>

#define SIGN_LENGTH 1
#define TERMINAL_LENGTH 1
#define DECIMAL_LENGTH 1

#define BUFFSIZE 256

char* trim_end(char*);
int strim(char* buf);
char* safe_strdup(char* s);
bool is_commit_or_rollback_statement(std::string query);
bool is_dml_statement(std::string query);
bool is_begin_transaction_statement(std::string query);
bool is_update_or_delete_statement(std::string query);
bool has_where_current_of(const std::string query, std::string& cursor_name);
inline void ltrim(std::string& s);

// trim from end (in place)
inline void rtrim(std::string& s);

// trim from both ends (in place)
inline void trim(std::string& s);

// trim from start (copying)
inline std::string ltrim_copy(std::string s);

// trim from end (copying)
inline std::string rtrim_copy(std::string s);

// trim from both ends (copying)
inline std::string trim_copy(std::string s);

inline bool starts_with(std::string s, std::string s1);

bool caseInsensitiveStringCompare(const std::string& str1, const std::string& str2);

std::string to_lower(const std::string s);
std::string to_upper(const std::string s);

#endif
