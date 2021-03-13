/*
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
#include <memory>
#include <stdexcept>

#include "gixcommon_global.h"


#define SIGN_LENGTH 1
#define TERMINAL_LENGTH 1
#define DECIMAL_LENGTH 1

#if (defined(_WIN32) || defined(_WIN64)) && !defined(__MINGW32__)
#define MEM_FREE(x) {free(x); x=NULL;}
#endif

#define BUFFSIZE 256

extern char type_tc_negative_final_number[];

void insert_decimal_point(char *, int, int);
int type_tc_is_positive(char *);
char *ocdb_getenv(char *, char *);
char *uint_to_str(int);
char *oc_strndup(char *, int);
char *trim_end(char *);
int strim(char * buf);
char *safe_strdup(char * s);
bool is_commit_or_rollback_statement(std::string query);
bool is_dml_statement(std::string query);
bool is_begin_transaction_statement(std::string query);

void ltrim(std::string &s);

// trim from end (in place)
void rtrim(std::string &s);

// trim from both ends (in place)
void trim(std::string &s);

// trim from start (copying)
std::string ltrim_copy(std::string s);

// trim from end (copying)
std::string rtrim_copy(std::string s);

// trim from both ends (copying)
std::string trim_copy(std::string s);

bool caseInsensitiveStringCompare(const std::string& str1, const std::string& str2);

bool GIXCOMMON_EXPORT starts_with(std::string s1, std::string s2);

template<typename ... Args>
std::string std_string_format(const std::string &format, Args ... args)
{
    int size = snprintf(nullptr, 0, format.c_str(), args ...) + 1; // Extra space for '\0'
    if (size <= 0) { throw std::runtime_error("Error during formatting."); }
    std::unique_ptr<char[]> buf(new char[size]);
    snprintf(buf.get(), size, format.c_str(), args ...);
    return std::string(buf.get(), buf.get() + size - 1); // We don't want the '\0' inside
}

#endif
