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

#pragma once

#include <string>
#include <memory>
#include <stdexcept>
#include <vector>
#include <map>
#include <filesystem>
#include <any>
#include <algorithm>
#include <memory>

#define SIGN_LENGTH 1
#define TERMINAL_LENGTH 1
#define DECIMAL_LENGTH 1

#if (defined(_WIN32) || defined(_WIN64)) && !defined(__MINGW32__) && !defined(MEM_FREE)
#define MEM_FREE(x) {free(x); x=NULL;}
#endif

#if defined(_WIN32)
#define PATH_SEPARATOR '\\'
#else
#define PATH_SEPARATOR '/'
#endif

#ifndef _DBG_OUT
#if _DEBUG
#if defined(_WIN32)
#define _DBG_OUT(format, ...) { char bfr[2048];	sprintf(bfr, format, ##__VA_ARGS__); OutputDebugStringA(bfr); }
#else
#define _DBG_OUT(format, ...) fprintf(stderr, format, ##__VA_ARGS__)
#endif
#else
#define _DBG_OUT(format, ...)
#endif
#endif

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

std::vector<std::string> split_with_quotes(const std::string& s);
std::vector<std::string> string_split(const std::string str, const std::string regex_str);

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

std::string lpad(const std::string& s, int len);
std::string rpad(const std::string& s, int len);

std::string string_chop(const std::string &s, int len);
bool string_contains(const std::string &s1, const std::string &s2);
std::string string_replace(std::string subject, const std::string &search, const std::string &replace);

bool caseInsensitiveStringCompare(const std::string& str1, const std::string& str2);

bool starts_with(std::string s1, std::string s2);

std::vector<std::string> file_read_all_lines(const std::string& filename);
bool file_write_all_lines(const std::string& filename, const std::vector<std::string>& lines);
bool file_exists(const std::string& filename);

std::string filename_change_ext(const std::string &filename, const std::string &ext);
std::string filename_get_name(const std::string &filename);
std::string filename_absolute_path(const std::string &filename);
std::string filename_absolute_path(const std::filesystem::path &filepath);
std::string filename_clean_path(const std::string &filepath);

bool file_is_writable(const std::string &filename);

template<typename T>
auto convert(T &&t)
{
    if constexpr (std::is_same<std::remove_cv_t<std::remove_reference_t<T>>, std::string>::value) {
        return std::forward<T>(t).c_str();
    }
    else {
        return std::forward<T>(t);
    }
}

/**
 * printf like formatting for C++ with std::string
 * Original source: https://stackoverflow.com/a/26221725/11722
 */
template<typename ... Args>
std::string stringFormatInternal(const std::string &format, Args&& ... args)
{
    size_t size = snprintf(nullptr, 0, format.c_str(), std::forward<Args>(args) ...) + 1;
    if (size <= 0) { throw std::runtime_error("Error during formatting."); }
    std::unique_ptr<char[]> buf(new char[size]);
    snprintf(buf.get(), size, format.c_str(), args ...);
    return std::string(buf.get(), buf.get() + size - 1);
}

template<typename ... Args>
std::string string_format(std::string fmt, Args&& ... args)
{
    return stringFormatInternal(fmt, convert(std::forward<Args>(args))...);
}

template<typename T>
inline bool vector_contains(const std::vector<T> &v, T item)
{
    return std::find(v.begin(), v.end(), item) != v.end();
}

template <typename TK, typename TV>
bool map_contains(const std::map<TK ,TV> &v, const TK& item)
{
    return v.find(item) != v.end();
}

template <typename TK, typename TV>
std::vector<TK> map_get_keys(const std::map<TK ,TV> &vmap)
{
    std::vector<TK> vkeys;
    for (auto const &vitem : vmap)
        vkeys.push_back(vitem.first);

    return vkeys;
}

std::string vector_join(const std::vector<std::string> &v, char sep);
std::string vector_join(const std::vector<std::string> &v, std::string sep);