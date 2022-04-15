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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <algorithm> 
#include <cctype>
#include <locale>

#include "utils.h"

/*
* <Function name>
*   trim_end
*
* <Outline>
*   $B0z?t$NJ8;zNs$N8eJ}$K$"$k6uGr$r(BTRIM$B$9$k(B
*
* <Input>
*   @target: $BF~NOJ8;zNs(B
*
* <Output>
*   success: $BJQ49$5$l$?J8;zNs(B
*   failure: NULL
*/
char *
trim_end(char *target) {
	char *pos;

	if (!target) {
		return NULL;
	}

	pos = target + strlen(target) - 1;
	for (; pos > target; pos--) {
		if (*pos != ' ')
			break;
		*pos = '\0';
	}

	return target;
}

int strim(char * buf)
{
	int len = (int)strlen(buf);
	if (len == 0) return 0;
	while (len > 0) {
		if (buf[len - 1] != '\n' && buf[len - 1] != '\r' &&
			buf[len - 1] != ' ' && buf[len - 1] != '\t')
		{
			break;
		}
		buf[--len] = 0;
	}
	if (len == 0) return 0;
	if (*buf == ' ' || *buf == '\t') {
		char * p = buf;
		char * q = buf + 1;
		while (*q == ' ' || *q == '\t') {
			++q;
			--len;
		}
		while ((*p++ = *q++) != 0);
	}
	return len;
}

char *safe_strdup(char * s)
{
	return (s != NULL) ? strdup(s) : NULL;
}

// trim from start (in place)
void ltrim(std::string &s) {
	s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
		return !std::isspace(ch);
	}));
}

// trim from end (in place)
void rtrim(std::string &s) {
	s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
		return !std::isspace(ch);
	}).base(), s.end());
}

// trim from both ends (in place)
void trim(std::string &s) {
	ltrim(s);
	rtrim(s);
}

// trim from start (copying)
std::string ltrim_copy(std::string s) {
	ltrim(s);
	return s;
}

// trim from end (copying)
std::string rtrim_copy(std::string s) {
	rtrim(s);
	return s;
}

// trim from both ends (copying)
std::string trim_copy(std::string s) {
	trim(s);
	return s;
}

bool starts_with(std::string s1, std::string s2)
{
	if (s1 == s2)
		return true;

	if (s2.size() > s1.size())
		return false;

	return s1.substr(0, s2.size()) == s2;
}

bool is_commit_or_rollback_statement(std::string query)
{
	std::string q = trim_copy(query);
	q = to_upper(q);
	return (q == "COMMIT" || q == "ROLLBACK");
}

bool is_update_or_delete_statement(std::string query)
{
	std::string q = trim_copy(query);
	q = to_upper(q);
	return starts_with(q, "UPDATE") || starts_with(q, "DELETE");
}

bool has_where_current_of(const std::string query, std::string& cursor_name, int* p)
{
	std::string q = trim_copy(query);
	q = to_upper(q);
	int n = q.find("WHERE CURRENT OF");
	if (n == std::string::npos)
		return false;

	q = q.substr(n + 16); // 16 -> length of "WHERE CURRENT OF"
	trim(q);
	cursor_name = q;
	*p = n;
	return true;
}

bool is_dml_statement(std::string query)
{
	std::string q = trim_copy(query);
	q = to_upper(q);

	return(
		// ANSI
		starts_with(q, "SELECT ") ||
		starts_with(q, "INSERT ") ||
		starts_with(q, "DELETE ") ||
		starts_with(q, "UPDATE ") ||
		starts_with(q, "MERGE ") ||

		// MISC
		starts_with(q, "CALL ") ||
		starts_with(q, "BULK INSERT ")
		);
}

bool  is_select_statement(std::string query)
{
	std::string q = trim_copy(query);
	q = to_upper(q);
	return starts_with(q, "SELECT ");
}

bool  is_begin_transaction_statement(std::string query)
{
	std::string q = trim_copy(query);
	return (
		// ANSI
		caseInsensitiveStringCompare(q, "BEGIN TRANSACTION") ||
		caseInsensitiveStringCompare(q, "START TRANSACTION") ||
		caseInsensitiveStringCompare(q, "BEGIN")
		);
}

bool caseInsensitiveStringCompare(const std::string& str1, const std::string& str2) {
	if (str1.size() != str2.size()) {
		return false;
	}
	for (std::string::const_iterator c1 = str1.begin(), c2 = str2.begin(); c1 != str1.end(); ++c1, ++c2) {
		if (tolower(*c1) != tolower(*c2)) {
			return false;
		}
	}
	return true;
}

std::string to_lower(const std::string s)
{
	std::string s1 = s;
	std::transform(s1.begin(), s1.end(), s1.begin(), ::tolower);
	return s1;
}

std::string to_upper(const std::string s)
{
	std::string s1 = s;
	std::transform(s1.begin(), s1.end(), s1.begin(), ::toupper);
	return s1;
}

int find_nocase(const std::string needle, const std::string haystack)
{
	std::string sn = to_upper(needle);
	std::string sh = to_upper(haystack);

	return haystack.find(needle);
}