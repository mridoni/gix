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

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <stdbool.h>
#include <math.h>
#include <algorithm> 
#include <cctype>
#include <locale>
#include <cstring>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <filesystem>
#include <fstream>
#include <algorithm>
#include <regex>

#include "libcpputils.h"

char type_tc_negative_final_number[] =
{
	'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y'
	//     '{', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R'
};
static int type_tc_negative_final_number_len =
sizeof(type_tc_negative_final_number) / sizeof(type_tc_negative_final_number[0]);

void insert_decimal_point(char *data, int data_size, int power) {
	int before_length, after_length;
	before_length = (int)strlen(data);
	after_length = (int)strlen(data) + 1;

	int n_decimal_places = -power;

	// check size of data
	if (data_size < after_length) {
		return;
	}
	else if (n_decimal_places <= 0 || n_decimal_places >= before_length) {
		return;
	}

	memmove(data + (after_length - n_decimal_places), data + (before_length - n_decimal_places),
		n_decimal_places * sizeof(char));
	data[before_length - n_decimal_places] = '.';
}

int type_tc_is_positive(char *lastchar) {
	int i;

	if (*lastchar >= '0' &&  *lastchar <= '9')
		return true;

	for (i = 0; i<type_tc_negative_final_number_len; i++) {
		if (*lastchar == type_tc_negative_final_number[i]) {
			char tmp[2];
			sprintf(tmp, "%d", i);
			*lastchar = tmp[0];
			return false;
		}
	}

	*lastchar = 0;
	return true;
}

char *ocdb_getenv(char *param, char *def) {
	char *env;
	if (param == NULL) {
		return def;
	}

	env = getenv(param);
	if (env == NULL) {
		return def;
	}
	else {

	}
	return env;
}


char *
uint_to_str(int i) {
	int tmp = i;
	int dig = 0;
	char *ret;

	if (i < 0) return NULL;
	do {
		dig++;
		tmp = tmp / 10;
	} while (tmp > 0);

	if ((ret = (char *)calloc(dig + TERMINAL_LENGTH, sizeof(char))) == NULL) {
		return NULL;
	}
	sprintf(ret, "%d", i);
	return ret;
}

char *
oc_strndup(char *src, int n) {
	char *ret;

	if (n < 0) {
		return NULL;
	}
	ret = (char *)malloc(sizeof(char) * (n + 1));
	if (!src) {
		return NULL;
	}

	memcpy(ret, src, n);
	ret[n] = '\0';

	return ret;
}

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
		return !std::isspace((unsigned char)ch);
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

bool starts_with(const std::string& s1, const std::string& s2)
{
	return s1.substr(0, s2.length()) == s2;
}

bool ends_with(std::string const& s1, std::string const& s2)
{
	if (s1.length() >= s2.length()) {
		return (0 == s1.compare(s1.length() - s2.length(), s2.length(), s2));
	}
	else {
		return false;
	}
}



std::string lpad(const std::string &s, int len)
{
	std::string s1 = s;
	if (s1.size() >= len)
		return s1;

	for (int i = 0; i < len - s1.size(); i++)
		s1 = ' ' + s1;

	return s1;
}

std::string rpad(const std::string &s, int len)
{
	std::string s1 = s;
	int ll = len - s1.size();
	if (s1.size() >= len)
		return s1;

	for (int i = 0; i < ll; i++)
		s1 = s1 + ' ';

	return s1;
}

std::string string_chop(const std::string &s, int len)
{
	std::string s1 = s;
	if (s1.size() < len)
		len = s1.size();

	int new_len = s1.size() - len;
	return s1.substr(0, new_len);
}

bool string_contains(const std::string &s1, const std::string &s2, bool case_insensitive)
{
    if (!case_insensitive)
        return s1.find(s2) != std::string::npos;
    else {
        auto it = std::search(
          s1.begin(), s1.end(),
          s2.begin(),   s2.end(),
          [](char ch1, char ch2) { return std::toupper(ch1) == std::toupper(ch2); }
        );
        return (it != s1.end() );        
    }
}

std::string string_replace(std::string subject, const std::string &search, const std::string &replace)
{
	size_t pos = 0;
	while ((pos = subject.find(search, pos)) != std::string::npos) {
		subject.replace(pos, search.length(), replace);
		pos += replace.length();
	}
	return subject;
}

std::string string_replace_regex(std::string subject, const std::string &search_rx, const std::string &replace_rx, bool case_insensitive)
{
    std::regex::flag_type f = std::regex_constants::ECMAScript;
    if (case_insensitive) {
        f |=std::regex_constants::icase;
    }
    
	std::regex reg(search_rx, f);
	return regex_replace(subject, reg, replace_rx);
}

std::vector<std::string> file_read_all_lines(const std::string &filename)
{
	std::vector<std::string> res;

	std::filesystem::path filepath(filename);

	if (!std::filesystem::exists(filepath)) {
		return res;
	}

	std::ifstream ifs(filepath);
	
	std::string line;
	while (std::getline(ifs, line)) {
		res.push_back(line);
	}

	ifs.close();

	return res;
}

bool file_write_all_lines(const std::string &filename, const std::vector<std::string> &lines)
{
	std::filesystem::path filepath(filename);

	std::ofstream ofs(filepath);

	for (std::string line : lines) {
		ofs << line << std::endl;
	}

	ofs.close();

	return true;
}

bool file_exists(const std::string &filename)
{
	std::filesystem::path fp(filename);
	return std::filesystem::exists(fp);
}

std::string filename_change_ext(const std::string &filename, const std::string &ext)
{
	std::filesystem::path fp(filename);
	return fp.replace_extension(ext).string();
}

std::string filename_get_name(const std::string &filename)
{
	std::filesystem::path fp(filename);
	return fp.filename().string();
}

std::string filename_get_dir(const std::string& filename)
{
	std::filesystem::path fp(filename);
	return fp.parent_path().string();
}

std::string filename_absolute_path(const std::filesystem::path &filepath)
{
	return std::filesystem::absolute(filepath).string();
}

std::string filename_clean_path(const std::string &filepath)
{
	std::string s = std::filesystem::canonical(filepath).string();
	std::replace(s.begin(), s.end(), '\\', '/');
	return s;
}

std::string filename_absolute_path(const std::string &filename)
{
	std::filesystem::path fp(filename);
	return filename_absolute_path(fp);
}

bool is_commit_or_rollback_statement(std::string query)
{
	std::string q = trim_copy(query);
	return (q == "COMMIT"  || q == "ROLLBACK");
}

bool is_dml_statement(std::string query)
{
	std::string q = trim_copy(query);

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

std::vector<std::string> split_with_quotes(const std::string& in_s)
{
    std::istringstream iss(in_s);
    std::vector<std::string> v;
    std::string s;

    while (iss >> std::quoted(s)) {
        v.push_back(s);
    }
    return v;
}

std::vector<std::string> string_split(const std::string str, const std::string regex_str)
{   
	std::regex regexz(regex_str);
	std::sregex_token_iterator token_iter(str.begin(), str.end(), regexz, -1);
	std::sregex_token_iterator end;
	std::vector<std::string> list;
	while (token_iter != end) {
		list.emplace_back(*token_iter++);
	}
	return list;
}


bool file_is_writable(const std::string &filename)
{
	std::string dir_path = std::filesystem::path(filename).parent_path().string();
	auto fs = std::filesystem::status(filename);
	auto p = fs.permissions();
	return ((p & std::filesystem::perms::owner_write) != std::filesystem::perms::none) ||
		((p & std::filesystem::perms::group_write) != std::filesystem::perms::none);
}

std::string vector_join(const std::vector<std::string> &v, char sep)
{
	std::string s;

	for (std::vector< std::string>::const_iterator p = v.begin();
		p != v.end(); ++p) {
		s += *p;
		if (p != v.end() - 1)
			s += sep;
	}
	return s;
}

std::string vector_join(const std::vector<std::string> &v, std::string sep)
{
	std::string s;

	for (std::vector< std::string>::const_iterator p = v.begin();
		p != v.end(); ++p) {
		s += *p;
		if (p != v.end() - 1)
			s += sep;
	}
	return s;
}

bool split_in_args(std::vector<std::string>& qargs, std::string command, bool remove_empty)
{
	int len = command.length();
	bool qot = false, sqot = false;
	int arglen;
	for (int i = 0; i < len; i++) {
		int start = i;
		if (command[i] == '\"') {
			qot = true;
		}
		else if (command[i] == '\'') sqot = true;

		if (qot) {
			i++;
			start++;
			while (i < len && command[i] != '\"')
				i++;
			if (i < len)
				qot = false;
			arglen = i - start;
			i++;
		}
		else if (sqot) {
			i++;
			while (i < len && command[i] != '\'')
				i++;
			if (i < len)
				sqot = false;
			arglen = i - start;
			i++;
		}
		else {
			while (i < len && command[i] != ' ')
				i++;
			arglen = i - start;
		}

		std::string a = command.substr(start, arglen);
		if (!remove_empty || !a.empty())
			qargs.push_back(a);
	}

	return (qot == sqot);
}


std::string unquote(const std::string &s)
{
	std::string res = trim_copy(s);
	if (res.size() < 2)
		return res;

	if (res[0] == '"' || res[0] == '\'')
		res = res.substr(1);

	if (res.back() == '"' || res.back() == '\'')
		res = res.substr(0, res.size() - 1);

	return res;
}

std::string to_lower(const std::string& s)
{
	std::string s1 = s;
	std::transform(s1.begin(), s1.end(), s1.begin(), ::tolower);
	return s1;
}

std::string to_upper(const std::string& s)
{
	std::string s1 = s;
	std::transform(s1.begin(), s1.end(), s1.begin(), ::toupper);
	return s1;
}
