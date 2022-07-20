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
#include <string>
#include <stdbool.h>
#include <malloc.h>
#include <math.h>
#include <algorithm> 
#include <cctype>
#include <locale>
#include <cstring>
#include <regex>

#include "utils.h"
#include "Logger.h"

char type_tc_negative_final_number[] =
{
	'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y'
	//     '{', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R'
};
static int type_tc_negative_final_number_len =
sizeof(type_tc_negative_final_number) / sizeof(type_tc_negative_final_number[0]);

/*
* <Function name>
*   insert_decimal_point
*
* <Outline>
*   power$B$G;XDj$7$?0LCV$K>.?tE@$rA^F~$9$k(B
*
* <Input>
*   @data: $BA^F~BP>](B
*   @data_size: data$B$K3d$jEv$F$i$l$?%5%$%:(B($B%P%$%HC10L(B)
*   @power: $B>.?tE@0J2<$N7e?t(B($BIi$NCM(B)
*/
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

/*
* <Function name>
*   type_tc_is_positive
*
* <Outline>
*   OCDB_TYPE_SIGNED_NUMBER_TC$B$N%G!<%?$,@5Ii$G$"$k$+$rH=JL$7!"(B
*   $BIi$NCM$N>l9g$OId9f$r<h$j=|$$$??tCM$G0z?t$r>e=q$-$9$k(B
*   $B$b$73:Ev$9$k?tCM$,B8:_$7$J$$>l9g$O!"(B0$B$r%;%C%H$7$?>e$G(Btrue$B$rJV$9(B
*
* <Input>
*   @lastchar: $BH=JLBP>]$NJ8;z(B
*
* <Output>
*   $BH=JLBP>]$,@5(B : true
*   $BH=JLBP>]$,Ii(B : false
*
*/
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

	//LOG_DEBUG(__FILE__, __func__, "no final_number found: %c\n", *lastchar);
	*lastchar = 0;
	return true;
}

///*
//* <Function name>
//*   ocdb_getenv
//*
//* <Outline>
//*   $B4D6-JQ?t$+$iCM$r<hF@$9$k!#$J$$>l9g$O%(%i!<%m%0$r;D$7$?>e$G(BNULL$B$rJV$9(B
//*
//* <Input>
//*   @param: $B%Q%i%a!<%?L>(B
//*   @def  : default value
//*
//* <Output>
//*   success: $B%Q%i%a!<%?$NCM(B
//*   failure: default value
//*/
//char *ocdb_getenv(char *param, char *def) {
//	char *env;
//
//	if (param == NULL) {
//		LOG_ERROR("parameter is NULL\n");
//		return def;
//	}
//
//	env = getenv(param);
//	if (env == NULL) {
//		LOG_DEBUG(__FILE__, __func__, "param '%s' is not set. set default value. \n", param);
//		return def;
//	}
//	else {
//		LOG_DEBUG(__FILE__, __func__, "param '%s' is %s. \n", param, env);
//	}
//	return env;
//}


/*
* <Function name>
*   uint_to_str
*
* <Outline>
*   $B0z?t$H$7$FM?$($i$l$??tCM$+$iJ8;zNs$r@8@.$7$FJV$9(B
*
* <Input>
*   @i: $B?tCM(B
*
* <Output>
*   success: $BJQ49$5$l$?J8;zNs(B
*   failure: NULL
*/
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

/*
* <Function name>
*   oc_strndup
*
* <Outline>
*   $B0z?t$NJ8;zNs$+$i;XDjJ8;z?tJ,$rJ#@=$7$FJV$9(B
*
* <Input>
*   @src: $BF~NOJ8;zNs(B
*   @n: $BJ8;z?t(B
*
* <Output>
*   success: $BJ#@=$5$l$?J8;zNs(B
*   failure: NULL
*/
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

bool starts_with(const std::string& s1, const std::string& s2)
{
	return s1.substr(0, s2.length()) == s2;
}

bool ends_with(std::string const &s1, std::string const &s2)
{
	if (s1.length() >= s2.length()) {
		return (0 == s1.compare(s1.length() - s2.length(), s2.length(), s2));
	}
	else {
		return false;
	}
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

std::string string_replace(std::string subject, const std::string &search, const std::string &replace)
{
	size_t pos = 0;
	while ((pos = subject.find(search, pos)) != std::string::npos) {
		subject.replace(pos, search.length(), replace);
		pos += replace.length();
	}
	return subject;
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