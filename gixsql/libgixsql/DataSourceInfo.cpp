/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
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

#include "DataSourceInfo.h"
#include "utils.h"

#include <string>
#include <sstream>
#include <stdlib.h>
#include <stdio.h>
#include <regex>

DataSourceInfo::DataSourceInfo()
{
	conn_string.clear();
	port = 0;
}

DataSourceInfo::~DataSourceInfo()
{
}

std::string rx_escape(const std::string s)
{
	if (s.size() == 1) {
		char c = s[0];
		if (c == '[' || c == ']' || c == '(' || c == ')' || c == '{' || c == '}' || c == '*' || c == '+' || c == '?' || c == '|' || c == '^' || c == '$' || c == '.' || c == '\\') {
			return "\\" + s;
		}
	}
	return s;
}

/*
(?:((?:gixsql|mysql|pgsql|odbc)\:))(\/\/[A-Za-z0-9\-_]+)(:[0-9]+)?(\/[A-Za-z0-9\-_]+)?
*/
int DataSourceInfo::init(const std::string &data_source, const std::string &username, const std::string &password)
{
	// Format: type://user.password@host[:port][/database][?default_schema=schema&par1=val&par2=val]
	// e.g. pgsql://user.password@localhost:5432/postgres?default_schema=public&par1=val1&par2=val2

	/*
	(?:((?:gixsql|mysql|pgsql|odbc)\:))(\/\/(?:(([^:]+)\.([^:]+)@)?)[A-Za-z0-9\-_]+)(:[0-9]+)?(\/[A-Za-z0-9\-_]+)?
	*/

	std::string connstring_rx_text = R"((?:((?:gixsql|mysql|pgsql|odbc)\:))(\/\/(?:(([^:]+)##GIXSQL_USRPWD_SEP##([^:]+)@)?)[A-Za-z0-9\-_\.]+)(:[0-9]+)?(\/[A-Za-z0-9\-_]+)?)";
	std::string gixsql_usrpwd_sep = getenv("GIXSQL_USRPWD_SEP") ? getenv("GIXSQL_USRPWD_SEP") : "";
	if (gixsql_usrpwd_sep.empty() || gixsql_usrpwd_sep.size() != 1) {
		gixsql_usrpwd_sep = ".";
	}

	auto sss = rx_escape(gixsql_usrpwd_sep);
	connstring_rx_text = string_replace(connstring_rx_text, "##GIXSQL_USRPWD_SEP##", rx_escape(gixsql_usrpwd_sep));

	std::regex rxConnString(connstring_rx_text);
	std::smatch cm;
	if (!regex_match(data_source, cm, rxConnString, std::regex_constants::match_default))
		return 1;

	this->conn_string = data_source;

	for (int i = 1; i < cm.size(); i++) {
		std::string m = cm[i];

		if (ends_with(m, ":")) {
			this->dbtype = m.substr(0, m.size() - 1);
			continue;
		}

		if (starts_with(m, "//")) {
			int p_at = m.find("@");
			if (p_at == std::string::npos)
				this->host = m.substr(2);
			else
				if (p_at < (m.size() - 1))
					this->host = m.substr(p_at + 1);
			continue;
		}

		if (starts_with(m, "/")) {
			this->dbname = m.substr(1);
			continue;
		}

		if (starts_with(m, ":")) {
			this->port = atoi(m.substr(1).c_str());
			continue;
		}

		if (ends_with(m, "@")) {
			std::string up = m.substr(0, m.size() - 1);
			int p = up.find(gixsql_usrpwd_sep);
			if (p != std::string::npos) {
				this->username = up.substr(0, p);
				if (p < (up.size() - 1)) {
					this->password = up.substr(p + 1);
				}
			}
			else {
				this->username = up;
			}
			continue;
		}

	}

	if (!username.empty() && !password.empty()) {
		this->username = username;
		this->password = password;
	}
	else {
		if (!username.empty() && password.empty()) {
			std::string up = username;
			int p = up.find(gixsql_usrpwd_sep);
			if (p != std::string::npos) {
				this->username = up.substr(0, p);
				if (p < (up.size() - 1)) {
					this->password = up.substr(p + 1);
				}
			}
		}
	}

	int p = data_source.find("?");
	if (p != std::string::npos && p < (data_source.size() - 1)) {
		std::string ps = data_source.substr(p + 1);
		auto params = string_split(ps, "&");
		for (auto pp : params) {
			auto ppi = string_split(pp, "=");
			if (ppi.size() == 2 && !ppi[0].empty() && !ppi[1].empty()) {
				this->options[ppi[0]] = ppi[1];
			}
		}
	}

	return 0;
}

std::string DataSourceInfo::get()
{
	return conn_string;
}

LIBGIXSQL_API std::string DataSourceInfo::getDbType()
{
	return dbtype;
}

std::string DataSourceInfo::getHost()
{
	return host;
}

int DataSourceInfo::getPort()
{
	return port;
}

std::string DataSourceInfo::getDbName()
{
	return dbname;
}

std::string DataSourceInfo::getUsername()
{
	return username;
}

std::string DataSourceInfo::getPassword()
{
	return password;
}

//std::string DataSourceInfo::getDefaultSchema()
//{
//	return options.find("default_schema") != options.end() ? options["default_schema"] : std::string();
//}

LIBGIXSQL_API std::string DataSourceInfo::getName()
{
	std::string conn_name = this->getDbType() + "://" + this->getUsername();

	if (this->getDbType() == "odbc")
		conn_name += ("@" + this->getHost());
	else
		conn_name += ("@" + this->getHost() + "/" + this->getDbName());

	return conn_name;
}

LIBGIXSQL_API void DataSourceInfo::setPassword(std::string pwd)
{
	this->password = pwd;
}


std::string DataSourceInfo::dump(bool with_password)
{
#ifdef _DEBUG
	std::string s;

	s += "conn_string: " + conn_string + "\n";
	s += "dbtype     : " + dbtype + "\n";
	s += "host       : " + host + "\n";
	s += "port       : " + std::to_string(port) + "\n";
	s += "dbname     : " + dbname + "\n";
	s += "username   : " + username + "\n";
	s += "password   : " + password + "\n";

	int i = 0;
	for (auto it = options.begin(); it != options.end(); ++it) {
		s += "option     " + std::to_string(i++) + ": " + it->first + " => " + it->second + "\n";
	}

	return s;
#endif
}

LIBGIXSQL_API const std::map<std::string, std::string> &DataSourceInfo::getOptions()
{
	return options;
}


std::string DataSourceInfo::toConnectionString(bool use_pwd, std::string pwd)
{
	std::ostringstream res;
	res << dbtype << "://" << username << ":";
	if (use_pwd)
		res << ((!pwd.empty()) ? pwd : password);

	res << "@" << host;
	
	if (port > 0)
		res << ":" << std::to_string(port);

	if (!dbname.empty()) {
		res << "/" << dbname;
	
	}

	return res.str();
}
