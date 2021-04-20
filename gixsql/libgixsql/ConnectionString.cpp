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

#include "ConnectionString.h"

#include <string>
#include <sstream>
#include <stdlib.h>
#include <stdio.h>
#include <regex>

ConnectionString::ConnectionString()
{
	conn_string.clear();
	port = 0;
}

ConnectionString::~ConnectionString()
{
}

int ConnectionString::init(std::string name, std::string user, std::string password)
{
	int rc = 1;

	std::string real_dbname, host, s_port;
	int port = 0;

	// dbname -> host:port/dbname
	if (!name.empty()) {
		size_t tmpstr;

		tmpstr = name.find('/');
		if (tmpstr != std::string::npos) {
			real_dbname = name.substr(tmpstr + 1);
			host = name.substr(0, tmpstr);
		}
		else {
			host = name;
			real_dbname = ".";
		}

		tmpstr = host.find(':');
		if (tmpstr != std::string::npos) {
			port = atoi(host.substr(tmpstr + 1).c_str());
			s_port = host.substr(tmpstr + 1);
			host = host.substr(0, tmpstr);
		}
	}

	if (!real_dbname.empty() && !host.empty()) {
		int bfrlen = real_dbname.length() + host.length() + s_port.length() + user.length() + password.length() + 6;
		char * bfr = (char *) calloc(bfrlen, 1);
		sprintf(bfr, "%s/%s@%s:%d/%s", user.c_str(), password.c_str(), host.c_str(), port, real_dbname.c_str());
		conn_string = bfr;
		free(bfr);

		return parse();
	}

	return 1;
}

std::string ConnectionString::get()
{
	return conn_string;
}

LIBGIXSQL_API std::string ConnectionString::getDbType()
{
	return dbtype;
}

std::string ConnectionString::getHost()
{
	return host;
}

int ConnectionString::getPort()
{
	return port;
}

std::string ConnectionString::getDbName()
{
	return dbname;
}

std::string ConnectionString::getUsername()
{
	return username;
}

std::string ConnectionString::getPassword()
{
	return password;
}

std::string ConnectionString::getDefaultSchema()
{
	return default_schema;
}

LIBGIXSQL_API std::string ConnectionString::getName()
{
	std::string conn_name = this->getDbType() + "://" + this->getUsername();

	if (this->getDbType() == "odbc")
		conn_name += ("@" + this->getHost());
	else
		conn_name += ("@" + this->getHost() + "/" + this->getDbName());

	return conn_name;
}

LIBGIXSQL_API void ConnectionString::setPassword(std::string pwd)
{
	this->password = pwd;
}

LIBGIXSQL_API ConnectionString* ConnectionString::parseEx(std::string cs)
{
	// Format: type://user/password@host[:port]/database?default_schema
	// e.g. pgsql://user:password@localhost:5432/postgres?public

	//regex rxConnString("([A-Za-z0-9_]+)\\:\\/\\/([a-zA-Z0-9_\\-]+)\\:([a-zA-Z0-9_\\-]+)@([a-zA-Z0-9_\\-\\.]+)\\:([0-9]+)\\/([a-zA-Z0-9_\\-]+)\\?([a-zA-Z0-9_\\-]+)");
	//regex rxConnString("([A-Za-z0-9_]+)\\:\\/\\/([a-zA-Z0-9_\\-]+)\\:([a-zA-Z0-9_\\-]+)@([a-zA-Z0-9_\\-\\.]+)(\\:([0-9]+))?(\\/([a-zA-Z0-9_\\-]+))?(\\?([a-zA-Z0-9_\\-]+))?");
	std::regex rxConnString(R"(([A-Za-z0-9_]+)\:\/\/([a-zA-Z0-9_\-]+)\:([a-zA-Z0-9_\-]+)?@([a-zA-Z0-9_\-\.]+)(\:([0-9]+))?(\/([a-zA-Z0-9_\-]+))?(\?([a-zA-Z0-9_\-]+))?)");
	std::smatch cm;
	if (!regex_match(cs, cm, rxConnString, std::regex_constants::match_default))
		return nullptr;

	ConnectionString* ccs = new ConnectionString();
	ccs->conn_string = cs;
	ccs->dbtype = cm[1].str();
	ccs->username = cm[2].str();
	ccs->password = cm[3].str();
	ccs->host = cm[4].str();
	ccs->port = atoi(cm[6].str().c_str());
	ccs->dbname = cm[8].str();
	ccs->default_schema = cm[10].str();

	return ccs;
}

std::string ConnectionString::toConnectionString(bool use_pwd, std::string pwd)
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
	
		if (!default_schema.empty())
			res << "?" << default_schema;
	}

	return res.str();
}

int ConnectionString::init(const std::string& c)
{
	conn_string = c;
	return parse();
}

int ConnectionString::parse()
{
	std::string auth_portion, target_portion;

	size_t t = conn_string.find('@');

	if (t == std::string::npos)
		return 1;

	auth_portion = conn_string.substr(0, t);
	target_portion = conn_string.substr(t + 1);

	// parse auth_portion
	t = auth_portion.find('/');
	if (t == std::string::npos) {
		return 1;
	}

	username = auth_portion.substr(0, t);
	password = auth_portion.substr(t + 1);

	// parse target_portion
	t = target_portion.find('/');
	if (t != std::string::npos) {
		dbname = target_portion.substr(t + 1);
		t = dbname.find("?");
		if (t != std::string::npos) {
			default_schema = dbname.substr(t + 1);
			dbname = dbname.substr(0, t);
		}
	}

	t = target_portion.find(':');
	if (t != std::string::npos) {
		port = atoi(target_portion.substr(t + 1).c_str());
		host = target_portion.substr(0, t);
	}
	else {
		host = target_portion;
	}


	if (host.length() > 0 && username.length() > 0 && password.length() > 0)
		return 0;

	return 1;
}
