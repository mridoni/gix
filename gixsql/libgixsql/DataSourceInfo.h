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

#pragma once

#ifndef LIBGIXSQL_API
#if defined(_WIN32) || defined(_WIN64)
#define LIBGIXSQL_API __declspec(dllexport)   
#else  
#define LIBGIXSQL_API
#endif  
#endif

#include <string>
#include <map>
#include "IDataSourceInfo.h"



class DataSourceInfo : public IDataSourceInfo
{
public:
	LIBGIXSQL_API DataSourceInfo();
	LIBGIXSQL_API ~DataSourceInfo();
	LIBGIXSQL_API std::string get() override;

	int init(const std::string &data_source, const std::string &username, const std::string &password) override;

	LIBGIXSQL_API std::string getDbType() override;
	LIBGIXSQL_API std::string getHost() override;
	LIBGIXSQL_API int getPort() override;
	LIBGIXSQL_API std::string getDbName() override;
	LIBGIXSQL_API std::string getUsername() override;
	LIBGIXSQL_API std::string getPassword() override;

	LIBGIXSQL_API std::string toConnectionString(bool use_pwd, std::string pwd = "") override;

	LIBGIXSQL_API std::string getName() override;
	LIBGIXSQL_API virtual void setPassword(std::string) override;

	LIBGIXSQL_API const std::map<std::string, std::string>& getOptions() override;

	std::string dump(bool with_password = false);



private:
	std::string dbtype;
	std::string conn_string;
	std::string host;
	int port = 0; 
	std::string dbname;
	std::string username;
	std::string password;
	std::map<std::string, std::string> options;

	static DataSourceInfo *parse(const std::string& data_source);
};

