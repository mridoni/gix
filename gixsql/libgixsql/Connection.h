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
#include <vector>

#include "IConnection.h"
#include "IDbInterface.h"
#include "IDataSourceInfo.h"



class DbInterface;

class Connection : public IConnection
{
	friend class ConnectionManager;
	friend class IDbInterface;

public:
	LIBGIXSQL_API Connection();
	LIBGIXSQL_API ~Connection();

	LIBGIXSQL_API IDataSourceInfo *getConnectionInfo() override;
	LIBGIXSQL_API void setConnectionInfo(IDataSourceInfo *) override;
	LIBGIXSQL_API IDbInterface *getDbInterface() override;
	LIBGIXSQL_API void setDbInterface(IDbInterface *) override;

	LIBGIXSQL_API static bool test(IDataSourceInfo*);

	int getId() override;
	bool isOpen() override;
	void setName(std::string) override;

	bool getAutoCommit() override;
	void setAutoCommit(bool) override;
	
	void setOpened(bool) override;
	
	std::string getEncoding() override;
	void setEncoding(std::string) override;

	//bool addPreparedStatementData(std::string stmt_name, std::vector<std::string> params, void *private_data);
	//bool getPreparedStatementData(std::string stmt_name, std::tuple<std::vector<std::string>, void *>&);

private:

	int id;
	std::string name;
	IDataSourceInfo *conninfo;
	bool autocommit;
	bool is_opened; //open flag
	std::string encoding;

	bool ext_conninfo;

	IDbInterface *dbi;
};

