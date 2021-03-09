/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 2.1,
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

#define DB_PGSQL 			1
#define DB_ODBC  			2
#define DB_ORACLE 			3
#define DB_MYSQL			4
#define DB_MSSQL			5
#define DB_DB2  			6
#define DB_SET_RUNTIME		-1

#if defined(_WIN32) || defined(_WIN64)

#if defined(__MINGW32__)
#undef byte
typedef unsigned char byte;
#endif

#include <windows.h>
#define LIBHANDLE HINSTANCE
#else
#include <dlfcn.h>
#define LIBHANDLE void *
#endif

#ifndef LIBGIXSQL_API
#if defined(_WIN32) || defined(_WIN64)
#define LIBGIXSQL_API __declspec(dllexport)   
#else  
#define LIBGIXSQL_API
#endif  
#endif

#include <map>
#include "IDbInterface.h"
#include "IDbManagerInterface.h"

class DbInterfaceFactory
{
public:

	static LIBGIXSQL_API IDbInterface *getInterface(int);
	static LIBGIXSQL_API IDbInterface *getInterface(std::string);
	static LIBGIXSQL_API IDbManagerInterface* getManagerInterface(int);
	static LIBGIXSQL_API IDbManagerInterface* getManagerInterface(std::string);
	static int removeInterface(IDbInterface *);

	static LIBGIXSQL_API std::vector<std::string> getAvailableDrivers();

private:

	static IDbInterface *load_dblib(const char *);
};

