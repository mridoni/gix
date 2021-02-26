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
	static LIBGIXSQL_API IDbInterface *getInterface(string);
	static LIBGIXSQL_API IDbManagerInterface* getManagerInterface(int);
	static LIBGIXSQL_API IDbManagerInterface* getManagerInterface(string);
	static int removeInterface(IDbInterface *);

	static LIBGIXSQL_API vector<string> getAvailableDrivers();

private:

	static IDbInterface *load_dblib(const char *);
};

