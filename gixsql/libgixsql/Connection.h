#pragma once

#ifndef LIBGIXSQL_API
#if defined(_WIN32) || defined(_WIN64)
#define LIBGIXSQL_API __declspec(dllexport)   
#else  
#define LIBGIXSQL_API
#endif  
#endif

#include <string>

#include "IConnection.h"
#include "IDbInterface.h"
#include "IConnectionString.h"

using namespace std;

class DbInterface;

class Connection : public IConnection
{
	friend class ConnectionManager;
	friend class IDbInterface;

public:
	LIBGIXSQL_API Connection();
	LIBGIXSQL_API ~Connection();

	LIBGIXSQL_API IConnectionString *getConnectionInfo() override;
	LIBGIXSQL_API void setConnectionInfo(IConnectionString *) override;
	LIBGIXSQL_API IDbInterface *getDbInterface() override;
	LIBGIXSQL_API void setDbInterface(IDbInterface *) override;

	LIBGIXSQL_API static bool test(IConnectionString*);

	int getId() override;
	bool isOpen() override;
	void setName(string) override;

	bool getAutoCommit() override;
	void setAutoCommit(bool) override;
	
	void setOpened(bool) override;
	
	string getEncoding() override;
	void setEncoding(string) override;

private:

	int id;
	string cname;
	IConnectionString *conninfo;
	bool autocommit;
	bool is_opened; //open flag
	string encoding;

	bool ext_conninfo;

	IDbInterface *dbi;
};

