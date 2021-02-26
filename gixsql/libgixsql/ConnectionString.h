#pragma once

#ifndef LIBGIXSQL_API
#if defined(_WIN32) || defined(_WIN64)
#define LIBGIXSQL_API __declspec(dllexport)   
#else  
#define LIBGIXSQL_API
#endif  
#endif

#include <string>
#include "IConnectionString.h"

using namespace std;

class ConnectionString : public IConnectionString
{
public:
	LIBGIXSQL_API ConnectionString();
	LIBGIXSQL_API ~ConnectionString();
	LIBGIXSQL_API int init(const string& c);
	LIBGIXSQL_API string get() override;

	int init(string name, string user, string password) override;

	LIBGIXSQL_API string getDbType() override;
	LIBGIXSQL_API string getHost() override;
	LIBGIXSQL_API int getPort() override;
	LIBGIXSQL_API string getDbName() override;
	LIBGIXSQL_API string getUsername() override;
	LIBGIXSQL_API string getPassword() override;
	LIBGIXSQL_API string getDefaultSchema() override;

	LIBGIXSQL_API static ConnectionString * parseEx(const string& cs);

	LIBGIXSQL_API string toConnectionString(bool use_pwd, string pwd = "") override;
	LIBGIXSQL_API string getName() override;
	LIBGIXSQL_API virtual void setPassword(string) override;

private:
	string dbtype;
	string conn_string;
	string host;
	int port; 
	string dbname;
	string username;
	string password;
	string default_schema;
	int parse();

};

