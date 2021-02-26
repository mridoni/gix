#pragma once

#include <string>

using namespace std;

class IConnectionString
{

public:

	virtual int init(string name, string user, string password) = 0;
	virtual string get() = 0;

	virtual string getDbType() = 0;
	virtual string getHost() = 0;
	virtual int getPort() = 0;
	virtual string getDbName() = 0;
	virtual string getUsername() = 0;
	virtual string getPassword() = 0;
	virtual string getDefaultSchema() = 0;

	virtual void setPassword(string) = 0;

	virtual string toConnectionString(bool use_pwd, string use_this_pwd = "") = 0;
	virtual string getName() = 0;
};

