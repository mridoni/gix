#pragma once

#include <string>



class IConnectionString
{

public:

	virtual int init(std::string name, std::string user, std::string password) = 0;
	virtual std::string get() = 0;

	virtual std::string getDbType() = 0;
	virtual std::string getHost() = 0;
	virtual int getPort() = 0;
	virtual std::string getDbName() = 0;
	virtual std::string getUsername() = 0;
	virtual std::string getPassword() = 0;
	virtual std::string getDefaultSchema() = 0;

	virtual void setPassword(std::string) = 0;

	virtual std::string toConnectionString(bool use_pwd, std::string use_this_pwd = "") = 0;
	virtual std::string getName() = 0;
};

