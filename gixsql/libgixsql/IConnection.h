#pragma once

#include <string>

#include "IDbInterface.h"
#include "IConnectionString.h"



class IConnectionString;
class IDbInterface;

class IConnection
{

public:

	virtual int getId() = 0;
	virtual bool isOpen() = 0;
	virtual void setName(std::string) = 0;
	virtual void setConnectionInfo(IConnectionString *) = 0;
	virtual void setAutoCommit(bool) = 0;
	virtual void setOpened(bool) = 0;
	virtual void setEncoding(std::string) = 0;
	virtual void setDbInterface(IDbInterface *) = 0;
	virtual IConnectionString *getConnectionInfo() = 0;
	virtual std::string getEncoding() = 0;
	virtual IDbInterface *getDbInterface() = 0;
	virtual bool getAutoCommit() = 0;

};


