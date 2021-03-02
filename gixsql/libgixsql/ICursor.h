#pragma once

#include <string>
#include <vector>

#include "Connection.h"
#include "SqlVar.h"
#include "SqlVarList.h"



class IConnection;

class ICursor
{

public:

	virtual void setConnection(IConnection *) = 0;
	virtual void setName(std::string) = 0;
	virtual void setQuery(std::string) = 0;
	virtual void setNumParams(int) = 0;

	virtual IConnection *getConnection() = 0;
	virtual std::string getName() = 0;
	virtual std::string getQuery() = 0;
	virtual int getNumParams() = 0;
	virtual bool isWithHold() = 0;
	virtual bool isOpen() = 0;
	virtual void setOpened(bool) = 0;

	virtual std::vector<std::string> getParameterValues() = 0;
	virtual std::vector<int> getParameterTypes() = 0;

	virtual void *getPrivateData() = 0;
	virtual void setPrivateData(void *) = 0;

	virtual uint64_t getRowNum() = 0;
	virtual void increaseRowNum() = 0;
};

