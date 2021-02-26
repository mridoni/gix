#pragma once

#include <string>
#include <vector>
#include <vector>

#include "Connection.h"
#include "SqlVar.h"
#include "SqlVarList.h"

using namespace std;

class IConnection;

class ICursor
{

public:

	virtual void setConnection(IConnection *) = 0;
	virtual void setName(string) = 0;
	virtual void setQuery(string) = 0;
	virtual void setNumParams(int) = 0;

	virtual IConnection *getConnection() = 0;
	virtual string getName() = 0;
	virtual string getQuery() = 0;
	virtual int getNumParams() = 0;
	virtual bool isWithHold() = 0;
	virtual bool isOpen() = 0;
	virtual void setOpened(bool) = 0;

	virtual vector<string> getParameterValues() = 0;
	virtual vector<int> getParameterTypes() = 0;

	virtual void *getPrivateData() = 0;
	virtual void setPrivateData(void *) = 0;

	virtual uint64_t getRowNum() = 0;
	virtual void increaseRowNum() = 0;
};

