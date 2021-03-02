#pragma once

#include <string>
#include "Connection.h"
#include "IConnectionString.h"



class Connection;

class ConnectionManager
{
public:
	ConnectionManager();
	~ConnectionManager();

	Connection *create();
	Connection *current();
	int add(Connection *);
	void remove(Connection *);
	bool exists(std::string cname);
	int setCurrent(std::string cname);
	int setCurrent(int cid);

};

