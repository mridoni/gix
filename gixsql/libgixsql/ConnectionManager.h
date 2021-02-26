#pragma once

#include <string>
#include "Connection.h"
#include "IConnectionString.h"

using namespace std;

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
	bool exists(string cname);
	int setCurrent(string cname);
	int setCurrent(int cid);

};

