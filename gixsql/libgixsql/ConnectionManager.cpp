
#include <string>
#include <vector>
#include <map>
#include <algorithm>

#include "ConnectionManager.h"
#include "DbInterfaceFactory.h"



static std::vector<Connection *> _connections;
static std::map<int, Connection *> _connection_map;
static std::map<std::string, Connection *> _connection_name_map;
static Connection *current_connection;

static int next_conn_id = 1;

ConnectionManager::ConnectionManager()
{
}


ConnectionManager::~ConnectionManager()
{
}

Connection * ConnectionManager::create()
{
	return new Connection();
}

Connection *ConnectionManager::current()
{
	return current_connection;
}

int ConnectionManager::add(Connection *conn)
{
	_connections.push_back(conn);
	current_connection = conn;
	conn->id = next_conn_id++;
	return conn->id;
}

void ConnectionManager::remove(Connection *conn)
{
	if (conn == NULL)
		return;

	int id = conn->id;
	std::string name = conn->cname;
	_connections.erase(std::remove(_connections.begin(), _connections.end(), conn), _connections.end());
	_connection_map.erase(id);
	_connection_name_map.erase(name);
	delete (conn);
}

bool ConnectionManager::exists(std::string cname)
{
	return false;
}

int ConnectionManager::setCurrent(std::string cname)
{
	return 0;
}

int ConnectionManager::setCurrent(int cid)
{
	return 0;
}


