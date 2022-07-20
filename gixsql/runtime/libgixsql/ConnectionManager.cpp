/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 3,
* or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; see the file COPYING.LIB.  If
* not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
* Boston, MA 02110-1301 USA
*/


#include <string>
#include <vector>
#include <map>
#include <algorithm>

#include "ConnectionManager.h"
#include "DbInterfaceFactory.h"

#define GIXSQL_DEFAULT_CONN_PREFIX "DEFAULT"

static std::vector<Connection *> _connections;
static std::map<int, Connection *> _connection_map;
static std::map<std::string, Connection *> _connection_name_map;

static int next_conn_id = 1;

ConnectionManager::ConnectionManager()
{
}


ConnectionManager::~ConnectionManager()
{

}

Connection *ConnectionManager::create()
{
	return new Connection();
}

Connection *ConnectionManager::get(std::string name)
{
	if (name.empty())
		return default_connection;

	if (_connection_name_map.find(name) != _connection_name_map.end())
		return _connection_name_map[name];

	return nullptr;
}

int ConnectionManager::add(Connection *conn)
{
	conn->id = ++next_conn_id;
	if (conn->name.empty()) {
		conn->name = GIXSQL_DEFAULT_CONN_PREFIX + std::to_string(conn->id);
		this->default_connection = conn;
	}

	_connections.push_back(conn);
	_connection_map[conn->id] = conn;
	_connection_name_map[conn->name] = conn;

	return conn->id;
}

void ConnectionManager::remove(Connection *conn)
{
	if (conn == NULL)
		return;

	int id = conn->id;
	std::string name = conn->name;
	_connections.erase(std::remove(_connections.begin(), _connections.end(), conn), _connections.end());
	_connection_map.erase(id);
	_connection_name_map.erase(name);

	if (conn == default_connection)
		default_connection = nullptr;

	delete (conn);
}

bool ConnectionManager::exists(std::string cname)
{
	return _connection_name_map.find(cname) != _connection_name_map.end();
}

std::vector<Connection*> ConnectionManager::list()
{
	return _connections;
}


