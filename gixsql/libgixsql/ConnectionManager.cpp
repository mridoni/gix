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


