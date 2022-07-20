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

#include "Connection.h"
#include "DbInterfaceFactory.h"


Connection::Connection()
{	
	ext_conninfo = false;
	is_opened = false;
}


Connection::~Connection()
{
	if (dbi != NULL) {
		DbInterfaceFactory::removeInterface(dbi);
	}

	if (conninfo && !ext_conninfo)
		delete (conninfo);
}

int Connection::getId()
{
	return id;
}

bool Connection::isOpen()
{
	return is_opened;
}

void Connection::setName(std::string name)
{
	this->name = name;
}

std::string Connection::getName()
{
	return name;
}

void Connection::setConnectionInfo(IDataSourceInfo *conn_string)
{
	conninfo = conn_string;
	if (conn_string)
		ext_conninfo = true;
}

void Connection::setAutoCommit(bool ac)
{
	autocommit = ac;
}

void Connection::setOpened(bool i)
{
	is_opened = i;
}

void Connection::setEncoding(std::string enc)
{
	encoding = enc;
}

//
//bool Connection::getPreparedStatementData(std::string stmt_name, std::tuple<std::vector<std::string>, void *> &t)
//{
//	if (prepared_stmts.find(stmt_name) == prepared_stmts.end())
//		return false;
//
//	t = prepared_stmts[stmt_name];
//	return true;
//}

void Connection::setDbInterface(IDbInterface *_dbi)
{
	dbi = _dbi;
}

bool Connection::test(IDataSourceInfo*)
{
	return false;
}

IDataSourceInfo *Connection::getConnectionInfo()
{
	return conninfo;
}

std::string Connection::getEncoding()
{
	return encoding;
}

IDbInterface * Connection::getDbInterface()
{
	return dbi;
}

bool Connection::getAutoCommit()
{
	return autocommit;
}
