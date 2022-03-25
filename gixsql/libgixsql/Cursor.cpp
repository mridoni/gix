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

#include "Cursor.h"
#include "SqlVar.h"
#include "SqlVarList.h"
#include "utils.h"


Cursor::Cursor()
{
	dbi_data = NULL;
	rownum = 0;
	connection = nullptr;
	is_opened = false;
	is_with_hold = false;
	nParams = 0;
	tuples = 0;
}


Cursor::~Cursor()
{
}

void Cursor::setConnection(IConnection *c)
{
	connection = (Connection *) c;
}

void Cursor::setConnectionName(std::string n)
{
	connection_name = n;
}

void Cursor::setName(std::string n)
{
	name = n;
}

void Cursor::setQuery(std::string q)
{
	query = q;
}

void Cursor::setNumParams(int np)
{
	nParams = np;
}

IConnection *Cursor::getConnection()
{
	return (IConnection *) connection;
}

std::string Cursor::getConnectionName()
{
	return connection_name;
}

std::string Cursor::getName()
{
	return name;
}

std::string Cursor::getQuery()
{
	return query;
}

int Cursor::getNumParams()
{
	return nParams;
}

bool Cursor::isOpen()
{
	return is_opened;
}

bool Cursor::isWithHold()
{
	return is_with_hold;
}

void Cursor::setOpened(bool b)
{
	is_opened = b;
}

void Cursor::setParameters(SqlVarList &l)
{
	std::vector<SqlVar *>::iterator it;
	for (it = l.begin(); it != l.end(); it++) {
		SqlVar *v = (*it)->copy();
		parameter_list.push_back(v);
	}
}

SqlVarList& Cursor::getParameters()
{
	return parameter_list;
}

void Cursor::createRealDataforParameters()
{
	std::vector<SqlVar *>::iterator it;
	for (it = parameter_list.begin(); it != parameter_list.end(); it++) {
		SqlVar *v = (*it);
		v->createRealData();
	}
}

void Cursor::setWithHold(bool f)
{
	is_with_hold = f;
}

uint64_t Cursor::getRowNum()
{
	return rownum;
}

void Cursor::increaseRowNum()
{
	rownum++;
}

std::vector<std::string> Cursor::getParameterValues()
{
	std::vector<std::string> params;

	createRealDataforParameters();	// Just in case
	for (int i = 0; i < parameter_list.size(); i++) {
		params.push_back(std::string(parameter_list.at(i)->getRealData()));
	}
	return params;
}

std::vector<int> Cursor::getParameterTypes()
{
	std::vector<int> param_types;

	for (int i = 0; i < parameter_list.size(); i++) {
		param_types.push_back(parameter_list.at(i)->getType());
	}
	return param_types;
}

void * Cursor::getPrivateData()
{
	return dbi_data;
}

void Cursor::setPrivateData(void *d)
{
	dbi_data = d;
}

void Cursor::setConnectionReference(void *d, int l)
{
	connref_data = d;
	connref_datalen = l;
}

std::string Cursor::getConnectionNameFromReference()
{
	if (!connref_data)
		return std::string();

	if (!connref_datalen)
		return std::string((char *)connref_data);

	std::string s = std::string((char *)connref_data, connref_datalen);
	return trim_copy(s);
}
