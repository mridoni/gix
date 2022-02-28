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

#include "SqlVarList.h"
#include "Logger.h"



SqlVarList::SqlVarList()
{
}


SqlVarList::~SqlVarList()
{
	std::vector<SqlVar*>::iterator it;
	for (it = this->begin(); it != this->end(); ++it) {
		if (*it)
			delete(*it);
	}
}

SqlVar * SqlVarList::AddVar(int type, int length, int power, uint32_t flags, void *addr)
{
	SqlVar * v = new SqlVar(type, length, power, flags, addr);

	v->createRealData();

	std::vector<SqlVar *>::push_back(v);
	return v;
}

void SqlVarList::clear()
{
	std::vector<SqlVar *>::iterator it;

	for (it = std::vector<SqlVar *>::begin(); it != std::vector<SqlVar *>::end(); it++) {
		if (*it != NULL)
			delete(*it);
	}

	std::vector<SqlVar *>::clear();
}

void SqlVarList::dump()
{
	DECLARE_LOGGER(logger);
	std::vector<SqlVar *>::iterator it;
	for (it = std::vector<SqlVar *>::begin(); it != std::vector<SqlVar *>::end(); it++) {
		SqlVar * v = *it;
		if (v != NULL) 
			LOG_DEBUG(__FILE__, __func__, "%p %d %d %d %p\n", v, v->type, v->length, v->power, v->addr);
	}
}

int SqlVarList::getMaxLength()
{
	if (!size())
		return 0;

	int l = 0;
	for (int i = 0; i < size(); i++) {
		if (this->at(i)->length > l)
			l = this->at(i)->length;
	}
	return l;
}
