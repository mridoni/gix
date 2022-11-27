/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#include "CobolVarList.h"
#include "GixGlobals.h"

CobolVarList::CobolVarList()
{
}


CobolVarList::~CobolVarList()
{
}

CobolVar * CobolVarList::AddVar(int type, int length, int power, void *addr)
{
	CobolVar * v = new CobolVar();

	v->setType(type);
	v->setLength(length);
	v->setPower(power);
	v->setAddr(addr);

	v->createRealData();

	vector<CobolVar *>::push_back(v);
	return v;
}

void CobolVarList::clear()
{
	vector<CobolVar *>::iterator it;

	for (it = vector<CobolVar *>::begin(); it != vector<CobolVar *>::end(); it++) {
		if (*it != NULL)
			delete(*it);
	}

	vector<CobolVar *>::clear();
}

void CobolVarList::dump()
{
	auto logger = GixGlobals::getLogManager();

	vector<CobolVar *>::iterator it;
	for (it = vector<CobolVar *>::begin(); it != vector<CobolVar *>::end(); it++) {
		CobolVar * v = *it;
#ifdef _DEBUG
		//#LOG
		//if (v != NULL) {
		//	QString msg;
		//	msg.sprintf("%s@%s: %p %d %d %d %p\n", __FILE__, __func__, v, v->type, v->length, v->power, v->addr);
		//	QLogger::QLog_Trace(GIX_CONSOLE_LOG, msg);
		//}
#endif
	}
}