/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 2.1,
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

#include "CursorManager.h"
#include <algorithm>


CursorManager::CursorManager()
{
}


CursorManager::~CursorManager()
{
	std::vector<Cursor*>::iterator it;
	for (it = _cursor_list.begin(); it != _cursor_list.end(); ++it) {
		if (*it)
			delete (*it);
	}
}

Cursor * CursorManager::create()
{
	return new Cursor();
}

int CursorManager::add(Cursor *cursor)
{
	_cursor_list.push_back(cursor);
	_cursor_map[cursor->getName()] = cursor;
	return _cursor_list.size();
}

void CursorManager::remove(Cursor *c)
{
	_cursor_map.erase(c->getName());
	_cursor_list.erase(std::remove(_cursor_list.begin(), _cursor_list.end(), c), _cursor_list.end());
	delete c;
}

bool CursorManager::exists(std::string cname)
{
	std::map<std::string, Cursor *>::iterator it = _cursor_map.find(cname);
	return (it != _cursor_map.end());
}

Cursor * CursorManager::get(std::string cname)
{
	if (!exists(cname))
		return NULL;
	else
		return _cursor_map[cname];
}

void CursorManager::clearConnectionCursors(int connId, bool clear_held_cursors)
{
	std::vector<Cursor *> _cur_to_del;
	std::vector<Cursor *>::iterator it;

	for (it = _cursor_list.begin(); it != _cursor_list.end(); it++) {
		IConnection * conn = (*it)->getConnection();
		if (conn != NULL && (*it)->getConnection()->getId() == connId && (!(*it)->isWithHold() || clear_held_cursors))
			_cur_to_del.push_back((*it));
	}

	for (it = _cur_to_del.begin(); it != _cur_to_del.end(); it++) {
		Cursor *c = (*it);
		_cursor_map.erase(c->getName());
		remove(c);
	}
}
