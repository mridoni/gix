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

#pragma once

#include <vector>
#include <string>
#include <map>

#include "Cursor.h"

class CursorManager
{
public:
	CursorManager();
	~CursorManager();

	Cursor *create();
	void clearConnectionCursors(int, bool);
	
	//static Cursor *current();
	int add(Cursor *);
	void remove(Cursor *);
	bool exists(std::string cname);
	Cursor *get(std::string cname);

private:
	std::vector<Cursor *> _cursor_list;
	std::map<std::string, Cursor *> _cursor_map;
};

