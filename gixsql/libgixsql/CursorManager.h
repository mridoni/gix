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

