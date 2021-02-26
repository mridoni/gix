#pragma once

#include <vector>
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
	bool exists(string cname);
	Cursor *get(string cname);

private:
	vector<Cursor *> _cursor_list;
	map<string, Cursor *> _cursor_map;
};

