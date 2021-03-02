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
