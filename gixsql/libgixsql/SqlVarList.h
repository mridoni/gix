#pragma once

using namespace std;

#include <vector>

#include "SqlVar.h"

class SqlVarList : public vector<SqlVar *>
{
public:
	SqlVarList();
	~SqlVarList();

	SqlVar * AddVar(int type, int length, int power, uint32_t flags, void * addr);

	void clear();
	void dump();

	int getMaxLength();
};

