#pragma once

using namespace std;

#include <vector>

#include "CobolVar.h"

class CobolVarList : public vector<CobolVar *>
{
public:
	CobolVarList();
	~CobolVarList();

	CobolVar * AddVar(int type, int length, int power, void * addr);

	void clear();
	void dump();
};

