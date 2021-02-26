#include "CobolVarList.h"
#include "QLogger.h"



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
	vector<CobolVar *>::iterator it;
	for (it = vector<CobolVar *>::begin(); it != vector<CobolVar *>::end(); it++) {
		CobolVar * v = *it;
#ifdef _DEBUG
		if (v != NULL) {
			QString msg;
			msg.sprintf("%s@%s: %p %d %d %d %p\n", __FILE__, __func__, v, v->type, v->length, v->power, v->addr);
			QLogger::QLog_Trace(GIX_CONSOLE_LOG, msg);
		}
#endif
	}
}