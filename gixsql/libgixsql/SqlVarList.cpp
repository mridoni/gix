#include "SqlVarList.h"
#include "Logger.h"



SqlVarList::SqlVarList()
{
}


SqlVarList::~SqlVarList()
{
	vector<SqlVar*>::iterator it;
	for (it = this->begin(); it != this->end(); ++it) {
		if (*it)
			delete(*it);
	}
}

SqlVar * SqlVarList::AddVar(int type, int length, int power, uint32_t flags, void *addr)
{
	SqlVar * v = new SqlVar();

	v->setType(type);
	v->setLength(length);
	v->setPower(power);
	v->setAddr(addr);
	v->setFlags(flags);

	v->createRealData();

	vector<SqlVar *>::push_back(v);
	return v;
}

void SqlVarList::clear()
{
	vector<SqlVar *>::iterator it;

	for (it = vector<SqlVar *>::begin(); it != vector<SqlVar *>::end(); it++) {
		if (*it != NULL)
			delete(*it);
	}

	vector<SqlVar *>::clear();
}

void SqlVarList::dump()
{
	DECLARE_LOGGER(logger);
	vector<SqlVar *>::iterator it;
	for (it = vector<SqlVar *>::begin(); it != vector<SqlVar *>::end(); it++) {
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
