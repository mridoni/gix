#include "Cursor.h"
#include "SqlVar.h"
#include "SqlVarList.h"


Cursor::Cursor()
{
	dbi_data = NULL;
	rownum = 0;
}


Cursor::~Cursor()
{
}

void Cursor::setConnection(IConnection *c)
{
	connection = (Connection *) c;
}

void Cursor::setName(string n)
{
	name = n;
}

void Cursor::setQuery(string q)
{
	query = q;
}

void Cursor::setNumParams(int np)
{
	nParams = np;
}

IConnection *Cursor::getConnection()
{
	return (IConnection *) connection;
}

string Cursor::getName()
{
	return name;
}

string Cursor::getQuery()
{
	return query;
}

int Cursor::getNumParams()
{
	return nParams;
}

bool Cursor::isOpen()
{
	return is_opened;
}

bool Cursor::isWithHold()
{
	return is_with_hold;
}

void Cursor::setOpened(bool b)
{
	is_opened = b;
}

void Cursor::setParameters(SqlVarList &l)
{
	vector<SqlVar *>::iterator it;
	for (it = l.begin(); it != l.end(); it++) {
		SqlVar *v = (*it)->copy();
		parameter_list.push_back(v);
	}
}

SqlVarList& Cursor::getParameters()
{
	return parameter_list;
}

void Cursor::createRealDataforParameters()
{
	vector<SqlVar *>::iterator it;
	for (it = parameter_list.begin(); it != parameter_list.end(); it++) {
		SqlVar *v = (*it);
		v->createRealData();
	}
}

void Cursor::setWithHold(bool f)
{
	is_with_hold = f;
}

uint64_t Cursor::getRowNum()
{
	return rownum;
}

void Cursor::increaseRowNum()
{
	rownum++;
}

vector<string> Cursor::getParameterValues()
{
	vector<string> params;

	createRealDataforParameters();	// Just in case
	for (int i = 0; i < parameter_list.size(); i++) {
		params.push_back(string(parameter_list.at(i)->getRealData()));
	}
	return params;
}

vector<int> Cursor::getParameterTypes()
{
	vector<int> param_types;

	for (int i = 0; i < parameter_list.size(); i++) {
		param_types.push_back(parameter_list.at(i)->getType());
	}
	return param_types;
}

void * Cursor::getPrivateData()
{
	return dbi_data;
}

void Cursor::setPrivateData(void *d)
{
	dbi_data = d;
}
