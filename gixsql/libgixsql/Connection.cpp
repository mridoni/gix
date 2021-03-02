#include "Connection.h"
#include "DbInterfaceFactory.h"


Connection::Connection()
{	
	ext_conninfo = false;
	is_opened = false;
}


Connection::~Connection()
{
	if (dbi != NULL)
		DbInterfaceFactory::removeInterface(dbi);

	if (conninfo && !ext_conninfo)
		delete (conninfo);
}

int Connection::getId()
{
	return id;
}

bool Connection::isOpen()
{
	return is_opened;
}

void Connection::setName(std::string name)
{
	cname = name;
}

void Connection::setConnectionInfo(IConnectionString *conn_string)
{
	conninfo = conn_string;
	if (conn_string)
		ext_conninfo = true;
}

void Connection::setAutoCommit(bool ac)
{
	autocommit = ac;
}

void Connection::setOpened(bool i)
{
	is_opened = i;
}

void Connection::setEncoding(std::string enc)
{
	encoding = enc;
}

void Connection::setDbInterface(IDbInterface *_dbi)
{
	dbi = _dbi;
}

bool Connection::test(IConnectionString*)
{
	return false;
}

IConnectionString *Connection::getConnectionInfo()
{
	return conninfo;
}

std::string Connection::getEncoding()
{
	return encoding;
}

IDbInterface * Connection::getDbInterface()
{
	return dbi;
}

bool Connection::getAutoCommit()
{
	return autocommit;
}
