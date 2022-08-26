/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#pragma once

#include <string>

#include "IDbInterface.h"
#include "IDataSourceInfo.h"
#include "IConnectionOptions.h"

class IDataSourceInfo;
class IDbInterface;

class IConnection
{

public:

	virtual int getId() = 0;
	virtual bool isOpen() = 0;
	virtual void setName(std::string) = 0;
	virtual void setConnectionInfo(IDataSourceInfo *) = 0;
	virtual void setOpened(bool) = 0;
	virtual void setDbInterface(IDbInterface *) = 0;
	virtual IDataSourceInfo *getConnectionInfo() = 0;
	virtual IDbInterface *getDbInterface() = 0;
	virtual IConnectionOptions* getConnectionOptions() const = 0;
	virtual void setConnectionOptions(IConnectionOptions *) = 0;
};


