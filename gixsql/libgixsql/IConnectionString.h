/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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



class IConnectionString
{

public:

	virtual int init(std::string name, std::string user, std::string password) = 0;
	virtual std::string get() = 0;

	virtual std::string getDbType() = 0;
	virtual std::string getHost() = 0;
	virtual int getPort() = 0;
	virtual std::string getDbName() = 0;
	virtual std::string getUsername() = 0;
	virtual std::string getPassword() = 0;
	virtual std::string getDefaultSchema() = 0;

	virtual void setPassword(std::string) = 0;

	virtual std::string toConnectionString(bool use_pwd, std::string use_this_pwd = "") = 0;
	virtual std::string getName() = 0;
};

