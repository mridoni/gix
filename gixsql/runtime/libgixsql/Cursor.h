/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 3,
* or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; see the file COPYING.LIB.  If
* not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
* Boston, MA 02110-1301 USA
*/

#pragma once

#include <string>
#include <vector>

#include "ICursor.h"
#include "Connection.h"
#include "SqlVar.h"
#include "SqlVarList.h"



class Connection;

class Cursor : public ICursor
{

public:
	Cursor();
	~Cursor();

	// ICursor
	void setConnection(IConnection *) override;
	void setConnectionName(std::string) override;
	void setName(std::string) override;
	void setQuery(std::string) override;
	void setQuerySource(void*, int) override;
	void setNumParams(int) override;
	IConnection *getConnection() override;
	std::string getConnectionName() override;
	std::string getName() override;
	std::string getQuery() override;
	void getQuerySource(void**, int*) override;
	int getNumParams() override;
	bool isOpen() override;
	bool isWithHold() override;
	void setOpened(bool) override;
	std::vector<std::string> getParameterValues() override;
	std::vector<int> getParameterTypes() override;
	std::vector<int> getParameterLengths() override;

	// For private DbInterfaceData
	void *getPrivateData() override;
	void setPrivateData(void *) override;
	
	void setParameters(SqlVarList&);
	SqlVarList& getParameters();
	void createRealDataforParameters();
	void setWithHold(bool);

	uint64_t getRowNum() override;
	void increaseRowNum() override;

	void setConnectionReference(void *d, int l);
	std::string getConnectionNameFromReference();

private:

	Connection *connection; // connection id
	std::string connection_name; // default NULL
	std::string name; // default NULL
	std::string query; // default NULL
	void* query_source_addr = nullptr;
	int query_source_len = 0;
	int nParams; // params for query
	bool is_opened; //open flag
	bool is_with_hold;
	int tuples; //fetched row number

	SqlVarList parameter_list; // parameter list

	void* dbi_data = nullptr;

	uint64_t rownum;

	void *connref_data = nullptr;
	int connref_datalen = 0;
};

