#pragma once

#include <string>
#include <vector>

#include "ICursor.h"
#include "Connection.h"
#include "SqlVar.h"
#include "SqlVarList.h"

using namespace std;

class Connection;

class Cursor : public ICursor
{

public:
	Cursor();
	~Cursor();

	// ICursor
	void setConnection(IConnection *) override;
	void setName(string) override;
	void setQuery(string) override;
	void setNumParams(int) override;
	IConnection *getConnection() override;
	string getName() override;
	string getQuery() override;
	int getNumParams() override;
	bool isOpen() override;
	bool isWithHold() override;
	void setOpened(bool) override;
	vector<string> getParameterValues() override;
	vector<int> getParameterTypes() override;

	// For private DbInterfaceData
	void *getPrivateData() override;
	void setPrivateData(void *) override;
	
	void setParameters(SqlVarList&);
	SqlVarList& getParameters();
	void createRealDataforParameters();
	void setWithHold(bool);

	uint64_t getRowNum() override;
	void increaseRowNum() override;


private:

	Connection *connection; // connection id
	string name; // default NULL
	string query; // default NULL
	int nParams; // params for query
	bool is_opened; //open flag
	bool is_with_hold;
	int tuples; //fetched row number

	SqlVarList parameter_list; // parameter list

	void *dbi_data;

	uint64_t rownum;
};

