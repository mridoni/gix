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
	void setName(std::string) override;
	void setQuery(std::string) override;
	void setNumParams(int) override;
	IConnection *getConnection() override;
	std::string getName() override;
	std::string getQuery() override;
	int getNumParams() override;
	bool isOpen() override;
	bool isWithHold() override;
	void setOpened(bool) override;
	std::vector<std::string> getParameterValues() override;
	std::vector<int> getParameterTypes() override;

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
	std::string name; // default NULL
	std::string query; // default NULL
	int nParams; // params for query
	bool is_opened; //open flag
	bool is_with_hold;
	int tuples; //fetched row number

	SqlVarList parameter_list; // parameter list

	void *dbi_data;

	uint64_t rownum;
};

