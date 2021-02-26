#pragma once

#include <QString>

#include "IConnectionString.h"
#include "DbConnection.h"
//#include "DbInterfaceFactory.h"

enum class CodeGenerationType
{
	CobolCopy,
	InsertScript,
	SelectScript,
	CreateScript,
	UpdateScript,
	DeleteScript
};

enum class CodeGenerationDest
{
	Clipboard,
	File
};

class DbCodeGenerator
{
public:
	void setUseUpperCaseForCopyFields(bool);
	bool generateCopyFile(IConnectionString* conn_info, QString schema, QString table, QString& res);

private: 
	bool uc_copy_fields;
};

