#pragma once

#include <QString>

class BuildConsts {

public:
	const static QString MODULE_EXECUTABLE;
	const static QString MODULE_DYNLOAD;
	const static QString MODULE_DEFAULT;

	const static QString BUILD_ACTION_COMPILE;
	const static QString BUILD_ACTION_PREPROC_ESQL;
	const static QString BUILD_ACTION_LINK;
	const static QString BUILD_ACTION_GENERATE_SYMBOLS;
	const static QString BUILD_ACTION_COPY;
	const static QString BUILD_ACTION_MK_LISTING;
	const static QString BUILD_ACTION_NONE;
		
	const static QString TYPE_OBJ;
	const static QString TYPE_OBJ_MAIN;
	const static QString TYPE_LIB;
	const static QString TYPE_CBSQL;
	const static QString TYPE_CBLPP;
	const static QString TYPE_CBCICS;
	const static QString TYPE_COBOL;

	const static QString TYPE_LISTING;

	const static QString TYPE_FINAL;
	const static QString TYPE_NONE;
};