#pragma once

#include <QMap>

#include "CompilerDefinition.h"
#include "gixcommon_global.h"

class CompilerManager
{

public:
	GIXCOMMON_EXPORT CompilerManager();
	GIXCOMMON_EXPORT ~CompilerManager();

	GIXCOMMON_EXPORT void init();

	GIXCOMMON_EXPORT QMap<QString, CompilerDefinition*> getCompilers();

private:
	QMap<QString, CompilerDefinition*> compiler_defs;

	void cleanup();
};

