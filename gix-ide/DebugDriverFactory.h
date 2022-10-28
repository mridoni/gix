#pragma once

#include "IDebugDriver.h"

enum class DebugDriverType {
	Standard = 1,
	Experimental = 2,
};

class DebugManager;

class DebugDriverFactory
{
public:
	static IDebugDriver* get(DebugDriverType t, DebugManager* dm);
};

