#pragma once

#include "BuildActionHandler.h"

class BuildActionNoOpHandler : public BuildActionHandler
{
	// Inherited via BuildActionHandler
	virtual bool startBuild() override;
};

