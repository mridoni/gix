#pragma once
#include "BuildActionHandler.h"
class BuildActionRestModuleHandler :
	public BuildActionHandler
{
public:
	BuildActionRestModuleHandler();
	~BuildActionRestModuleHandler();

	// Inherited via BuildActionHandler
	virtual bool startBuild() override;
};

