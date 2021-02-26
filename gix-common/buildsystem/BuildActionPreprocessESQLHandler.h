#pragma once

#include "BuildActionHandler.h"

class BuildActionPreprocessESQLHandler : public BuildActionHandler
{
public:
	BuildActionPreprocessESQLHandler();
	~BuildActionPreprocessESQLHandler();

	// Inherited via BuildActionHandler
	virtual bool startBuild() override;

private:

};

