#pragma once

#include <QList>
#include <QString>
#include <QStringList>

#include "BuildActionHandler.h"

class BuildActionMakeListingHandler : public BuildActionHandler
{
public:
	BuildActionMakeListingHandler();
	~BuildActionMakeListingHandler();

	// Inherited via BuildActionHandler
	virtual bool startBuild() override;

private:


};