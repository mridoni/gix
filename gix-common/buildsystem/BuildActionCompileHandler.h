#pragma once

#include <QList>
#include <QString>
#include <QStringList>

#include "BuildActionHandler.h"

class BuildActionCompileHandler : public BuildActionHandler
{
public:
	BuildActionCompileHandler();
	~BuildActionCompileHandler();

	// Inherited via BuildActionHandler
	virtual bool startBuild() override;

private:
	QStringList retrieve_copy_dirs();

};