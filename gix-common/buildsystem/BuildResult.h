#pragma once

#include <QStringList>

#include "gixcommon_global.h"

class BuildResult {

	friend class BuildDriver;
	friend class ProjectBuilder;
	friend class SingleArtifactBuilder;

public:
	GIXCOMMON_EXPORT bool isSuccess();
	GIXCOMMON_EXPORT int getStatus();
	GIXCOMMON_EXPORT QStringList buildlog();

private:
	int status;
	QStringList build_log;
};
