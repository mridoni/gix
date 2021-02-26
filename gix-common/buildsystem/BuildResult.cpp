#include "BuildResult.h"


bool BuildResult::isSuccess()
{
	return status == 0;
}

int BuildResult::getStatus()
{
	return status;
}

QStringList BuildResult::buildlog()
{
	return build_log;
}
