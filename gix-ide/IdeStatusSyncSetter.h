#pragma once

#include "IdeStatus.h"

class IdeStatusSyncSetter
{
public:
	IdeStatusSyncSetter(IdeStatus _at_start, IdeStatus _at_end, bool _force_signals = false);
	~IdeStatusSyncSetter();

private:
	IdeStatus at_start;
	IdeStatus at_end;
	bool force_signals = false;
};

