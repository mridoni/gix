#include "IdeStatusSyncSetter.h"
#include "Ide.h"
#include "IdeTaskManager.h"

IdeStatusSyncSetter::IdeStatusSyncSetter(IdeStatus _at_start, IdeStatus _at_end, bool _force_signals) : 
											at_start(_at_start), at_end(_at_end), force_signals(_force_signals)
{
	Ide::TaskManager()->setStatus(at_start, force_signals);
}

IdeStatusSyncSetter::~IdeStatusSyncSetter()
{
	Ide::TaskManager()->setStatus(at_end, force_signals);
}
