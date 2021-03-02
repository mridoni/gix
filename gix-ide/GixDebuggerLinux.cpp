#include "GixDebuggerLinux.h"

int GixDebuggerLinux::start()
{
	return 0;
}


int GixDebuggerLinux::stop()
{
	return false;
}


bool GixDebuggerLinux::step()
{
	return false;
}


bool GixDebuggerLinux::continue_running()
{
	return false;
}

	
bool GixDebuggerLinux::getVariables(QList<VariableData *> var_list)
{
	return false;
}


QString GixDebuggerLinux::getCurrentCobolModuleName()
{
	return QString();
}


void GixDebuggerLinux::printLastError()
{
}


void GixDebuggerLinux::removeHardwareBreakpoint(UserBreakpoint *bkp)
{
}


bool GixDebuggerLinux::installHardwareBreakpoint(UserBreakpoint *bkp)
{
	return false;
}


bool GixDebuggerLinux::readProcessMemory(void *addr, void *bfr, int size)
{
	return false;
}

void *GixDebuggerLinux::getSymbolAddress(const char *sym_name) 
{
	return nullptr;
}	
	