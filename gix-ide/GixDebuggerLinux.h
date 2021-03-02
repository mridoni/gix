#pragma once

#include "GixDebugger.h"

class GixDebuggerLinux : public GixDebugger {
	
public:

	virtual int start() override;
	virtual int stop() override;
	virtual bool step() override;
	virtual bool continue_running() override;
	
	virtual bool getVariables(QList<VariableData *> var_list) override;
	virtual QString getCurrentCobolModuleName() override;

	virtual void printLastError() override;

	virtual void removeHardwareBreakpoint(UserBreakpoint *bkp) override;
	virtual bool installHardwareBreakpoint(UserBreakpoint *bkp) override;

	virtual bool readProcessMemory(void *addr, void *bfr, int size) override;	
	
private:
	virtual void *getSymbolAddress(const char *sym_name) override;	
};