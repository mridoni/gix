#pragma once

#include "GixDebugger.h"

class ISymbolProvider
{
public:

	virtual bool initialize(GixDebugger *gd, void *hproc, void *userdata) = 0;
	virtual bool isGnuCOBOLModule(GixDebugger *gd, void *hproc, void *hmod, void *userdata, int *err) = 0;
	virtual void *loadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *err) = 0;
	virtual SharedModuleInfo *extractModuleDebugInfo(GixDebugger *gd, void *hproc, void *hmod, void *hsym, const QString &mod_path, void *userdata, int *err) = 0;
	virtual void *getSymbolAddress(GixDebugger *gd, void *hproc, void *hmod, const QString &sym_name, void *userdata, int *err) = 0;
	virtual bool initCobolModuleLocalInfo(GixDebugger *gd, void *hproc, CobolModuleInfo *) = 0;
	virtual bool deinit(GixDebugger *gd, void *hproc) = 0;
	virtual QString dumpStackFrame(GixDebugger *gd, void *hproc, void *hthread) = 0;

	virtual void *resolveLocalVariableAddress(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi, uint64_t frame_ptr, VariableResolverData *rootvar, VariableResolverData *vvar) = 0;
};

