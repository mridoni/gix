#pragma once

#include "ISymbolProvider.h"

class CodeviewSymbolProvider : public ISymbolProvider
{
public:

	// Inherited via ISymbolProvider
	virtual bool initialize(GixDebugger *gd, void *hproc, void *userdata) override;
	virtual bool isGnuCOBOLModule(GixDebugger *gd, void *hproc, void *hmod, void *userdata, int *err) override;
	virtual void *loadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *err) override;
	virtual SharedModuleInfo *extractModuleDebugInfo(GixDebugger *gd, void *hproc, void *hmod, void *hsym, const QString &mod_path, void *userdata, int *err) override;
	virtual void * getSymbolAddress(GixDebugger *gd, void *hproc, void *hmod, const QString &sym_name, void *userdata, int *err) override;
	virtual bool initCobolModuleLocalInfo(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi) override;
	virtual bool deinit(GixDebugger *gd, void *hproc) override;
	virtual QString dumpStackFrame(GixDebugger *gd, void *hproc, void *hthread) override;
	virtual void *resolveLocalVariableAddress(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi, uint64_t frame_ptr, VariableResolverData *rootvar, VariableResolverData *vvar) override;

protected:
	std::vector<DWORD64> symbol_handles;

	bool is_dwarf2pdb = false;	// for DWARF-converted symbols

private:
	int readSymbolValueAsInt(void *hproc, const QString &s);
	uint8_t *readSymbolValueInBuffer(void *hproc, const QString &s, int len);
};

