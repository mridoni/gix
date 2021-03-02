#pragma once

#include "ISymbolProvider.h"
#include "dwarf.h"
#include "libdwarf.h"

struct module_dwarf_private_data;

class DwarfSymbolProvider : public ISymbolProvider
{
public:

	//// Inherited via ISymbolProvider
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
	std::vector<void *> symbol_handles;

private:
	static QMap<QString, struct module_dwarf_private_data *> module_data;

	bool is_acceptable_source(GixDebugger *gd, const QString &filename);
	bool get_vars(CobolModuleInfo *cmi);


	int readSymbolValueAsInt(GixDebugger *gd, CobolModuleInfo *cmi, Dwarf_Debug dbg, void *hproc, const QString &s);
	uint8_t *readSymbolValueInBuffer(GixDebugger *gd, CobolModuleInfo *cmi, Dwarf_Debug dbg, void *hproc, const QString &s, int bfrlen);
	void *get_local_var_addr(CobolModuleInfo *cmi, const QString &sym_name);

};

