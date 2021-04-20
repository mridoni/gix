/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#pragma once

#include "ISymbolProvider.h"

class CodeviewSymbolProvider : public ISymbolProvider
{
public:

	// Inherited via ISymbolProvider
	virtual bool initialize(GixDebugger *gd, void *hproc, void *userdata) override;
	virtual bool isGnuCOBOLModule(GixDebugger *gd, void *hproc, void *hmod, void *userdata, int *err) override;
	virtual void *loadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *err) override;
	virtual bool unloadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *err) override;
	virtual SharedModuleInfo *extractModuleDebugInfo(GixDebugger *gd, void *hproc, void *hmod, void *hsym, const QString &mod_path, void *userdata, int *err) override;
	virtual LibCobInfo *extractLibCobInfo(GixDebugger *gd, void *hproc, void *hmod, void *hsym, const QString &mod_path, void *userdata, int *err) override;
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

	GixDebugger *dbg_instance = nullptr;
};

