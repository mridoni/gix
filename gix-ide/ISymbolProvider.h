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

#include "GixDebugger.h"

class ISymbolProvider
{
public:

	virtual bool initialize(GixDebugger *gd, void *hproc, void *userdata) = 0;
	virtual bool isGnuCOBOLModule(GixDebugger *gd, void *hproc, void *hmod, void *userdata, int *err) = 0;
	virtual void *loadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *err) = 0;
	virtual bool unloadSymbols(GixDebugger *gd, void *hproc, void *hmod, const QString &mod_path, void *userdata, int *err) = 0;
	virtual SharedModuleInfo *extractModuleDebugInfo(GixDebugger *gd, void *hproc, void *hmod, void *hsym, const QString &mod_path, void *userdata, int *err) = 0;
	virtual LibCobInfo *extractLibCobInfo(GixDebugger *gd, void *hproc, void *hmod, void *hsym, const QString &mod_path, void *userdata, int *err) = 0;
	virtual void *getSymbolAddress(GixDebugger *gd, void *hproc, void *hmod, const QString &sym_name, void *userdata, int *err) = 0;
	virtual bool initCobolModuleLocalInfo(GixDebugger *gd, void *hproc, CobolModuleInfo *) = 0;
	virtual bool initCobolModulePreprocessedBlockInfo(GixDebugger* gd, void* hproc, SharedModuleInfo* mi) = 0;
	virtual bool deinit(GixDebugger *gd, void *hproc) = 0;
	virtual QString dumpStackFrame(GixDebugger *gd, void *hproc, void *hthread) = 0;

	virtual void *resolveLocalVariableAddress(GixDebugger *gd, void *hproc, CobolModuleInfo *cmi, uint64_t frame_ptr, VariableResolverData *rootvar, VariableResolverData *vvar) = 0;

protected:

	void _dbgMessageTrace(GixDebugger *gd, QString msg) {
		gd->getInterfaceBlock()->debuggerMessage(gd, msg, 0);
	}

	bool filterPreprocessedBlockBreakpoints(SharedModuleInfo* mi)
	{
		auto bk = mi->owner->breakpoints_by_line;
		if (!mi)
			return false;

		for (auto m : mi->cbl_modules) {

			for (auto ppblk : m->preprocessed_blocks) {

				if (ppblk->pp_gen_start_line < ppblk->pp_gen_end_line)
					continue;

				QString bkp_id = QDir::toNativeSeparators(QString::fromStdString(ppblk->pp_source_file)) + ":" + QString::number(ppblk->pp_gen_start_line);
				if (!mi->owner->breakpoints_by_line.contains(bkp_id))
					continue;

				//if (ppblk->pp_end_line == ppblk->pp_start_line) {
				//	QList<UserBreakpoint *> bkps = mi->owner->breakpoints_by_line.values(bkp_id);
				//	for (auto bkp : bkps) {
				//		//bkp->uninstall();
				//		mi->owner->breakPointRemove(bkp);
				//		mi->owner->getInterfaceBlock()->debuggerMessage(mi->owner, "PPBKP filter: removed breakpoint at " + bkp_id, 0);
				//	}
				//}
				//else {
					for (int ln = ppblk->pp_gen_start_line + 1; ln <= ppblk->pp_gen_end_line; ln++) {
						QString bkp_id = QString::fromStdString(ppblk->pp_source_file) + ":" + QString::number(ln);
						if (!mi->owner->breakpoints_by_line.contains(bkp_id))
							continue;

						QList<UserBreakpoint*> bkps = mi->owner->breakpoints_by_line.values(bkp_id);
						for (auto bkp : bkps) {
							bkp->uninstall();
							mi->owner->breakPointRemove(bkp);
							mi->owner->getInterfaceBlock()->debuggerMessage(mi->owner, "PPBKP filter: removed breakpoint at " + bkp_id, 0);
						}
					}
				//}
			}
			
		}

		return true;
	}
};

