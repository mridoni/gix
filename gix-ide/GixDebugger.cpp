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

#include "GixDebugger.h"
#if defined(_WIN32) || defined(_WIN64)
#include "GixDebuggerWin64.h"
#else
#include "GixDebuggerLinux.h"
#endif

#include <QMap>
#include <QDir>

#define LOG_DEBUG(file, func, format, ...)
#define LOG_ERROR(format, ...) 

GixDebugger::GixDebugger()
{
	if_blk = nullptr;

	src_file_types.push_back("*.cbl");
	src_file_types.push_back("*.cbsql");
	src_file_types.push_back("*.cpy");
}


GixDebugger *GixDebugger::get()
{
	GixDebugger *gd = NULL;

#if defined(_WIN32) || defined(_WIN64)
	gd = new GixDebuggerWin64();
#else

	gd = new GixDebuggerLinux();

#endif

	return gd;
}

//typedef cob_global *(*libcob_cob_get_global_ptr_t)(void);
//typedef int (*libcob_cob_is_initialized_t)(void);


bool GixDebugger::existsBreakpoint(const QString &src_file, int ln)
{
	QString k = src_file + ":" + QString::number(ln);
	return (breakpoints.find(k) != breakpoints.end());
}

UserBreakpoint *GixDebugger::findBreakpointByAddress(void *addr)
{
	if (!addr)
		return nullptr;

	for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
		UserBreakpoint *bkp = it.value();

		if (bkp->address == addr)
			return bkp;
	}

	return nullptr;
}

bool GixDebugger::isCobolSourceFile(const QString &filename)
{
	for (auto t : src_file_types) {
		t = t.replace("*", "");
		if (filename.endsWith(t))
			return true;
	}
	return false;
}



void GixDebugger::getAndResolveUserBreakpoints()
{
	QList<QPair<QString, int>> new_bkps;
	if_blk->getBreakpoints(this, new_bkps);

	// First: find all resolved breakpoints that have been deleted
	QList<UserBreakpoint *> to_be_removed;
	for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
		UserBreakpoint *bkp = it.value();
		if (bkp->automatic)
			continue;

		auto itt = std::find_if(new_bkps.begin(), new_bkps.end(), [bkp](QPair<QString, int> s) { return s.first == bkp->source_file && s.second == bkp->line; });
		if (itt == new_bkps.end()) {
			to_be_removed.push_back(bkp);
		}
	}

	for (auto bkp : to_be_removed) {
		bkp->automatic = true;
	}

	// Second: we add new breakpoints
	for (auto it = new_bkps.begin(); it != new_bkps.end(); ++it) {
		QPair<QString, int> newbp = *it;
		//auto itt = std::find_if(breakpoints.begin(), breakpoints.end(), [newbp](QPair<QString, UserBreakpoint *> b) { return newbp.first == b.second->source_file && newbp.second == b.second->line; });
		auto itt = std::find_if(breakpoints.begin(), breakpoints.end(), [newbp](UserBreakpoint *b) { return QDir::cleanPath(newbp.first) == QDir::cleanPath(b->source_file) && newbp.second == b->line; });
		if (itt == breakpoints.end()) {
			UserBreakpoint *bkp = new UserBreakpoint();
			bkp->key = newbp.first + ":" + QString::number(newbp.second);
			bkp->source_file = newbp.first;
			bkp->line = newbp.second;
			bkp->orig_instr = 0x00;
			bkp->address = 0x00000000;
			bkp->automatic = false;
			breakpoints[bkp->key] = bkp;
		}
		else {
			itt.value()->automatic = false;
		}
	}

	// we resolve everything (where needed)
	for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
		UserBreakpoint *bkp = it.value();
		if (bkp->address)
			continue;

		if (source_lines.find(it.key()) != source_lines.end()) {
			if_blk->debuggerMessage(this, "Resolved breakpoint (" + it.key() + ")" + QString::number((uint64_t)bkp->address), 0);
			bkp->address = source_lines[it.key()]->addr;

			installHardwareBreakpoint(bkp);
		}
	}
}


GixDebugger::~GixDebugger()
{

}

void GixDebugger::setDebuggingEnabled(bool b)
{
	is_debugging_enabled = b;
}
void GixDebugger::setProperty(const QString &k, const QString &v)
{
	properties[k] = v;
}
void GixDebugger::setInterfaceBlock(GixDebuggerInterfaceBlock *ib)
{
	if_blk = ib;
}

void GixDebugger::setEnvironment(const QMap<QString, QString>  vars)
{
	environment = vars;
}

void GixDebugger::setProcess(const QString &path)
{
	exepath = path;
}

void GixDebugger::setWorkingDirectory(const QString &wd)
{
	working_dir = QDir::cleanPath(wd);
}

void GixDebugger::setModuleDirectory(const QString &md)
{
	module_dir = QDir::cleanPath(md);
}

void GixDebugger::setCommandLine(const QString &args)
{
	cmd_line_args = args;
}

void GixDebugger::setSourceFileTypes(const QList<QString> &_src_file_types)
{
	src_file_types = _src_file_types;
}

void GixDebugger::setSymbolDirectories(const QList<QString> &_sym_dirs)
{
	sym_dirs = _sym_dirs;
}

void GixDebugger::setBreakAtModuleStart(bool b)
{
	break_at_module_start = b;
}

void GixDebugger::setVerbose(bool b)
{
	verbose = b;
}

void GixDebugger::setUseExternalConsole(bool b)
{
	use_external_console = b;
}

void GixDebugger::setDebuggedModuleType(DebuggedModuleType b)
{
	debuggee_type = b;
}

QString GixDebugger::getWorkingDirectory()
{
	return working_dir;
}

QString GixDebugger::getModuleDirectory()
{
	return module_dir;
}

DebuggedModuleType GixDebugger::getDebuggedModuleType()
{
	return debuggee_type;
}

SharedModuleInfo::~SharedModuleInfo()
{

}
