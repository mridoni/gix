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
#if defined(_WIN32)
#include "GixDebuggerWin.h"
#elif defined(__linux__)
#include "GixDebuggerLinux.h"
#else
#pragma warning Unsupported debugger platform 
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

#if defined(_WIN32)
	gd = new GixDebuggerWin();
#elif defined(__linux__)
	gd = new GixDebuggerLinux();
#endif

	return gd;
}

bool GixDebugger::existsBreakpoint(const QString &src_file, int ln)
{
	QString k = src_file + ":" + QString::number(ln);
	return (breakpoints_by_line.contains(k));
}

void GixDebugger::writeToProcess(QString s)
{
	// Nothing here
}

void GixDebugger::breakPointAdd(UserBreakpoint *bkp)
{
	if (!bkp)
		return;

	if (!breakpoints.contains(bkp))
		breakpoints.append(bkp);

#if BITNESS==64
	uint64_t addr = (uint64_t)bkp->address;
#else
	uint32_t addr = (uint32_t)bkp->address;
#endif
	if (!breakpoints_by_addr.contains(addr))
		breakpoints_by_addr.insert(addr, bkp);

	if (!breakpoints_by_line.contains(bkp->key))
		breakpoints_by_line.insert(bkp->key, bkp);
}

void GixDebugger::breakPointRemove(UserBreakpoint *bkp)
{
	if (!bkp)
		return;

	if (breakpoints.contains(bkp))
		breakpoints.removeOne(bkp);

#if BITNESS==64
	uint64_t addr = (uint64_t)bkp->address;
#else
	uint32_t addr = (uint32_t)bkp->address;
#endif
	if (breakpoints_by_addr.contains(addr))
		breakpoints_by_addr.remove(addr);

	if (breakpoints_by_line.contains(bkp->key))
		breakpoints_by_line.remove(bkp->key);

	delete bkp;
}

UserBreakpoint *GixDebugger::findBreakpointByAddress(void *addr)
{
	if (!addr)
		return nullptr;

	for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
		UserBreakpoint *bkp = *it;

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

#if _DEBUG
	_DBG_OUT("IDE is resolving user breakpoints (%d user breakpoint(s) received)\n", new_bkps.size());
	for (auto it = new_bkps.begin(); it != new_bkps.end(); ++it) {
		QPair<QString, int> newbp = *it;
		_DBG_OUT("IDE says: user breakpoint at %d@%s\n", newbp.second, newbp.first.toLocal8Bit().data());
	}
#endif


	// First: find all resolved source line breakpoints that have been deleted from the user list
	for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
		UserBreakpoint *bkp = *it;
		if (bkp->automatic)
			continue;

		auto itt = std::find_if(new_bkps.begin(), new_bkps.end(), [bkp](QPair<QString, int> s) { return s.first == bkp->source_file && s.second == bkp->line; });
		if (itt == new_bkps.end()) {
			bkp->automatic = true;
		}
	}

	// Second: we add new breakpoints
	for (auto it = new_bkps.begin(); it != new_bkps.end(); ++it) {
		QPair<QString, int> newbp = *it;
		auto itt = std::find_if(breakpoints.begin(), breakpoints.end(), [newbp](UserBreakpoint *b) { return QDir::cleanPath(newbp.first).toLower() == QDir::cleanPath(b->source_file).toLower() && newbp.second == b->line; });
		if (itt == breakpoints.end()) {
			UserBreakpoint *bkp = UserBreakpoint::createInstance();
			bkp->key = newbp.first + ":" + QString::number(newbp.second);
			bkp->source_file = newbp.first;
			bkp->line = newbp.second;
			bkp->orig_instr = 0x00;
			bkp->address = 0x00000000;
			bkp->automatic = false;

			breakPointAdd(bkp);
		}
		else {
			(*itt)->automatic = false;
		}
	}

	// we resolve everything (where needed)
	for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
		UserBreakpoint *bkp = *it;
		if (bkp->address)
			continue;

		if (source_lines.contains(bkp->key)) {
			if_blk->debuggerMessage(this, "Resolved breakpoint (" + bkp->key + ")" + QString::number((uint64_t)bkp->address), 0);
			bkp->address = source_lines[bkp->key]->addr;

			bkp->install();
		}
	}
}


bool GixDebugger::isCblEntryPoint(void *addr, CobolModuleInfo **cmi)
{
	for (auto mi : shared_modules) {
		for (auto it = mi->cbl_modules.begin(); it != mi->cbl_modules.end(); ++it) {
			fprintf(stderr, "isCblEntryPoint - entry_point: %p - addr: %p\n", it.value()->entry_point, addr);
			if (it.value()->entry_point == addr) {
				fprintf(stderr, "isCblEntryPoint - current COBOL module is %s\n", it.value()->name.toLocal8Bit().data());
				*cmi = it.value();
				return true;
			}
		}
	}
	return false;
}

bool GixDebugger::is_first_line_of_preproc_block(CobolModuleInfo* cmi, QString src_file, int line, int* actual_line)
{
	for (auto ppblk : cmi->preprocessed_blocks) {
		QString s1 = QString::fromStdString(ppblk->pp_source_file).toLower();
		QString s2 = src_file.toLower();
		if (QDir::cleanPath(s1) == QDir::cleanPath(s2) && ppblk->pp_gen_start_line == line) {
			*actual_line = ppblk->pp_start_line;
			return true;
		}
	}
	return false;
}


GixDebugger::~GixDebugger()
{
	if (libcob_info)
		delete libcob_info;
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

void GixDebugger::setBuildDirectory(const QString& bd)
{
	build_dir = bd;
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

void GixDebugger::setStdInFile(const QString &f)
{
	stdin_file = f;
}

bool GixDebugger::usesExternalConsole()
{
	return use_external_console;
}

GixDebuggerInterfaceBlock *GixDebugger::getInterfaceBlock()
{
	return if_blk;
}

QString GixDebugger::getWorkingDirectory()
{
	return working_dir;
}

QString GixDebugger::getModuleDirectory()
{
	return module_dir;
}

QString GixDebugger::getBuildDirectory()
{
	return build_dir;
}

DebuggedModuleType GixDebugger::getDebuggedModuleType()
{
	return debuggee_type;
}

void GixDebugger::printMessage(QString msg)
{
	if_blk->debuggerMessage(this, msg, 0);
}

SharedModuleInfo::~SharedModuleInfo()
{

}

UserBreakpoint *UserBreakpoint::createInstance()
{
#if defined(_WIN32)
	return new WinUserBreakpoint();
#elif defined(__linux__)
	return new LinuxUserBreakpoint();
#endif
}
