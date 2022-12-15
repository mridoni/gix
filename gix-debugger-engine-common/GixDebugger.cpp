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
#include "NetworkManager.h"
#include "GixDebugger.h"
#include "libcpputils.h"
#include "gix-debugger-types.h"
#include "comp_helper.h"

#include "debugger-msg-defs.h"
#include "GixDebuggerSessionConfig.h"
#include "DebuggerHostInputMessage.h"
#include "DebuggerHostOutputMessage.h"


#if defined(_WIN32)
#include "GixDebuggerWin.h"
#elif defined(__linux__)
#include "GixDebuggerLinux.h"
#else
#pragma warning Unsupported debugger platform 
#endif

#include <spdlog/spdlog.h>
#include <sstream>
#include <debugger_host_sink.h>

enum class WsEntryStorageType {
	Unknown = 0,	// only for initialization
	Literal = 1,
	Comp3 = 2,
	Comp5 = 3,
	Comp = 4,
	CompX = 5
};

GixDebugger::GixDebugger()
{
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

bool GixDebugger::existsBreakpoint(const std::string &src_file, int ln)
{
	std::string k = src_file + ":" + std::to_string(ln);
	return (map_contains(breakpoints_by_line, k));
}

void GixDebugger::writeToProcess(std::string s)
{
	// Nothing here
}

void GixDebugger::breakPointAdd(UserBreakpoint *bkp)
{
	if (!bkp)
		return;

	if (!vector_contains(breakpoints, bkp))
		breakpoints.push_back(bkp);

#if BITNESS==64
	uint64_t addr = (uint64_t)bkp->address;
#else
	uint32_t addr = (uint32_t)bkp->address;
#endif
	if (!map_contains(breakpoints_by_addr, addr))
		breakpoints_by_addr[addr] = bkp;

	if (!map_contains(breakpoints_by_line, bkp->key))
		breakpoints_by_line.insert({ bkp->key, bkp });
}

#ifdef GIX_IDE
void GixDebugger::setDriver(IDebugDriver* d)
#else
void GixDebugger::setDriver(DebuggerHostDriver* d)
#endif
{
	debug_driver = d;
}

void GixDebugger::breakPointRemove(UserBreakpoint *bkp)
{
	if (!bkp)
		return;

	if (vector_contains(breakpoints, bkp))
		vector_remove(breakpoints, bkp);

#if BITNESS==64
	uint64_t addr = (uint64_t)bkp->address;
#else
	uint32_t addr = (uint32_t)bkp->address;
#endif
	if (map_contains(breakpoints_by_addr, addr))
		map_remove(breakpoints_by_addr, addr);

	if (map_contains(breakpoints_by_line, bkp->key))
		map_remove(breakpoints_by_line, bkp->key);

	delete bkp;
}

UserBreakpoint *GixDebugger::findBreakpointByAddress(void *addr)
{
	if (!addr)
		return nullptr;

	for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
		UserBreakpoint *bkp = *it;
		spdlog::trace("Examining {}@{}", bkp->line, bkp->source_file);
		if (bkp->address == addr)
			return bkp;
	}

	return nullptr;
}

bool GixDebugger::isCobolSourceFile(const std::string &filename)
{
	for (auto t : src_file_types) {
		t = string_replace(t, "*", "");
		if (ends_with(filename, t))
			return true;
	}
	return false;
}



bool GixDebugger::is_debugging_enabled()
{
	return session_type == DebuggerSessionType::Debug || session_type == DebuggerSessionType::Attach;
}

void GixDebugger::getAndResolveUserBreakpoints()
{
	spdlog::trace("Trying to get and resolve user breakpoints");

	std::vector<std::pair<std::string, int>> new_bkps;

	debug_driver->dbgr_client_getBreakpoints(this, new_bkps);

#if _DEBUG
	_DBG_OUT("IDE is resolving user breakpoints (%d user breakpoint(s) received)\n", new_bkps.size());
	for (auto it = new_bkps.begin(); it != new_bkps.end(); ++it) {
		std::pair<std::string, int> newbp = *it;
		_DBG_OUT("IDE says: user breakpoint at %d@%s\n", newbp.second, newbp.first.c_str());
	}
#endif


	// First: find all resolved source line breakpoints that have been deleted from the user list
	for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
		UserBreakpoint *bkp = *it;
		if (bkp->automatic)
			continue;

		auto itt = std::find_if(new_bkps.begin(), new_bkps.end(), [bkp](std::pair<std::string, int> s) { return s.first == bkp->source_file && s.second == bkp->line; });
		if (itt == new_bkps.end()) {
			bkp->automatic = true;
		}
	}

	// Second: we add new breakpoints
	for (auto it = new_bkps.begin(); it != new_bkps.end(); ++it) {
		std::pair<std::string, int> newbp = *it;
		auto itt = std::find_if(breakpoints.begin(), breakpoints.end(), [newbp](UserBreakpoint *b) { return to_lower(filename_clean_path(newbp.first)) == to_lower(filename_clean_path(b->source_file)) && newbp.second == b->line; });

		if (itt == breakpoints.end()) {
			UserBreakpoint *bkp = UserBreakpoint::createInstance();
			bkp->key = newbp.first + ":" + std::to_string(newbp.second);
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

		if (map_contains(source_lines, bkp->key)) {
			spdlog::trace("Resolved breakpoint ({}): {}", bkp->key, bkp->address);
			bkp->address = source_lines[bkp->key]->addr;

			bkp->install();
		}
	}

	spdlog::trace("Processed {} user breakpoints (source automatic breakpoints: {})", new_bkps.size(), breakpoints.size());
}


bool GixDebugger::isCblEntryPoint(void *addr, CobolModuleInfo **cmi)
{
	for (auto mi : shared_modules) {
		for (auto it = mi->cbl_modules.begin(); it != mi->cbl_modules.end(); ++it) {
			spdlog::trace("isCblEntryPoint - entry_point: {} - addr: {}", it->second->entry_point, addr);
			if (it->second->entry_point == addr) {
				spdlog::trace("isCblEntryPoint - current COBOL module is {}", it->second->name);
				*cmi = it->second;
				return true;
			}
		}
	}
	return false;
}

bool GixDebugger::is_first_line_of_preproc_block(CobolModuleInfo* cmi, std::string src_file, int line, int* actual_line)
{
	for (auto ppblk : cmi->preprocessed_blocks) {
		std::string s1 = to_lower(ppblk->pp_source_file);
		std::string s2 = to_lower(src_file);

		if (filename_clean_path(s1) == filename_clean_path(s2) && ppblk->pp_gen_start_line == line) {
			*actual_line = ppblk->pp_start_line;
			return true;
		}
	}
	return false;
}

bool GixDebugger::buildVariableDisplayData(VariableDisplayData& vdd, uint8_t* raw_data)
{
	char* s = nullptr;
	int ndigits = 0;

	if (!raw_data || !vdd.storage_size) {
		vdd.display_data = "{??}";
		return false;
	}

	switch (vdd.type) {

		//case WsEntryType::Group:
		//{
		//	std::string module_name = this->current_cbl_module->name;
		//	CobolModuleMetadata* cmm = GixGlobals::getMetadataManager()->getModuleMetadata(module_name);
		//	if (!cmm) {
		//		vdd.display_data = "{??}";
		//		return false;
		//	}

		//	// We need a DataEntry, since it has more information (children, etc.)
		//	DataEntry* e = cmm->findDefinition(var_path, true);
		//	if (!e) {
		//		vdd.display_data = "{??}";
		//		return false;
		//	}

		//	for (DataEntry* c : e->children) {
		//		formatVariable(c->path, data + c->offset_local, c->type, c->storage_type, c->storage_size, c->display_size, c->is_signed, c->decimals, vres);
		//	}
		//}
		//break;

		case WsEntryType::Filler:
			vdd.display_data = "{??}";
			return false;

		case WsEntryType::NotSet:
			vdd.display_data = "{??}";
			return false;

		default:
			switch (vdd.storage_type) {

				case (int)WsEntryStorageType::Literal:
					vdd.display_data += std::string((char *)raw_data, vdd.storage_size);
					free(s);
					break;

				case (int)WsEntryStorageType::Comp3:
					ndigits = vdd.display_size - ((vdd.is_signed ? 1 : 0) + (vdd.decimals > 0 ? 1 : 0));
					s = CompHelper::comp3_to_display(ndigits, vdd.decimals, vdd.is_signed, raw_data);
					vdd.display_data += std::string(s, vdd.display_size);
					free(s);
					break;

				case (int)WsEntryStorageType::Comp:
				case (int)WsEntryStorageType::Comp5:
					bool is_native_binary = (vdd.storage_type == (int)WsEntryStorageType::Comp5);
					ndigits = vdd.display_size - ((vdd.is_signed ? 1 : 0) + (vdd.decimals > 0 ? 1 : 0));
					s = CompHelper::comp5_to_display(ndigits, vdd.decimals, vdd.is_signed, raw_data, is_native_binary);
					vdd.display_data += std::string(s, vdd.display_size);
					free(s);
					break;

			}
	}

	return true;
}

void GixDebugger::log_local(const std::string& msg, spdlog::level::level_enum l)
{
#if SPDLOG_VERSION >= 10800
	spdlog::details::log_msg m;
	m.level = l;
	m.payload = msg;
	m.logger_name = spdlog::default_logger()->name();
	spdlog::default_logger()->sinks().at(0)->log(m);
#else
	std::string logger_name = spdlog::default_logger()->name();
	spdlog::details::log_msg m(&logger_name, l, msg);
	spdlog::default_logger()->sinks().at(0)->log(m);

#endif
}

GixDebugger::~GixDebugger()
{
	if (libcob_info)
		delete libcob_info;
}

void GixDebugger::setConfiguration(const GixDebuggerSessionConfig& cfg)
{
	this->session_type = cfg.dbgr_host_session_type;
	this->properties = cfg.dbgr_host_properties;
	this->use_external_console = cfg.use_external_console;
	this->attach_pid = cfg.pid;
	this->exepath = cfg.program;
	this->cmd_line_args = vector_join(cfg.program_args, ' ');
	this->working_dir = cfg.working_dir;
	this->build_dir = cfg.build_dir;
	this->module_dir = cfg.module_dir;
	this->debuggee_type = cfg.module_type;
	this->stdin_file = cfg.stdin_file;
	this->environment = cfg.environment;
}

void GixDebugger::setLogger(std::shared_ptr<spdlog::logger> l)
{
	logger = l;
}

void GixDebugger::setProperty(const std::string &k, const std::string &v)
{
	properties[k] = v;
}

bool GixDebugger::setMergeEnvironment(bool b)
{
	merge_env = b;
	return true;
}

bool GixDebugger::setEnvironmentVariable(const std::string& k, const std::string& v)
{
	if (this->target_is_running)
		return false;

	std::string tv;
	if (!merge_env || environment.find(k) != environment.end()) {
		tv = v;
	}
	else {
#ifdef _WIN32
		tv = v + ";" + environment[k];
#else
		tv = v + ":" + environment[k];
#endif

		environment[k] = tv;
	}
	return true;

}

bool GixDebugger::delEnvironmentVariable(const std::string& k)
{
	if (this->target_is_running)
		return false;

	if (environment.find(k) == environment.end()) {
		return true;
	}

	environment.erase(k);
	return true;
}

bool GixDebugger::clearEnvironment()
{
	if (this->target_is_running)
		return false;

	environment.clear();
	return true;
}

void GixDebugger::setProcess(const std::string &path)
{
	exepath = path;
}

void GixDebugger::setWorkingDirectory(const std::string &wd)
{
	working_dir = filename_clean_path(wd);
}

void GixDebugger::setBuildDirectory(const std::string& bd)
{
	build_dir = bd;
}

void GixDebugger::setModuleDirectory(const std::string &md)
{
	module_dir = filename_clean_path(md);
}

void GixDebugger::setProcessArgs(const std::string &args)
{
	cmd_line_args = args;
}

void GixDebugger::setSourceFileTypes(const std::vector<std::string> &_src_file_types)
{
	src_file_types = _src_file_types;
}

void GixDebugger::setSymbolDirectories(const std::vector<std::string> &_sym_dirs)
{
	sym_dirs = _sym_dirs;
}

void GixDebugger::setBreakAtModuleStart(bool b)
{
	break_at_module_start = b;
}

void GixDebugger::setUseExternalConsole(bool b)
{
	use_external_console = b;
}

void GixDebugger::setDebuggedModuleType(DebuggedModuleType b)
{
	debuggee_type = b;
}

void GixDebugger::setStdInFile(const std::string &f)
{
	stdin_file = f;
}

bool GixDebugger::usesExternalConsole()
{
	return use_external_console;
}

std::string GixDebugger::getWorkingDirectory()
{
	return working_dir;
}

std::string GixDebugger::getModuleDirectory()
{
	return module_dir;
}

std::string GixDebugger::getBuildDirectory()
{
	return build_dir;
}

DebuggedModuleType GixDebugger::getDebuggedModuleType()
{
	return debuggee_type;
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
