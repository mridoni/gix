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

#include "gix-debugger_global.h"

#include <string>
#include <map>
#include <vector>
#include <utility>
#include <functional>

#include <mutex>

#include "spdlog/spdlog.h"

#ifdef GIX_IDE
#include "IDebugDriver.h"
#else
#include "DebuggerHostDriver.h"
#endif

#include "gix-debugger-types.h"

#ifdef _WIN32
#include <Windows.h>
#include <psapi.h>
#include <Dbghelp.h>
#include <tchar.h>
#endif

#if defined(_MSC_VER) || (defined(__INTEL_COMPILER) && defined(_WIN32))
#if defined(_M_X64)
#define BITNESS 64
#define LONG_SIZE 4
#define DWARF_ADDR_T uint64_t
#else
#define BITNESS 32
#define LONG_SIZE 4
#define DWARF_ADDR_T uint32_t
#endif
#elif defined(__clang__) || defined(__INTEL_COMPILER) || defined(__GNUC__)
#if defined(__x86_64)
#define BITNESS 64
#define DWARF_ADDR_T uint64_t
#else
#define BITNESS 32
#define DWARF_ADDR_T uint32_t
#endif
#if __LONG_MAX__ == 2147483647L
#define LONG_SIZE 4
#define DWARF_ADDR_T uint32_t
#else
#define LONG_SIZE 8
#define DWARF_ADDR_T uint64_t
#endif
#endif

#include "gix-debugger-types.h"
#include "GixDebuggerSessionConfig.h"

/*
	Strict typing for these functions is not actually needed, since we'll be calling them in the debugeee's context,
	but just in case...
*/
typedef const char *(*f_libcob_version_ptr);
typedef const char *(*f_cob_get_field_str_ptr)(const void *f, char *buffer, size_t size);
typedef int (*f_cob_put_field_str_ptr)(const void *dst, const char *str);
typedef void(*f_cob_runtime_error_ptr)(const char *fmt, ...);

struct LibCobInfo
{
	f_libcob_version_ptr f_libcob_version = nullptr;
	f_cob_get_field_str_ptr f_cob_get_field_str = nullptr;
	f_cob_put_field_str_ptr f_cob_put_field_str = nullptr;
	f_cob_runtime_error_ptr f_cob_runtime_error = nullptr;
};

enum class PipeChannelType {
    Unknown,
    Out,
    Err,
    In
};

class GIX_DEBUGGER_EXPORT GixDebugger
{
	friend class ISymbolProvider;

public:
	GixDebugger();
	virtual ~GixDebugger();

	void setConfiguration(const GixDebuggerSessionConfig& cfg);
	void setLogger(std::shared_ptr<spdlog::logger> l);

	void setProperty(const std::string &k, const std::string &v);

	// Environment (must be set before program start)
	bool setMergeEnvironment(bool b);
	bool setEnvironmentVariable(const std::string &k, const std::string& v);
	bool delEnvironmentVariable(const std::string& k);
	bool clearEnvironment();

	void setProcess(const std::string &path);
	void setWorkingDirectory(const std::string &wd);
	void setBuildDirectory(const std::string& bd);
	void setModuleDirectory(const std::string &md);
	void setProcessArgs(const std::string &args);
	void setSourceFileTypes(const std::vector<std::string> &_src_file_types);
	void setSymbolDirectories(const std::vector<std::string> &_sym_dirs);
	void setBreakAtModuleStart(bool b);
	void setUseExternalConsole(bool b);
	void setDebuggedModuleType(DebuggedModuleType b);
	void setStdInFile(const std::string &f);

    bool usesExternalConsole();
	std::string getWorkingDirectory();
	std::string getModuleDirectory();
	std::string getBuildDirectory();

	DebuggedModuleType getDebuggedModuleType();

	virtual int start() = 0;
	virtual int stop() = 0;
	virtual bool step() = 0;
	virtual bool continue_running() = 0;
	
	virtual bool getVariables(const std::vector<std::string>& var_names, std::map<std::string, VariableDisplayData>& var_list) = 0;
	virtual std::string getCurrentCobolModuleName() = 0;

	static GixDebugger *get();

	bool existsBreakpoint(const std::string &src_file, int ln);

	virtual void printLastError() = 0;

	virtual bool readProcessMemory(void *addr, void *bfr, int size) = 0;

    virtual void writeToProcess(std::string s);

	std::map<void *, SourceLineInfo *>		source_lines_by_addr;
	std::map<std::string, SourceLineInfo *> source_lines;
	std::vector<std::string> src_file_types;

	std::vector<UserBreakpoint *> breakpoints;
	std::multimap<std::string, UserBreakpoint *> breakpoints_by_line;

#if BITNESS==64
	std::map<uint64_t, UserBreakpoint *> breakpoints_by_addr;
#else
	std::map<uint32_t, UserBreakpoint *> breakpoints_by_addr;
#endif

	void breakPointRemove(UserBreakpoint *bkp);
	void breakPointAdd(UserBreakpoint *bkp);

	//void setNetworkManager(std::shared_ptr<NetworkManager> nm);
#ifdef GIX_IDE
	void setDriver(IDebugDriver *d);
#else
	void setDriver(DebuggerHostDriver* d);
#endif

protected:

	DebuggerSessionType session_type = DebuggerSessionType::NotSet;

	bool target_is_running = false;

	std::map<std::string, std::string> properties;

	bool break_at_module_start = true;

	DebuggedModuleType debuggee_type = DebuggedModuleType::Shared;
	std::map<std::string, std::string> environment;
    int exit_code = 0;
	uint64_t attach_pid;
	std::string exepath;
	std::string working_dir;
	std::string module_dir;
	std::string cmd_line_args;
	std::string stdin_file;

	CobolModuleInfo* current_cbl_module = nullptr;

	std::vector<SharedModuleInfo *> shared_modules;
	std::vector<std::string> sym_dirs;

	bool is_debugging_enabled();

	void getAndResolveUserBreakpoints();
	UserBreakpoint *findBreakpointByAddress(void *addr);
	bool isCobolSourceFile(const std::string &s);

	bool use_external_console = false;

	LibCobInfo *libcob_info = nullptr;

	std::string build_dir;

    bool isCblEntryPoint(void *addr, CobolModuleInfo **cmi);
	bool is_first_line_of_preproc_block(CobolModuleInfo* cmi, std::string src_file, int line, int* actual_line);
	bool buildVariableDisplayData(VariableDisplayData& vdd, uint8_t* raw_data);

	void log_local(const std::string& msg, spdlog::level::level_enum l);

#ifdef GIX_IDE
	IDebugDriver* debug_driver = nullptr;
#else
	DebuggerHostDriver *debug_driver = nullptr;
#endif

	std::shared_ptr<spdlog::logger> logger;

private:

	virtual void *getSymbolAddress(const char *sym_name) = 0;

	bool merge_env = true;

};
