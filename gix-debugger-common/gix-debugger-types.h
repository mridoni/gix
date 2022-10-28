#pragma once

#include <string>
#include <vector>
#include <map>
#include <stdint.h>

#include "json11.hpp"

struct VariableResolverData;
class SharedModuleInfo;
class CobolModuleInfo;
class GixDebugger;

class UserBreakpoint
{

public:
	std::string key;
	std::string source_file;
	int line = 0;

	void* address = 0x00000000;
	uint8_t orig_instr = 0x00;

	uint8_t automatic = false;

	SharedModuleInfo* owner = nullptr;

	bool isInstalled() { return orig_instr != 0x00; }

	static UserBreakpoint* createInstance();

	virtual bool install() = 0;
	virtual bool uninstall() = 0;
};

#if defined(GIX_DEBUGGER) || defined(GIX_IDE)
class SharedModuleInfo
{
public:
	~SharedModuleInfo();

	GixDebugger* owner = nullptr;

	std::string name;
	std::vector<std::string> source_files;
	std::map<std::string, CobolModuleInfo *> cbl_modules;


	std::string dll_path;
	void* dll_base = nullptr;
	void* dll_symbols = nullptr;
	void* dll_owner_process = nullptr;

	uint64_t base_of_code = 0;	// Used under Windows for DWARF objects
};
#endif

// This will disappear when the standalone debugger will be the only option
// For now this quick hack prevents compilation errors and having to
// refactor this into an external file
#ifndef _GIXPP_BLOCK_
#define _GIXPP_BLOCK_

// we need to replicate this
enum class PreprocessedBlockType {
	ESQL = 1
};

// and this
struct PreprocessedBlockInfo {
	std::string module_name;

	std::string orig_source_file;
	int orig_start_line = 0;
	int orig_end_line = 0;

	std::string pp_source_file;
	int pp_start_line = 0;
	int pp_end_line = 0;
	int pp_gen_start_line = 0;
	int pp_gen_end_line = 0;

	PreprocessedBlockType type;
	std::string command;
};
#endif

class CobolModuleInfo
{
public:
	SharedModuleInfo* owner = nullptr;
	std::string name;
	void* entry_point = nullptr;;
	std::map<std::string, VariableResolverData*> locals;
	std::vector<PreprocessedBlockInfo*> preprocessed_blocks;

	UserBreakpoint* entry_breakpoint = nullptr;
	bool initialized = false;
};

class SourceLineInfo
{
public:
	SharedModuleInfo* module = nullptr;
	std::string source_file;
	int line;

	void* addr = nullptr;
};

// we replicate this here to avoid having to include DataEntry.h
enum class WsEntryType {
	NotSet = 0,	// only for initialization
	Alphabetic = 1,
	Numeric = 2,
	Alphanumeric = 3,
	Group = 99,
	Filler = 98
};

struct VariableDisplayData;

struct VariableResolverData
{
	std::string module_name;
	std::string var_name;
	std::string var_path;

	WsEntryType type = WsEntryType::NotSet;
	int level = 0;
	int display_size = 0;
	int is_signed = 0;
	int decimals = 0;
	std::string format;
	int storage_type = 0;
	std::string storage;
	int occurs = 0;
	std::string redefines;

	// The "root" variable
	std::string base_var_name;

	std::string local_sym_name;
	int local_addr = 0;

	int storage_size = 0;

	VariableDisplayData toDisplayData();
};

enum class DebuggedModuleType
{
	Shared = 1,
	Executable = 2
};

struct VariableData
{
	std::string var_name;
	VariableResolverData *resolver_data;

	uint8_t* data = nullptr;
};

struct VariableDisplayData
{
	~VariableDisplayData();

	std::string var_name;
	std::string module_name;
	std::string var_path;

	WsEntryType type = WsEntryType::NotSet;
	int level = 0;
	int display_size = 0;
	int is_signed = 0;
	int decimals = 0;
	std::string format;
	int storage_type = 0;
	std::string storage;
	int occurs = 0;
	std::string redefines;

	// The "root" variable
	std::string base_var_name;

	std::string local_sym_name;
	int local_addr = 0;

	int storage_size = 0;

	json11::Json to_json() const;

	std::string display_data;
};


enum class DebuggerSessionType {
	NotSet,
	Debug,
	Run,
	Attach
};
