/*
Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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
#include "ISymbolProvider.h"

//#include "dwarf/dwarf++.hh"
#include "elf/elf++.hh"

#include <signal.h>
#include <set>
#include <elf.h>
#include <link.h>
#include <sys/types.h>

#include <string>
#include <thread>

class GixDebuggerLinux;

using addr_t = std::uintptr_t;

struct lib_info {
    lib_info (std::string name, addr_t addr)
        : name{std::move(name)}, addr{addr}
    {}
    const std::string name;
    const addr_t addr;
};

struct stPipeThreadData;

bool operator< (lib_info const& lhs, lib_info const& rhs);


//class PipeReader
//{

//public:
//    PipeReader(GixDebuggerLinux *gd, PipeChannelType ct, int _fd);
//    void stopRunning();

//signals:
//    void dataAvailable(std::string msg);

//public slots:
//    void startReading();

//private:
//    int fd = 0;
//    PipeChannelType channel_type;
//    GixDebuggerLinux *debugger_instance;
//};

class LinuxUserBreakpoint : public UserBreakpoint
{
    // Inherited via UserBreakpoint
    virtual bool install() override;
    virtual bool uninstall() override;
};


class GixDebuggerLinux : public GixDebugger
{
    
public:

    ~GixDebuggerLinux();

	virtual int start() override;
	virtual int stop() override;
	virtual bool step() override;
	virtual bool continue_running() override;
	
	virtual bool getVariables(const std::vector<std::string>& var_names, std::map<std::string, VariableDisplayData>& var_list) override;
	virtual std::string getCurrentCobolModuleName() override;

	virtual void printLastError() override;

	virtual bool readProcessMemory(void *addr, void *bfr, int size) override;	

    virtual void writeToProcess(std::string s) override;
	
    pid_t getPid();

    static GixDebuggerLinux *getInstance();

private:
	virtual void *getSymbolAddress(const char *sym_name) override;	

    ISymbolProvider *sym_provider = nullptr;

    void initialise_load_address();
    bool wait_for_signal();
    siginfo_t get_signal_info();
    void handle_sigtrap(siginfo_t info);
    void set_pc(uint64_t pc);
    uint64_t get_pc();
    uint64_t offset_load_address(uint64_t addr);

    std::string m_prog_name;
    pid_t m_pid;
    uint64_t m_load_address = 0;

    // Console stuff
    std::string outname, errname, inname;
    int fd_out = 0, fd_err = 0, fd_in = 0;
    bool stop_reading_pipes = false;

//    dwarf::dwarf m_dwarf;
    elf::elf m_elf;

    bool __breakpoint_0_hit = false;
    bool is_on_break = false;
    bool is_cpu_single_stepping = false;
    bool is_ld_single_stepping = false;
    CobolModuleInfo *current_cbl_module = nullptr;

//    PipeReader *pipe_reader_out = nullptr;
//    PipeReader *pipe_reader_err = nullptr;
    
    static void PipeReaderThread(stPipeThreadData *data);
    
    std::thread pipe_reader_thread_out;
    std::thread pipe_reader_thread_err;

    void stopReaderThreads();

    void update_libraries();

    uint64_t m_rendezvous_addr = 0;
    std::set<lib_info> m_libraries{};

    void resolve_rendezvous();
    UserBreakpoint *m_linker_breakpoint = nullptr;
    UserBreakpoint *m_entry_breakpoint = nullptr;

    uint64_t read_word(addr_t& addr);
    std::string read_string(addr_t& start_addr);

    template <class T>
    T read_from_inferior(addr_t& addr);

    void handleLibraryLoaded(std::string, void *lib_addr);
    void handleLibraryUnloaded(std::string, void *lib_addr);
    bool processImage(void *imageBase, void *hSym, std::string imageName);

    void deinit_console();

    static GixDebuggerLinux *dbgr_instance;
    
//signals:
//    void startReading();

};

