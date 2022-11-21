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

#include "GixDebuggerLinux.h"
#include "LinuxProcessRunner.h"
#include "DwarfSymbolProvider.h"
#include "registers.hpp"

#include <string>
#include <vector>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <sys/personality.h>
#include <unistd.h>
#include <sstream>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/prctl.h>
#include <sys/uio.h>

#include <filesystem>

#include "libcpputils.h"

#undef GIX_DBGR_USES_DOUBLEFORK
#define LOG_DEBUG(format, ...) fprintf(stderr, format, ##__VA_ARGS__)

bool single_step = false;
std::string last_source_file;
int last_source_line = 0;
UserBreakpoint *last_bkp = nullptr;

GixDebuggerLinux *GixDebuggerLinux::dbgr_instance = nullptr;

struct stPipeThreadData
{
	GixDebuggerLinux *debugger_instance = nullptr;
    int fd = 0;
    PipeChannelType channel_type = PipeChannelType::Unknown;
    bool is_running = false;
};

static struct stPipeThreadData threadDataOut;
static struct stPipeThreadData threadDataErr;

bool operator< (lib_info const &lhs, lib_info const &rhs)
{
    return std::tie(lhs.name, lhs.addr) < std::tie(rhs.name, rhs.addr);
}

GixDebuggerLinux::~GixDebuggerLinux()
{
    if (!inname.empty() && file_exists(inname))
        file_remove(inname);

    if (!outname.empty() && file_exists(outname))
        file_remove(outname);

    if (!errname.empty() && file_exists(errname))
        file_remove(errname);
}

int GixDebuggerLinux::start()
{
    dbgr_instance = this;

    LinuxProcessRunner process(this);
    std::string strEventMessage;

    srand(time(0));

    std::vector<std::string> v;
    split_in_args(v, cmd_line_args, false);
    process.setArguments(v);

    // environment
    std::vector<std::string> env;
    for (auto it = environment.begin(); it != environment.end(); ++it) {
        std::string k = it->first;
        std::string v = it->second;
        env.push_back(k + "=" + v);
    }

    process.setEnvironment(env);

    process.setProgram(exepath);
    process.setWorkingDirectory(this->working_dir);

    // There is no "external console" on Linux
    use_external_console = false;

    spdlog::trace("Setting up console");
    if (!use_external_console) {
        srand(time(0));
        int t = rand();

        
        outname = path_combine({ path_get_temp_path(), "gix_t_out_" + std::to_string(t) });
        if (mkfifo(outname.c_str(), 0666)) {
            spdlog::error("cannot create output pipe");
            return false;
        }
        fd_out = open(outname.c_str(), O_RDWR | O_CREAT | O_SYNC, S_IRUSR | S_IWUSR);
        process.setStdOut(fd_out);

        errname = path_combine({ path_get_temp_path(), "gix_t_err_" + std::to_string(t) });
        if (mkfifo(errname.c_str(), 0666)) {
            spdlog::error("cannot create error pipe");
            return false;
        }
        fd_err = open(errname.c_str(), O_RDWR | O_CREAT | O_SYNC, S_IRUSR | S_IWUSR);
        process.setStdErr(fd_err);

        inname = path_combine({ path_get_temp_path(), "gix_t_in_" + std::to_string(t) });
        if (mkfifo(inname.c_str(), 0666)) {
            spdlog::error("cannot create input pipe");
            return false;
        }
        fd_in = open(inname.c_str(), O_RDWR | O_CREAT | O_SYNC, S_IRUSR | S_IWUSR);
        process.setStdIn(fd_in);

//        reader_thread_out = new QThread();
//        reader_out = new PipeReader(this, PipeChannelType::Out, fd_out);
//        connect(reader_thread_out, &QThread::finished, reader_out, &QObject::deleteLater);
//        connect(this, &GixDebuggerLinux::startReading, reader_out, &PipeReader::startReading, Qt::ConnectionType::QueuedConnection);
//        reader_out->moveToThread(reader_thread_out);

//        reader_thread_err = new QThread();
//        reader_err = new PipeReader(this, PipeChannelType::Err, fd_err);
//        connect(reader_thread_err, &QThread::finished, reader_err, &QObject::deleteLater);
//        connect(this, &GixDebuggerLinux::startReading, reader_err, &PipeReader::startReading, Qt::ConnectionType::QueuedConnection);
//        reader_err->moveToThread(reader_thread_err);

//        reader_thread_out->start();
//        reader_thread_err->start();
        
        threadDataOut = { this, fd_out, PipeChannelType::Out, false };
        pipe_reader_thread_out = std::thread(&GixDebuggerLinux::PipeReaderThread, &threadDataOut);
        
        threadDataErr = { this, fd_err, PipeChannelType::Err, false };
        pipe_reader_thread_err = std::thread(&GixDebuggerLinux::PipeReaderThread, &threadDataErr);        
        
        pipe_reader_thread_out.detach();
        pipe_reader_thread_err.detach();

//        emit startReading();
    }

    uint64_t pid;
    spdlog::trace("Starting process");
    
    if (is_debugging_enabled()) {
        auto fd = open(this->exepath.c_str(), O_RDONLY);
        m_elf = elf::elf{ elf::create_mmap_loader(fd) };
    
        sym_provider = new DwarfSymbolProvider();
        sym_provider->initialize(this, NULL, NULL);
    
        spdlog::trace("Symbols loaded");
        spdlog::trace("Symbols loaded");
    }
    
    if (process.start(&pid)) {
        m_pid = pid;

        if (!is_debugging_enabled()) {
            debug_driver->dbgr_client_debuggerReady(this, exepath);
        }

        debug_driver->dbgr_client_debuggerProcessStarted(this, exepath);

#if GIX_DBGR_USES_DOUBLEFORK
        if (ptrace(PTRACE_ATTACH, m_pid, NULL, NULL) < 0) {
            spdlog::trace("Trace error: {}", strerror(errno));
            debug_driver->dbgr_client_debuggerError(this, 1, "Cannot attach to process");
            return 1;
        }
#endif

        spdlog::trace("Process started ({})", m_pid);

        if (is_debugging_enabled()) {


//            initialise_load_address();

//            if (debuggee_type == DebuggedModuleType::Executable) {
//                int err = 0;
//                void *hSymbols = sym_provider->loadSymbols(this, (void *)m_pid, (void *)m_load_address, exepath, NULL, &err);

//                if (!processImage((void *)m_load_address, hSymbols, exepath)) {
//                    spdlog::trace("Error loading image information for: %s", exepath.c_str());
//                }
//            }
        }

        bool keep_running = true;
        while (keep_running) {

            keep_running = wait_for_signal();
            if (!keep_running)
                break;

            if (!is_cpu_single_stepping)
                ptrace(PTRACE_CONT, m_pid, nullptr, nullptr);
        }
    }
    else {
        spdlog::trace("Cannot start process");
        debug_driver->dbgr_client_debuggerError(this, 1, "Cannot start process");
        printLastError();
        deinit_console();
        return 1;
    }


    deinit_console();

    spdlog::trace("GixDebuggerLinux is exiting");

    return 0;
}

int GixDebuggerLinux::stop()
{
    spdlog::trace("Stop requested");
    if (!kill(m_pid, 0)) {
        exit_code = 0xDEAD;
        kill(m_pid, SIGKILL);
    }
    return false;
}


bool GixDebuggerLinux::step()
{
    spdlog::trace("step invoked");
    single_step = true;
    return true;
}

bool GixDebuggerLinux::continue_running()
{
    spdlog::trace("continue_running invoked");
    single_step = false;
    return true;
}


void GixDebuggerLinux::deinit_console()
{
    if (!use_external_console) {
        spdlog::trace("Stopping reader threads");

        stopReaderThreads();
        
        close(fd_out);
        close(fd_err);

        unlink(outname.c_str());
        unlink(errname.c_str());
        spdlog::trace("Reader threads stopped");
    }
}


bool GixDebuggerLinux::getVariables(const std::vector<std::string>& var_names, std::map<std::string, VariableDisplayData>& var_list)
{
    if (!is_on_break)
        return false;

    if (!var_names.size())
        return true;

    for (std::string var_name : var_names) {
        spdlog::trace("GixDebuggerWin is trying to resolve variable [{}]", var_name);

        // Resolve COBOL variable name to a local symbol in the current stack frame + offset
        if (map_contains(current_cbl_module->locals, var_name)) {

            VariableResolverData *vrd = current_cbl_module->locals[var_name];
            if (map_contains(current_cbl_module->locals, vrd->base_var_name)) {

                VariableResolverData *vrootvar = current_cbl_module->locals[vrd->base_var_name];

                uint64_t addr = (uint64_t)sym_provider->resolveLocalVariableAddress(this, (void *)m_pid, current_cbl_module, m_load_address, vrootvar, vrd);
                
                VariableDisplayData vd = vrd->toDisplayData();
				uint8_t* raw_data = new uint8_t[vrd->storage_size];

				memset(raw_data, 0x00, vrd->storage_size);

                if (!readProcessMemory((void *)addr, raw_data, vrd->storage_size)) {
                    this->printLastError();
                    continue;
                }

                if (!this->buildVariableDisplayData(vd, raw_data)) {
					spdlog::warn("Cannot build displayable data for variable {}", var_name);
				}

				var_list[vd.var_name] = vd;
            }
        }
    }

    return true;
}


std::string GixDebuggerLinux::getCurrentCobolModuleName()
{
    if (current_cbl_module)
        return current_cbl_module->name;

    return std::string();
}


void GixDebuggerLinux::printLastError()
{
    if (!errno)
        spdlog::trace("No error");
    else
        spdlog::trace("Error ({}): {}", errno, strerror(errno));
}

bool GixDebuggerLinux::readProcessMemory(void *src_addr, void *dest_addr, int sz)
{
    iovec local_iov{ dest_addr, (unsigned long)sz };
    iovec remote_iov{ src_addr, (unsigned long)sz };
    if (process_vm_readv(m_pid, &local_iov, 1, &remote_iov, 1, 0) < 0) {
        spdlog::trace("Cannot read memory: ({}) {}", errno, strerror(errno));
        return false;
    }

    return true;
}

// TODO: is this still needed?
void GixDebuggerLinux::writeToProcess(std::string s)
{
    char * bfr = (char *)s.c_str();
    int fd = open(inname.c_str(), O_WRONLY);
    for (int i = 0; i < strlen(bfr); i++) {
        if (bfr[i] == 0x0d) bfr[i] = 0x0a;
    }

    fprintf(stderr, "");

    write(fd, bfr, strlen(bfr));
    close(fd);
}

pid_t GixDebuggerLinux::getPid()
{
    return m_pid;
}

GixDebuggerLinux *GixDebuggerLinux::getInstance()
{
    return dbgr_instance;
}

void *GixDebuggerLinux::getSymbolAddress(const char *sym_name)
{
    return nullptr;
}

uint64_t GixDebuggerLinux::get_pc()
{
    return get_register_value(m_pid, reg::rip);
}

void GixDebuggerLinux::handle_sigtrap(siginfo_t info)
{

    is_cpu_single_stepping = false;

    if (m_entry_breakpoint && m_entry_breakpoint->isInstalled()) {
        if (get_pc() == ((uint64_t)m_entry_breakpoint->address) + 1) {  // + 1

            update_libraries();

            m_entry_breakpoint->uninstall();
            uint64_t new_addr = get_pc() - 1;
            set_pc(new_addr);
            spdlog::trace("Updated libraries, removed entry_breakpoint, moving PC to {:x}", new_addr);
            return;
        }
    }

    switch (info.si_code) {
        //one of these will be set if a breakpoint was hit
        case SI_KERNEL:
        case TRAP_BRKPT:
        {
            spdlog::trace("(2) Received TRAP_BRKPT");

            is_ld_single_stepping = false;

            if (get_pc() == ((uint64_t)m_linker_breakpoint->address) + 1) {
                spdlog::trace("Reached breakpoint at runtime linker ({})", m_linker_breakpoint->address);
                update_libraries();

                set_pc(get_pc() - 1);
                m_linker_breakpoint->uninstall();

                is_cpu_single_stepping = true;
                is_ld_single_stepping = true;
                ptrace(PTRACE_SINGLESTEP, m_pid, nullptr, nullptr);

                return;
            }

            spdlog::trace("Hit breakpoint at address {}", get_pc());

            bool is_cbl_entry_point = isCblEntryPoint((void *)(get_pc() - 1), &current_cbl_module);

            getAndResolveUserBreakpoints();

            UserBreakpoint *bp = findBreakpointByAddress((void *)(get_pc() - 1));
            if (!bp) {	// This will probably lead to a crash
                spdlog::trace("Breakpoint not found");
                break;
            }

            spdlog::trace("Breakpoint is at (source): {}@{}", bp->line, bp->source_file.c_str());

            set_pc(get_pc() - 1);
            bp->uninstall();

            last_bkp = bp;
            last_source_file = bp->source_file;
            last_source_line = bp->line;

            is_cpu_single_stepping = true;
            ptrace(PTRACE_SINGLESTEP, m_pid, nullptr, nullptr);

            return;
        }

        //this will be set if the signal was sent by single stepping
        case TRAP_TRACE:
        {
            spdlog::trace("Single stepped here ({:x})", get_pc());

            is_on_break = true;

            if (is_ld_single_stepping) {
                spdlog::trace("Reinstalling breakpoint at runtime linker ({})", m_linker_breakpoint->address);
                m_linker_breakpoint->install();
                is_on_break = false;
                is_ld_single_stepping = false;
                last_bkp = NULL;
                break;
            }

            if (last_bkp) {
                last_bkp->install();


                if (!last_bkp->automatic || single_step) {
                    spdlog::trace("User breakpoint: must be processed");
                    if (this->source_lines_by_addr.find(last_bkp->address) != source_lines_by_addr.end()) {
                        SourceLineInfo *sli = source_lines_by_addr[last_bkp->address];
                        spdlog::trace("Found breakpoint at 0x{:x} ({}:{})", last_bkp->address, sli->source_file, sli->line);
                        debug_driver->dbgr_client_debuggerBreak(this, current_cbl_module->name, sli->source_file, sli->line);
                    }
                    else {
                        spdlog::trace("Cannot find SourceLineInfo record");
                    }
                }
            }

            is_on_break = false;
            last_bkp = NULL;


            return;
        }

        default:
            std::cout << "Unknown SIGTRAP code " << info.si_code << std::endl;
            return;
    }
}

uint64_t GixDebuggerLinux::offset_load_address(uint64_t addr)
{
    return addr - m_load_address;
}

void GixDebuggerLinux::set_pc(uint64_t pc)
{
    set_register_value(m_pid, reg::rip, pc);
}

void GixDebuggerLinux::initialise_load_address()
{
    //If this is a dynamic library (e.g. PIE)
    if (m_elf.get_hdr().type == elf::et::dyn) {
        //The load address is found in /proc/<pid>/maps
        std::ifstream map("/proc/" + std::to_string(m_pid) + "/maps");

        //Read the first address from the file
        std::string addr;
        std::getline(map, addr, '-');
        m_load_address = strtoull(addr.c_str(), nullptr, 16);
        spdlog::trace("Load address for PID {}: {:x} (raw: {})", m_pid, m_load_address, addr);
    }
}

siginfo_t GixDebuggerLinux::get_signal_info()
{
    siginfo_t info;
    ptrace(PTRACE_GETSIGINFO, m_pid, nullptr, &info);
    return info;
}


bool GixDebuggerLinux::wait_for_signal()
{
    int wait_status;
    auto options = 0;

    spdlog::trace("Waiting for signal");

    waitpid(m_pid, &wait_status, options);

    spdlog::trace("Got signal: {}", wait_status);

//    if (WIFSTOPPED(wait_status))
//        kill(m_pid, SIGSTOP);  /* Signal entire child process to stop */

    if (WIFEXITED(wait_status) || WIFSIGNALED(wait_status)) {
        spdlog::trace("Process exited");

        if (WIFEXITED(wait_status))
            exit_code = WEXITSTATUS(wait_status);

        debug_driver->dbgr_client_debuggerProcessExit(this, exit_code, exepath);
        spdlog::trace(":: EXIT CODE: {}", exit_code);

        return false;
    }

    auto siginfo = get_signal_info();

    // TODO: when attaching to a process (not implemented yet) we will have to check for SIGSTOP, not SIGTRAP
#if GIX_DBGR_USES_DOUBLEFORK
    if (is_debugging_enabled() && !__breakpoint_0_hit && siginfo.si_signo == SIGSTOP) {
#else
    if (is_debugging_enabled() && !__breakpoint_0_hit && siginfo.si_signo == SIGTRAP) {
#endif

        spdlog::trace("Got signal {} ({})", siginfo.si_signo), strsignal(siginfo.si_signo);
        spdlog::trace("Breakpoint 0 hit");

        initialise_load_address();

        uint64_t entry_point = m_elf.get_hdr().entry;
        entry_point += m_load_address;

        m_entry_breakpoint = UserBreakpoint::createInstance();
        m_entry_breakpoint->address = (void *)entry_point;
        m_entry_breakpoint->automatic = true;
        m_entry_breakpoint->install();

        if (debuggee_type == DebuggedModuleType::Executable) {
            int err = 0;
            void *hSymbols = sym_provider->loadSymbols(this, (void *)m_pid, (void *)m_load_address, exepath, NULL, &err);

            if (!processImage((void *)m_load_address, hSymbols, exepath)) {
                spdlog::trace("Error loading image information for: {}", exepath);
            }
        }

        debug_driver->dbgr_client_debuggerReady(this, exepath);

        __breakpoint_0_hit = true;

        resolve_rendezvous();

        spdlog::trace("Breakpoint 0 processing finished");
        return true;
    }

    switch (siginfo.si_signo) {

        case SIGTRAP:
            handle_sigtrap(siginfo);

            break;
        case SIGSEGV:
            exit(1);
            break;

    }

    return true;
}


void GixDebuggerLinux::stopReaderThreads()
{
    stop_reading_pipes = true;

    usleep(500 * 1000);
    
    if (threadDataOut.is_running) {
        spdlog::warn("stdout reader thread is still running, should have terminated");
    }

    if (threadDataErr.is_running) {
        spdlog::warn("stderr reader thread is still running, should have terminated");
    }
}

void GixDebuggerLinux::resolve_rendezvous()
{
    spdlog::trace("Resolving rendezvous address");

    // Rendezvous address is found in the .dynamic section
    auto dyn_section = m_elf.get_section(".dynamic");
    uint64_t addr = dyn_section.get_hdr().addr;

    addr += m_load_address;

    auto val = read_word(addr);

    while (val != 0) {
        //        debug_driver->dbgr_client_debuggerMessage(this, std::string("addr: %1, val: %2").arg(addr).arg(val), 0);

        if (val == DT_DEBUG) {

            uint64_t rend_addr = read_word(addr);

            m_rendezvous_addr = rend_addr;

            r_debug rendezvous = read_from_inferior<r_debug>(rend_addr);
            // The .dynamic section stores a pointer to a function which is called whenever
            // a .so is loaded or unloaded

            m_linker_breakpoint = UserBreakpoint::createInstance();
            m_linker_breakpoint->address = (void *)rendezvous.r_brk;
            m_linker_breakpoint->automatic = true;

            spdlog::trace("Linker breakpoint (rendezvous) address is {:x}", m_linker_breakpoint->address);

            m_linker_breakpoint->install();

            return;
        }

        val = read_word(addr);

    }
    spdlog::trace("ERROR: Could not resolve rendezvous structure");

}

template <class T>
T GixDebuggerLinux::read_from_inferior(addr_t &addr)
{
    T t;
    iovec local_iov{ &t, sizeof(T) };
    iovec remote_iov{ (void *)addr, sizeof(T) };
    if (process_vm_readv(m_pid, &local_iov, 1, &remote_iov, 1, 0) < 0) {
        //spdlog::trace("Cannot read memory: (%d) %s", errno, strerror(errno));
    }
    addr += sizeof(T);
    return t;
}

uint64_t GixDebuggerLinux::read_word(addr_t &addr)
{
    return read_from_inferior<uint64_t>(addr);
}

std::string GixDebuggerLinux::read_string(addr_t &start_addr)
{
    auto addr = start_addr;
    std::string str = "";

    auto word = read_word(addr);
    while (true) {
        auto word_ptr = reinterpret_cast<unsigned char *>(&word);

        for (int i = 0; i < 8; ++i) {
            if (word_ptr[i]) {
                str += word_ptr[i];
            }
            else {
                start_addr = addr + i;
                return str;
            }
        }
        word = read_word(addr);
    }

    return str;
}

void GixDebuggerLinux::handleLibraryLoaded(std::string lib_path, void *lib_addr)
{
    int err = 0;

    if (properties["symformat"] == "dwarf" && !starts_with(lib_path, this->module_dir))
        return;

    void *hSymbols = sym_provider->loadSymbols(this, (void *)m_pid, lib_addr, lib_path, NULL, &err);

    if (!processImage(lib_addr, hSymbols, lib_path)) {
        spdlog::trace("Error loading image information for: {}", lib_path.c_str());
    }
    else {
        m_entry_breakpoint->owner = shared_modules.back();
    }
}

void GixDebuggerLinux::handleLibraryUnloaded(std::string lib_path, void *lib_addr)
{

}

void GixDebuggerLinux::update_libraries()
{
    spdlog::trace("Updating list of loaded libraries");
    if (!m_rendezvous_addr) {
        resolve_rendezvous();
    }

    std::set<lib_info> new_libs{};
    auto rend_addr = m_rendezvous_addr;
    auto rendezvous = read_from_inferior<r_debug>(rend_addr);
    auto link_map_addr = rendezvous.r_map;

    // The link map defines a linked list of .so entries
    while (link_map_addr) {
        auto addr = reinterpret_cast<addr_t>(link_map_addr);
        auto map = read_from_inferior<link_map>(addr);
        auto name_addr = (uint64_t)map.l_name;
        auto name = read_string(name_addr);

        // If the name is empty, it's probably the exe or vdso. Just ignore it.
        if (name != "" && name != "linux-vdso.so.1" && name != "linux-gate.so.1") {
            // the l_name member in link_map does not always return an absolute path
            if (!starts_with(name, "/") && !working_dir.empty() && dir_exists(working_dir)) {
                std::string wd = ends_with(working_dir, "/") ? working_dir.substr(0, working_dir.size() - 1) : working_dir;
                name = filename_absolute_path(wd + "/" + name);
            }
            new_libs.emplace(name, map.l_addr);
        }
        link_map_addr = map.l_next;
    }

    std::vector<lib_info> loaded;
    std::vector<lib_info> unloaded;

    std::set_difference(m_libraries.begin(), m_libraries.end(),
        new_libs.begin(), new_libs.end(),
        std::back_inserter(unloaded));

    std::set_difference(new_libs.begin(), new_libs.end(),
        m_libraries.begin(), m_libraries.end(),
        std::back_inserter(loaded));

    for (auto &&lib : loaded) {
        std::cerr << "Loaded " << lib.name << " at 0x" << std::hex << lib.addr << std::endl;
        handleLibraryLoaded(lib.name, (void *)lib.addr);
        spdlog::trace("Loading library {}", lib.name.c_str());
    }

    for (auto &&lib : unloaded) {
        std::cerr << "Unloaded " << lib.name << " at 0x" << std::hex << lib.addr << std::endl;
        handleLibraryUnloaded(lib.name, (void *)lib.addr);
        spdlog::trace("Unloading library {}", lib.name.c_str());
    }

    m_libraries = new_libs;

    spdlog::trace("The list of loaded libraries has been updated");
}

bool GixDebuggerLinux::processImage(void *imageBase, void *hSym, std::string imageName)
{
    int err = 0;
    if (hSym) {
        spdlog::trace(":: Loaded symbols for {}", imageName);
        if (sym_provider->isGnuCOBOLModule(this, NULL, imageBase, NULL, &err)) {
            spdlog::trace("{} is a GnuCOBOL module", imageName);
            uint64_t base_of_code = m_load_address;
            SharedModuleInfo *mi = sym_provider->extractModuleDebugInfo(this, NULL, imageBase, (void *)hSym, imageName, (void *)base_of_code, &err);

            if (mi) {

                shared_modules.push_back(mi);
                for (auto it = mi->cbl_modules.begin(); it != mi->cbl_modules.end(); ++it) {
                    spdlog::trace("Installing startup hardware breakpoint for module {}", it->second->name);
                    it->second->entry_breakpoint->install();
                    breakPointAdd(it->second->entry_breakpoint);
                }
                getAndResolveUserBreakpoints();
            }
            else
                return false;
        }
        else {
            spdlog::trace("{} is NOT a GnuCOBOL module", imageName);
        }

    }

    return true;
}

//PipeReader::PipeReader(GixDebuggerLinux *gd, PipeChannelType ct, int _fd)
//{
//    debugger_instance = gd;
//    channel_type = ct;
//    fd = _fd;
//}

//void PipeReader::stopRunning()
//{
//    // Nothing to do (for now)
//}

//void PipeReader::startReading()
//{
//    std::string m;

//    char bfr[1024];
//    if (!fd || !debugger_instance) {
//        spdlog::trace("Cannot launch pipe reader thread for channel type {}", (int) channel_type);
//        return;
//    }

//    while (this->debugger_instance->stop_reading_pipes) {
//        int nread = read(fd, bfr, sizeof(bfr));
//        if (nread > 0) {
//            bfr[nread] = '\0';
//            m = std::string(bfr, nread);

//            switch (this->channel_type) {
//                case  PipeChannelType::Out:
//                   debugger_instance->debug_driver->dbgr_client_debuggerStdOutAvailable(debugger_instance, m);    
//                   break;
                   
//                case  PipeChannelType::Err:
//                   debugger_instance->debug_driver->dbgr_client_debuggerStdErrAvailable(debugger_instance, m);    
//                   break;        
                   
//                default:
//                   spdlog::trace("Invalid channel type: ", (int) channel_type); 
//                   break;                       
//            }
//        }
//    }
//}


bool LinuxUserBreakpoint::install()
{
    if (isInstalled()) {
        spdlog::trace("Breakpoint at {:p} for {}@{} is already installed, skipping", this->address, this->line, this->source_file);
        return true;
    }

    if (this->orig_instr)
        return false;

    GixDebuggerLinux *gdlinux = GixDebuggerLinux::getInstance();

    uint64_t data = ptrace(PTRACE_PEEKDATA, gdlinux->getPid(), this->address, nullptr);
    if ((int64_t)data == -1)
        spdlog::trace("Error ({}) {}", errno, strerror(errno));
    this->orig_instr = static_cast<uint8_t>(data & 0xff); //save bottom byte
    uint64_t int3 = 0xcc;
    uint64_t data_with_int3 = ((data & ~0xff) | int3); //set bottom byte to 0xcc
    spdlog::trace("Installing breakpoint at {:x} - old data: {:x}, new data : {:x}, saved data: {:x}", this->address, data, data_with_int3, this->orig_instr);
    ptrace(PTRACE_POKEDATA, gdlinux->getPid(), this->address, data_with_int3);
    return true;
}

bool LinuxUserBreakpoint::uninstall()
{
    if (!isInstalled()) {
        spdlog::trace("Breakpoint at {:x} for {}@{} is not installed, skipping", this->address, this->line, this->source_file);
        return true;
    }

    GixDebuggerLinux *gdlinux = GixDebuggerLinux::getInstance();

    uint64_t data = ptrace(PTRACE_PEEKDATA, gdlinux->getPid(), this->address, nullptr);
    uint64_t restored_data = ((data & ~0xff) | this->orig_instr);
    spdlog::trace("Removing breakpoint at {:x} - old data: {:x}, new data : {:x}, orig_instr: {:x}", this->address, data, restored_data, this->orig_instr);
    ptrace(PTRACE_POKEDATA, gdlinux->getPid(), this->address, restored_data);
    this->orig_instr = 0x00;
    return true;
}

void GixDebuggerLinux::PipeReaderThread(stPipeThreadData *data)
{
    std::string m;

    char bfr[1024];
    if (!data->fd || !data->debugger_instance) {
        spdlog::trace("Cannot launch pipe reader thread for channel type {}", (int) data->channel_type);
        return;
    }


    data->is_running = true;
    
    // Initialize file descriptor sets
    fd_set read_fds, write_fds, except_fds;
    FD_ZERO(&read_fds);
    FD_ZERO(&write_fds);
    FD_ZERO(&except_fds);
    FD_SET(data->fd, &read_fds);
    
    // Set timeout to 1.0 seconds
    struct timeval timeout;
    timeout.tv_sec = 0;
    timeout.tv_usec = 250 * 1000;
    
    while (data->debugger_instance->stop_reading_pipes) {
        
        // Wait for input to become ready or until the time out; the first parameter is
        // 1 more than the largest file descriptor in any of the sets
        int p = select(data->fd + 1, &read_fds, &write_fds, &except_fds, &timeout);
        
        if (p == 1) {
            int nread = read(data->fd, bfr, sizeof(bfr));
            if (nread > 0) {
                bfr[nread] = '\0';
                m = std::string(bfr, nread);
    
                switch (data->channel_type) {
                    case  PipeChannelType::Out:
                       data->debugger_instance->debug_driver->dbgr_client_debuggerStdOutAvailable(data->debugger_instance, m);    
                       break;
                       
                    case  PipeChannelType::Err:
                       data->debugger_instance->debug_driver->dbgr_client_debuggerStdErrAvailable(data->debugger_instance, m);    
                       break;        
                       
                    default:
                       spdlog::trace("Invalid channel type: ", (int) data->channel_type); 
                       break;                       
                }
            }
        }
        else
        {
            if (p == -1)
                break;
        }        
    }
    data->is_running = false;
}
