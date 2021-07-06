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
#include "utils.h"
#include "LinuxProcessRunner.h"
#include "DwarfSymbolProvider.h"
#include "PathUtils.h"
#include "registers.hpp"
#include "GixGlobals.h"
#include "SysUtils.h"

#include <QString>
#include <QFile>
#include <QDir>

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

bool single_step = false;
QString last_source_file;
int last_source_line = 0;
UserBreakpoint *last_bkp = nullptr;

bool operator< (lib_info const &lhs, lib_info const &rhs)
{
    return std::tie(lhs.name, lhs.addr) < std::tie(rhs.name, rhs.addr);
}

GixDebuggerLinux::~GixDebuggerLinux()
{
    if (!inname.isEmpty() && QFile::exists(inname))
        QFile::remove(inname);

    if (!outname.isEmpty() && QFile::exists(outname))
        QFile::remove(outname);

    if (!errname.isEmpty() && QFile::exists(errname))
        QFile::remove(errname);
}

int GixDebuggerLinux::start()
{
    LinuxProcessRunner process(this);
    QString strEventMessage;

    srand(time(0));

    process.setArguments(cmd_line_args.split(' '));

    // environment
    QStringList env;
    for (int i = 0; i < environment.size(); i++) {
        QString k = environment.keys().at(i);
        QString v = environment.value(k);
        env.append(k + "=" + v);
    }

    process.setEnvironment(env);

    process.setProgram(exepath);
    process.setWorkingDirectory(this->working_dir);

    // There is no "external console" on Linux
    use_external_console = false;

    if_blk->debuggerMessage(this, "Setting up console", 0);
    LOG_DEBUG("Setting up console\n");
    if (!use_external_console) {
        srand(time(0));
        int t = rand();

        outname = PathUtils::combine(QDir::tempPath(), "gix_t_out_" + QString::number(t));
        if (mkfifo(outname.toLocal8Bit().constData(), 0666)) {
            LOG_DEBUG("no out pipe\n");
            return false;
        }
        fd_out = open(outname.toLocal8Bit().constData(), O_RDWR | O_CREAT | O_SYNC, S_IRUSR | S_IWUSR);
        process.setStdOut(fd_out);

        errname = PathUtils::combine(QDir::tempPath(), "gix_t_err_" + QString::number(t));
        if (mkfifo(errname.toLocal8Bit().constData(), 0666)) {
            //LOG_DEBUG("no err pipe\n");
            return false;
        }
        fd_err = open(errname.toLocal8Bit().constData(), O_RDWR | O_CREAT | O_SYNC, S_IRUSR | S_IWUSR);
        process.setStdErr(fd_err);

        inname = PathUtils::combine(QDir::tempPath(), "gix_t_in_" + QString::number(t));
        if (mkfifo(inname.toLocal8Bit().constData(), 0666)) {
            //LOG_DEBUG("no err pipe\n");
            return false;
        }
        fd_in = open(inname.toLocal8Bit().constData(), O_RDWR | O_CREAT | O_SYNC, S_IRUSR | S_IWUSR);
        process.setStdIn(fd_in);

        reader_thread_out = new QThread();
        reader_out = new PipeReader(this, fd_out);
        connect(reader_thread_out, &QThread::finished, reader_out, &QObject::deleteLater);
        connect(this, &GixDebuggerLinux::startReading, reader_out, &PipeReader::startReading, Qt::ConnectionType::QueuedConnection);
        reader_out->moveToThread(reader_thread_out);

        reader_thread_err = new QThread();
        reader_err = new PipeReader(this, fd_err);
        connect(reader_thread_err, &QThread::finished, reader_err, &QObject::deleteLater);
        connect(this, &GixDebuggerLinux::startReading, reader_err, &PipeReader::startReading, Qt::ConnectionType::QueuedConnection);
        reader_err->moveToThread(reader_thread_err);

        reader_thread_out->start();
        reader_thread_err->start();

        emit startReading();
    }


    uint64_t pid;
    LOG_DEBUG("Starting process\n");
    if (process.start(&pid)) {
        m_pid = pid;

        if (!is_debugging_enabled) {
            if_blk->debuggerReady(this, exepath);
        }

        if_blk->debuggerProcessStarted(this, exepath);

#if GIX_DBGR_USES_DOUBLEFORK
        if (ptrace(PTRACE_ATTACH, m_pid, NULL, NULL) < 0) {
            LOG_DEBUG("Trace error: %s\n", strerror(errno));
            if_blk->debuggerError(this, 1, "Cannot attach to process");
            return 1;
        }
#endif

        if_blk->debuggerMessage(this, QString("Process started (%1)").arg(m_pid), 0);
        LOG_DEBUG("Process started (%d)\n", m_pid);

        if (is_debugging_enabled) {
            auto fd = open(this->exepath.toLocal8Bit().data(), O_RDONLY);
            m_elf = elf::elf{ elf::create_mmap_loader(fd) };

            sym_provider = new DwarfSymbolProvider();
            sym_provider->initialize(this, NULL, NULL);

            if_blk->debuggerMessage(this, "Symbols loaded\n", 0);
            LOG_DEBUG("Symbols loaded\n");

//            initialise_load_address();

//            if (debuggee_type == DebuggedModuleType::Executable) {
//                int err = 0;
//                void *hSymbols = sym_provider->loadSymbols(this, (void *)m_pid, (void *)m_load_address, exepath, NULL, &err);

//                if (!processImage((void *)m_load_address, hSymbols, exepath)) {
//                    LOG_DEBUG("Error loading image information for: %s\n", exepath.toLocal8Bit().data());
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
        LOG_DEBUG("Cannot start process\n");
        if_blk->debuggerError(this, 1, "Cannot start process");
        printLastError();
        deinit_console();
        return 1;
    }


    deinit_console();

    LOG_DEBUG("GixDebuggerLinux is exiting\n");

    return 0;
}

int GixDebuggerLinux::stop()
{
    LOG_DEBUG("Stop requested");
    if (!kill(m_pid, 0)) {
        exit_code = 0xDEAD;
        kill(m_pid, SIGKILL);
    }
    return false;
}


bool GixDebuggerLinux::step()
{
    LOG_DEBUG("step invoked\n");
    single_step = true;
    return true;
}

bool GixDebuggerLinux::continue_running()
{
    LOG_DEBUG("continue_running invoked\n");
    single_step = false;
    return true;
}


void GixDebuggerLinux::deinit_console()
{
    if (!use_external_console) {
        LOG_DEBUG("Stopping reader threads\n");

        stopReaderThreads();

        close(fd_out);
        close(fd_err);

        unlink(outname.toLocal8Bit().data());
        unlink(errname.toLocal8Bit().data());
        LOG_DEBUG("Reader threads stopped\n");
    }
}


bool GixDebuggerLinux::getVariables(QList<VariableData *> var_list)
{
    if (!is_on_break)
        return false;

    if (!var_list.size())
        return true;

    unsigned long frame_ptr = get_register_value(m_pid, reg::rdi);

    for (VariableData *vd : var_list) {

        // Resolve COBOL variable name to a local symbol in the current stack frame + offset
        if (current_cbl_module->locals.contains(vd->var_name)) {

            VariableResolverData *vrd = current_cbl_module->locals[vd->var_name];
            if (current_cbl_module->locals.contains(vrd->base_var_name)) {

                VariableResolverData *vrootvar = current_cbl_module->locals[vrd->base_var_name];

                uint64_t addr = (uint64_t)sym_provider->resolveLocalVariableAddress(this, (void *)m_pid, current_cbl_module, m_load_address, vrootvar, vrd);

                vd->data = new uint8_t[vrd->storage_len];
                vd->resolver_data = vrd;

                memset(vd->data, 0x00, vrd->storage_len);

                if (!readProcessMemory((void *)addr, vd->data, vrd->storage_len)) {
                    this->printLastError();
                    continue;
                }

#if _DEBUG
                vd->data[vrd->storage_len - 1] = 0;
                LOG_DEBUG("%s : [%s]\n", vd->var_name.toLocal8Bit().constData(), vd->data);
#endif
            }
        }
    }

    return true;
}


QString GixDebuggerLinux::getCurrentCobolModuleName()
{
    if (current_cbl_module)
        return current_cbl_module->name;

    return QString();
}


void GixDebuggerLinux::printLastError()
{
    if (!errno)
        LOG_DEBUG("No error\n");
    else
        LOG_DEBUG("Error (%d): %s\n", errno, strerror(errno));
}


void GixDebuggerLinux::removeHardwareBreakpoint(UserBreakpoint *bkp)
{
    uint64_t data = ptrace(PTRACE_PEEKDATA, m_pid, bkp->address, nullptr);
    uint64_t restored_data = ((data & ~0xff) | bkp->orig_instr);
    LOG_DEBUG("Removing breakpoint at %p - old data: %p, new data : %p, orig_instr: %02x\n", bkp->address, data, restored_data, bkp->orig_instr);
    ptrace(PTRACE_POKEDATA, m_pid, bkp->address, restored_data);
    bkp->orig_instr = 0x00;
}


bool GixDebuggerLinux::installHardwareBreakpoint(UserBreakpoint *bkp)
{
    if (bkp->orig_instr)
        return false;

    uint64_t data = ptrace(PTRACE_PEEKDATA, m_pid, bkp->address, nullptr);
    if ((int64_t)data == -1)
        LOG_DEBUG("Error (%d) %s\n", errno, strerror(errno));
    bkp->orig_instr = static_cast<uint8_t>(data & 0xff); //save bottom byte
    uint64_t int3 = 0xcc;
    uint64_t data_with_int3 = ((data & ~0xff) | int3); //set bottom byte to 0xcc
    LOG_DEBUG("Installing breakpoint at %p - old data: %p, new data : %p, saved data: %02x\n", bkp->address, data, data_with_int3, bkp->orig_instr);
    ptrace(PTRACE_POKEDATA, m_pid, bkp->address, data_with_int3);
    return true;
}


bool GixDebuggerLinux::readProcessMemory(void *src_addr, void *dest_addr, int sz)
{
    iovec local_iov{ dest_addr, (unsigned long)sz };
    iovec remote_iov{ src_addr, (unsigned long)sz };
    if (process_vm_readv(m_pid, &local_iov, 1, &remote_iov, 1, 0) < 0) {
        LOG_DEBUG("Cannot read memory: (%d) %s\n", errno, strerror(errno));
        return false;
    }

    return true;
}

void GixDebuggerLinux::writeToProcess(QString s)
{
    char * bfr = s.toLocal8Bit().data();
    int fd = open(inname.toLocal8Bit().constData(), O_WRONLY);
    for (int i = 0; i < strlen(bfr); i++) {
        if (bfr[i] == 0x0d) bfr[i] = 0x0a;
    }

    fprintf(stderr, "\n");

    write(fd, bfr, strlen(bfr));
    close(fd);
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
            //m_entry_breakpoint.disable();
            removeHardwareBreakpoint(m_entry_breakpoint);
            uint64_t new_addr = get_pc() - 1;
            set_pc(new_addr);
            LOG_DEBUG("Updated libraries, removed entry_breakpoint, moving PC to %p\n", new_addr);
            return;
        }
    }

    switch (info.si_code) {
        //one of these will be set if a breakpoint was hit
        case SI_KERNEL:
        case TRAP_BRKPT:
        {
            LOG_DEBUG("(2) Received TRAP_BRKPT\n");

            if (get_pc() == ((uint64_t)m_linker_breakpoint->address) + 1) {
                LOG_DEBUG("Reached breakpoint at runtime linker\n");
                update_libraries();
                return;
            }

            LOG_DEBUG("Hit breakpoint at address %p\n", get_pc());

            bool is_cbl_entry_point = isCblEntryPoint((void *)(get_pc() - 1), &current_cbl_module);

            getAndResolveUserBreakpoints();

            UserBreakpoint *bp = findBreakpointByAddress((void *)(get_pc() - 1));
            if (!bp) {	// This will probably lead to a crash
                LOG_DEBUG("Breakpoint not found\n");
                break;
            }

            LOG_DEBUG("Breakpoint is at (source): %d@%s\n", bp->line, bp->source_file.toLocal8Bit().data());

            set_pc(get_pc() - 1);

            removeHardwareBreakpoint(bp);

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
            //        if (!is_debugging_enabled)
            //            break;

            LOG_DEBUG("Single stepped here\n");

            is_on_break = true;

            if (last_bkp) {

                installHardwareBreakpoint(last_bkp);


                if (!last_bkp->automatic || single_step) {
                    LOG_DEBUG("User breapoint: must be processed\n");
                    if (this->source_lines_by_addr.find(last_bkp->address) != source_lines_by_addr.end()) {
                        SourceLineInfo *sli = source_lines_by_addr[last_bkp->address];
                        QString msg = QString("Found breakpoint at %1 (%2:%3)\n").arg(SysUtils::toHexString((uint64_t)last_bkp->address)).arg(sli->source_file).arg(sli->line);
                        if_blk->debuggerMessage(this, msg, 0);
                        if_blk->debuggerBreak(this, current_cbl_module->name, sli->source_file, sli->line);
                    }
                    else {
                        LOG_DEBUG("Cannot find SourceLineInfo record\n");
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
        bool b = false;
        std::string addr;
        std::getline(map, addr, '-');
        m_load_address = QString::fromStdString(addr).toULong(&b, 16);
        LOG_DEBUG("Load addressfor PID %d: 0x%016lx (raw: %s)\n",
                  m_pid, m_load_address, addr.c_str());
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

    LOG_DEBUG("Waiting for signal\n");

    waitpid(m_pid, &wait_status, options);

    if (WIFEXITED(wait_status) || WIFSIGNALED(wait_status)) {
        LOG_DEBUG("Process exited\n");

        if (WIFEXITED(wait_status))
            exit_code = WEXITSTATUS(wait_status);

        if_blk->debuggerProcessExit(this, exit_code, exepath);
        if_blk->debuggerMessage(this, ":: EXIT CODE: " + QString::number(exit_code), 0);

        LOG_DEBUG(":: EXIT CODE: %d\n", exit_code);

        return false;
    }


    auto siginfo = get_signal_info();

    // TODO: when attaching to a process (not implemented yet) we will have to check for SIGSTOP, not SIGTRAP
#if GIX_DBGR_USES_DOUBLEFORK
    if (is_debugging_enabled && !__breakpoint_0_hit && siginfo.si_signo == SIGSTOP) {
#else
    if (is_debugging_enabled && !__breakpoint_0_hit && siginfo.si_signo == SIGTRAP) {
#endif
        QString msg = QString("Got signal %1 (%2)").arg(siginfo.si_signo).arg(strsignal(siginfo.si_signo));
        LOG_DEBUG("%s\n", msg.toLocal8Bit().constData());
        LOG_DEBUG("Breakpoint 0 hit\n");

        initialise_load_address();

        if (debuggee_type == DebuggedModuleType::Executable) {
            int err = 0;
            void *hSymbols = sym_provider->loadSymbols(this, (void *)m_pid, (void *)m_load_address, exepath, NULL, &err);

            if (!processImage((void *)m_load_address, hSymbols, exepath)) {
                LOG_DEBUG("Error loading image information for: %s\n", exepath.toLocal8Bit().data());
            }
        }

        uint64_t entry_point = m_elf.get_hdr().entry;
        entry_point += m_load_address;

        m_entry_breakpoint = new UserBreakpoint();
        m_entry_breakpoint->address = (void *)entry_point;
        m_entry_breakpoint->automatic = true;

        installHardwareBreakpoint(m_entry_breakpoint);

        if_blk->debuggerReady(this, exepath);

        __breakpoint_0_hit = true;

        resolve_rendezvous();

        LOG_DEBUG("Breakpoint 0 processing finished\n");
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
    if (reader_out)
        reader_out->stopRunning();

    if (reader_err)
        reader_err->stopRunning();

    if (reader_thread_out) {
        reader_thread_out->terminate();
    }

    if (reader_thread_err) {
        reader_thread_err->terminate();
    }
}

void GixDebuggerLinux::resolve_rendezvous()
{
    LOG_DEBUG("Resolving rendezvous address\n");

    // Rendezvous address is found in the .dynamic section
    auto dyn_section = m_elf.get_section(".dynamic");
    uint64_t addr = dyn_section.get_hdr().addr;

    addr += m_load_address;

    auto val = read_word(addr);

    while (val != 0) {
        //        if_blk->debuggerMessage(this, QString("addr: %1, val: %2").arg(addr).arg(val), 0);

        if (val == DT_DEBUG) {

            uint64_t rend_addr = read_word(addr);

            m_rendezvous_addr = rend_addr;

            r_debug rendezvous = read_from_inferior<r_debug>(rend_addr);
            // The .dynamic section stores a pointer to a function which is called whenever
            // a .so is loaded or unloaded

            m_linker_breakpoint = new UserBreakpoint();
            m_linker_breakpoint->address = (void *)rendezvous.r_brk;
            m_linker_breakpoint->automatic = true;

            LOG_DEBUG("Linker breakpoint (rendezvous) address is %p\n", m_linker_breakpoint->address);
            //m_linker_breakpoint.enable();

            installHardwareBreakpoint(m_linker_breakpoint);

            return;
        }

        val = read_word(addr);

    }
    LOG_DEBUG("ERROR: Could not resolve rendezvous structure\n");

}

template <class T>
T GixDebuggerLinux::read_from_inferior(addr_t &addr)
{
    T t;
    iovec local_iov{ &t, sizeof(T) };
    iovec remote_iov{ (void *)addr, sizeof(T) };
    if (process_vm_readv(m_pid, &local_iov, 1, &remote_iov, 1, 0) < 0) {
        //LOG_DEBUG("Cannot read memory: (%d) %s\n", errno, strerror(errno));
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

void GixDebuggerLinux::handleLibraryLoaded(QString lib_path, void *lib_addr)
{
    int err = 0;

    if (properties["symformat"] == "dwarf" && !lib_path.startsWith(this->module_dir))
        return;

    void *hSymbols = sym_provider->loadSymbols(this, (void *)m_pid, lib_addr, lib_path, NULL, &err);

    if (!processImage(lib_addr, hSymbols, lib_path)) {
        LOG_DEBUG("Error loading image information for: %s\n", lib_path.toLocal8Bit().data());
    }
}

void GixDebuggerLinux::handleLibraryUnloaded(QString lib_path, void *lib_addr)
{

}

void GixDebuggerLinux::update_libraries()
{
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
        if (name != "") {
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
        handleLibraryLoaded(QString::fromStdString(lib.name), (void *)lib.addr);
        //if_blk->debuggerMessage(this, QString("Loaded %1").arg(QString::fromStdString(lib.name)), 0);
    }

    for (auto &&lib : unloaded) {
        std::cerr << "Unloaded " << lib.name << " at 0x" << std::hex << lib.addr << std::endl;
        handleLibraryUnloaded(QString::fromStdString(lib.name), (void *)lib.addr);
        //if_blk->debuggerMessage(this, QString("Unloaded %1").arg(QString::fromStdString(lib.name)), 0);
    }

    m_libraries = new_libs;
}

bool GixDebuggerLinux::processImage(void *imageBase, void *hSym, QString imageName)
{
    int err = 0;
    if (hSym) {
        if_blk->debuggerMessage(this, ":: Loaded symbols for " + imageName, 0);
        if (sym_provider->isGnuCOBOLModule(this, NULL, imageBase, NULL, &err)) {
            if_blk->debuggerMessage(this, imageName + " is a GnuCOBOL module", 0);
            uint64_t base_of_code = m_load_address;
            SharedModuleInfo *mi = sym_provider->extractModuleDebugInfo(this, NULL, imageBase, (void *)hSym, imageName, (void *)base_of_code, &err);

            if (mi) {

                shared_modules.push_back(mi);
                for (auto it = mi->cbl_modules.begin(); it != mi->cbl_modules.end(); ++it) {
                    if_blk->debuggerMessage(this, QString("Installing startup hardware breakpoint for module " + it.value()->name), 0);
                    installHardwareBreakpoint(it.value()->entry_breakpoint);
                    breakpoints[it.value()->entry_breakpoint->key] = it.value()->entry_breakpoint;
                }
            }
            else
                return false;
        }
        else {
            if_blk->debuggerMessage(this, imageName + " is NOT a GnuCOBOL module", 0);
        }

    }

    return true;
}

PipeReader::PipeReader(GixDebuggerLinux *gd, int _fd)
{
    debugger_instance = gd;
    fd = _fd;
}

void PipeReader::stopRunning()
{
    keep_running = false;
}

void PipeReader::startReading()
{
    QString m;

    char bfr[1024];
    if (!fd)
        return;

    keep_running = true;

    while (keep_running) {
        int nread = read(fd, bfr, sizeof(bfr));
        if (nread > 0) {
            bfr[nread] = '\0';
            m = QString::fromLocal8Bit(bfr, nread);

            debugger_instance->getInterfaceBlock()->debuggerStdOutAvailable(debugger_instance, m);
        }
    }
}

