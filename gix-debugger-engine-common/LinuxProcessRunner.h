#pragma once

#include <string>
#include <vector>

#include "GixDebugger.h"

class LinuxProcessRunner {

public:
    LinuxProcessRunner(GixDebugger *gd);

    void setProgram(std::string p);
    void setArguments(std::vector<std::string> a);
    void setEnvironment(std::vector<std::string> e);
    void setWorkingDirectory(std::string d);

    bool start(uint64_t *_pid);

    void setStdIn(int s);
    void setStdOut(int s);
    void setStdErr(int s);

private:
    std::string program_name;
    std::string working_dir;
    std::vector<std::string> args;
    std::vector<std::string> environment;

    uint64_t m_pid;

    int p_stdin = 0;
    int p_stdout = 0;
    int p_stderr = 0;

    FILE *xterm_stdout = 0;
    pid_t xterm_pid = 0;

    GixDebugger *debugger_instance = nullptr;
};
