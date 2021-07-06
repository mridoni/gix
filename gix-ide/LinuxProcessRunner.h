#pragma once

#include <QString>
#include <QStringList>

#include "GixDebugger.h"

class LinuxProcessRunner {

public:
    LinuxProcessRunner(GixDebugger *gd);

    void setProgram(QString p);
    void setArguments(QStringList a);
    void setEnvironment(QStringList e);
    void setWorkingDirectory(QString d);

    bool start(uint64_t *_pid);

    void setStdIn(int s);
    void setStdOut(int s);
    void setStdErr(int s);

private:
    QString program_name;
    QString working_dir;
    QStringList args;
    QStringList environment;

    uint64_t m_pid;

    int p_stdin = 0;
    int p_stdout = 0;
    int p_stderr = 0;

    FILE *xterm_stdout = 0;
    pid_t xterm_pid = 0;

    GixDebugger *debugger_instance = nullptr;
};
