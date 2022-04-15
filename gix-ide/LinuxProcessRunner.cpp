#include "LinuxProcessRunner.h"
#include "SysUtils.h"

#include <QFile>
#include <QDir>

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include <paths.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <sys/ptrace.h>

#undef GIX_DBGR_USES_DOUBLEFORK
#define LOG_DEBUG(format, ...) fprintf(stderr, format, ##__VA_ARGS__)

#define LIBSTDBUF   "/usr/lib/x86_64-linux-gnu/coreutils/libstdbuf.so"

pid_t find_pid(QString prgname, QString astring);

FILE *c_popen(const char *program, const char *type, pid_t *the_pid);
int c_pclose(FILE *iop);

LinuxProcessRunner::LinuxProcessRunner(GixDebugger *gd)
{
    debugger_instance = gd;
}

void LinuxProcessRunner::setProgram(QString p)
{
    program_name = p;
}

void LinuxProcessRunner::setArguments(QStringList a)
{
    args = a;
}

void LinuxProcessRunner::setEnvironment(QStringList e)
{
    environment = e;
}

void LinuxProcessRunner::setWorkingDirectory(QString d)
{
    working_dir = d;
}

#if GIX_DBGR_USES_DOUBLEFORK
bool LinuxProcessRunner::start(uint64_t *pid)
{
    int startedPipe[2];
     if (pipe2(startedPipe, O_CLOEXEC) != 0) {
         LOG_DEBUG("Pipe error (1)\n");
         return false;
     }
     // To communicate the pid of the child
     int pidPipe[2];

     if (pipe2(pidPipe, O_CLOEXEC) != 0) {
         close(startedPipe[0]);
         close(startedPipe[1]);
         LOG_DEBUG("Pipe error (2)\n");
         return false;
     }

     /* verify/setup I/O channels
     on error:
        safe_close(pidPipe[0]);
        safe_close(pidPipe[1]);
        safe_close(startedPipe[0]);
        safe_close(startedPipe[1]);
        return false;
     */

     if (QFile::exists(LIBSTDBUF)) {
         environment.append(QString("LD_PRELOAD=%1").arg(LIBSTDBUF));
         environment.append("_STDBUF_O=0");
     }

     pid_t childPid = fork();

     if (childPid == 0) {
         struct sigaction noaction;
         memset(&noaction, 0, sizeof(noaction));
         noaction.sa_handler = SIG_IGN;
         ::sigaction(SIGPIPE, &noaction, 0);
         ::setsid();

         close(startedPipe[0]);
         close(pidPipe[0]);

         pid_t doubleForkPid = fork();

         if (doubleForkPid == 0) {
             close(pidPipe[1]);

             // setup I/O (if needed)

             char *progname = strdup(this->program_name.toLocal8Bit().data());

             // arguments

             char *argv[args.size() + 2];
             argv[0] = progname;
             for (int i = 0; i < args.size(); i++)
                 argv[i + 1] = strdup(args.at(i).toLocal8Bit().data());

             argv[args.size() + 1] = 0;

              // environment
             char *env[environment.size() + 1];
             for (int i = 0; i < environment.size(); i++) {
                 QString k = environment.at(i);
                 env[i] = strdup(k.toLocal8Bit().data());
             }

             env[environment.size()] = 0;

             if (!debugger_instance->usesExternalConsole()) {
                 if (p_stdin) {
                    dup2(p_stdin, STDIN_FILENO);
                    close (p_stdin);
                 }

                 if (p_stdout) {
                    dup2(p_stdout, STDOUT_FILENO);
                    close (p_stdout);
                 }

                 if (p_stderr) {
                    dup2(p_stderr, STDERR_FILENO);
                    close (p_stderr);
                 }
             }
             else {
                // Nothing, this should never happen
             }

             //chdir(working_dir.toLocal8Bit().data());

             execve(progname, argv, env);

             struct sigaction noaction;
             memset(&noaction, 0, sizeof(noaction));
             noaction.sa_handler = SIG_IGN;
             ::sigaction(SIGPIPE, &noaction, 0);

             // '\1' means execv failed
             char c = '\1';
             write(startedPipe[1], &c, 1);
             close(startedPipe[1]);

             LOG_DEBUG("ERROR: %d\n", errno);

             c_pclose(xterm_stdout);
             if (xterm_pid)
                kill (xterm_pid, SIGKILL);

             ::_exit(1);

         } else if (doubleForkPid == -1) {
             struct sigaction noaction;
             memset(&noaction, 0, sizeof(noaction));
             noaction.sa_handler = SIG_IGN;
             ::sigaction(SIGPIPE, &noaction, 0);

             // '\2' means internal error
             char c = '\2';
             write(startedPipe[1], &c, 1);
         }

         char c = '\0';
         write(startedPipe[1], &c, 1);
         close(startedPipe[1]);

         write(pidPipe[1], (const char *)&doubleForkPid, sizeof(pid_t));

         //::_exit(1);
     }

     close(startedPipe[1]);
     close(pidPipe[1]);

     if (childPid == -1) {
         close(startedPipe[0]);
         close(pidPipe[0]);
         LOG_DEBUG("Pipe error (3): %d\n", errno);
         return false;
     }

     char reply = '\0';
     int startResult = read(startedPipe[0], &reply, 1);
     int result;

     close(startedPipe[0]);

     waitpid(childPid, &result, 0);

     bool success = (startResult != -1 && reply == '\0');
     LOG_DEBUG("startResult: %d, reply: %s, pid:%d\n", startResult, reply, pid);
     if (success && pid) {
         pid_t actualPid = 0;
         if (read(pidPipe[0], (char *)&actualPid, sizeof(pid_t)) == sizeof(pid_t)) {
             *pid = actualPid;
         } else {
             *pid = 0;
         }
     }

     close(pidPipe[0]);

     LOG_DEBUG("Process start result: %s\n", success ? "OK" : "KO");

     return success;
}
#else
bool LinuxProcessRunner::start(uint64_t *pid)
{
#if 0
    if (QFile::exists(LIBSTDBUF)) {
        environment.append(QString("LD_PRELOAD=%1").arg(LIBSTDBUF));
        environment.append("_STDBUF_O=0");
    }

    environment.append(QString("LD_PRELOAD=%1").arg(GIXDBGHELPER));
#endif

    pid_t childPid = fork();

    if (childPid == 0) {
        // we are in the child process
        char *progname = strdup(this->program_name.toLocal8Bit().data());

        // arguments

        char *argv[args.size() + 2];
        argv[0] = progname;
        for (int i = 0; i < args.size(); i++)
            argv[i + 1] = strdup(args.at(i).toLocal8Bit().data());

        argv[args.size() + 1] = 0;

        // environment
        char *env[environment.size() + 1];
        for (int i = 0; i < environment.size(); i++) {
            QString k = environment.at(i);
            env[i] = strdup(k.toLocal8Bit().data());
        }

        env[environment.size()] = 0;

        if (!debugger_instance->usesExternalConsole()) {
            if (p_stdin) {
                dup2(p_stdin, STDIN_FILENO);
                close (p_stdin);
            }

            if (p_stdout) {
                dup2(p_stdout, STDOUT_FILENO);
                close (p_stdout);
            }

            if (p_stderr) {
                dup2(p_stderr, STDERR_FILENO);
                close (p_stderr);
            }
        }
        else {
            // Nothing, this should never happen
        }

        if (!working_dir.isEmpty() && QDir(working_dir).exists()) {
            chdir(working_dir.toLocal8Bit().data());
        }
        
        ptrace(PTRACE_TRACEME, 0, NULL, NULL);
        kill(getpid(), SIGSTOP);
        execve(progname, argv, env);

        _DBG_OUT("ERROR: %d\n", errno);

        ::_exit(1);

    }

    if (childPid == -1) {
        // fork error
        *pid = 0;
        return false;
    }
    else {
        // parent process
        *pid = childPid;
        return true;
    }
}
#endif

void LinuxProcessRunner::setStdIn(int s)
{
    p_stdin = s;
}

void LinuxProcessRunner::setStdOut(int s)
{
    p_stdout = s;
}


void LinuxProcessRunner::setStdErr(int s)
{
    p_stderr = s;
}

pid_t find_pid(QString prgname, QString astring)
{
    bool ok;
    QDir proc("/proc");

    for (QString e : proc.entryList()) {
        int n_pid = e.toInt(&ok);
        if (ok && n_pid > 1000) {

            QByteArray cmdline = SysUtils::FileReadAll(PathUtils::combine( { "/proc", e, "cmdline" }));
            int p0 = cmdline.indexOf((int)0, 0);

            QByteArray ap = cmdline.mid(0, p0);
            QByteArray aa = cmdline.mid(p0 + 1);

            QString sp = QString::fromLocal8Bit(ap);
            QString sa = QString::fromLocal8Bit(aa);

            printf("examining pid: %d, p: %s, a: %s\n", n_pid, sp.toLocal8Bit().constData(), sa.toLocal8Bit().constData());
            if (sp == prgname && sa == astring)
                return (pid_t) n_pid;

        }
    }
    return 0;
}


static struct pid {
    struct pid *next;
    FILE *fp;
    pid_t pid;
} *pidlist;

extern char **environ;

FILE *
c_popen(const char *program, const char *type, pid_t *the_pid)
{
    struct pid * volatile cur;
    FILE *iop;
    int pdes[2];
    pid_t pid;
    char *argp[] = {"sh", "-c", NULL, NULL};
    if ((*type != 'r' && *type != 'w') || type[1] != '\0') {
        errno = EINVAL;
        return (NULL);
    }
    if ((cur = (struct pid * ) malloc(sizeof(struct pid))) == NULL)
        return (NULL);
    if (pipe(pdes) < 0) {
        free(cur);
        return (NULL);
    }
    switch (pid = fork()) {
    case -1:			/* Error. */
        (void)close(pdes[0]);
        (void)close(pdes[1]);
        free(cur);
        return (NULL);
        /* NOTREACHED */
    case 0:				/* Child. */
    {
        struct pid *pcur;
        /*
         * We fork()'d, we got our own copy of the list, no
         * contention.
         */
        for (pcur = pidlist; pcur; pcur = pcur->next)
            close(fileno(pcur->fp));
        if (*type == 'r') {
            (void) close(pdes[0]);
            if (pdes[1] != STDOUT_FILENO) {
                (void)dup2(pdes[1], STDOUT_FILENO);
                (void)close(pdes[1]);
            }
        } else {
            (void)close(pdes[1]);
            if (pdes[0] != STDIN_FILENO) {
                (void)dup2(pdes[0], STDIN_FILENO);
                (void)close(pdes[0]);
            }
        }
        argp[2] = (char *)program;
        *the_pid = getpid();

        execve(_PATH_BSHELL, argp, environ);
        _exit(127);
        /* NOTREACHED */
    }
    default:
        *the_pid = pid;
    }
    /* Parent; assume fdopen can't fail. */
    if (*type == 'r') {
        iop = fdopen(pdes[0], type);
        (void)close(pdes[1]);
    } else {
        iop = fdopen(pdes[1], type);
        (void)close(pdes[0]);
    }
    /* Link into list of file descriptors. */
    cur->fp = iop;
    cur->pid =  pid;
    cur->next = pidlist;
    pidlist = cur;
    return (iop);
}
/*
 * pclose --
 *	Pclose returns -1 if stream is not associated with a `popened' command,
 *	if already `pclosed', or waitpid returns an error.
 */
int
c_pclose(FILE *iop)
{
    struct pid *cur, *last;
    int pstat;
    pid_t pid;
    /* Find the appropriate file pointer. */
    for (last = NULL, cur = pidlist; cur; last = cur, cur = cur->next)
        if (cur->fp == iop)
            break;
    if (cur == NULL)
        return (-1);
    (void)fclose(iop);
    do {
        pid = waitpid(cur->pid, &pstat, 0);
    } while (pid == -1 && errno == EINTR);
    /* Remove the entry from the linked list. */
    if (last == NULL)
        pidlist = cur->next;
    else
        last->next = cur->next;
    free(cur);
    return (pid == -1 ? -1 : pstat);
}
