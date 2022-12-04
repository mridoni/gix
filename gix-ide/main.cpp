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

#define ORG_NAME "MediumGray"
#define APP_NAME "gix-ide"

#ifdef _WIN32
#include <Windows.h>
#include <imagehlp.h>
#endif

#include <QApplication>
#include <QCommandLineParser>
#include <QCommandLineOption>
#include <QSplashScreen>
#include <QDesktopWidget>
#include <QTimer>

#include <QDebug>
#include <QDataStream>
#include <QSettings>

#if defined(Q_OS_LINUX) && defined(_DEBUG)
#include <execinfo.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#endif

#include "MainWindow.h"
#include "Ide.h"

#include "GixGlobals.h"
#include "IGixLogManager.h"
#include "IdeLogManager.h"
#include "GixVersion.h"
#include "DataEntry.h"


#if 0
class GixApplication final : public QApplication {
public:
    GixApplication(int& argc, char** argv) : QApplication(argc, argv) {}
    virtual bool notify(QObject *receiver, QEvent *e) override {
        try {
            return QApplication::notify(receiver, e);
        }
        catch(std::runtime_error e)
        {
            qDebug() << "std::runtime_error in thread : " << QThread::currentThreadId();
            qDebug() << e.what();
        }
        catch(std::exception e)
        {
            qDebug() << "std::exception in thread : " << QThread::currentThreadId();
            qDebug() << e.what();
        }
        catch(...)
        {
            qDebug() << "exception thread : " << QThread::currentThreadId();
        }

        qDebug() << "catch in notify ";
        return false;
    }
};
#endif

#if 0 && defined(Q_OS_LINUX) && defined(_DEBUG)
void handler(int sig) {
  void *array[50];
  size_t size;

  // get void*'s for all entries on the stack
  size = backtrace(array, 50);

  // print out all the frames to stderr
  fprintf(stderr, "Error: signal %d:\n", sig);
  backtrace_symbols_fd(array, size, STDERR_FILENO);
  exit(1);
}

void
handler_t()
{
    void *trace_elems[50];
    int trace_elem_count(backtrace( trace_elems, 50 ));
    char **stack_syms(backtrace_symbols( trace_elems, trace_elem_count ));
    for ( int i = 0 ; i < trace_elem_count ; ++i )
    {
        std::cout << stack_syms[i] << "\n";
    }
    free( stack_syms );

    exit(1);
}

#endif

#ifdef _WIN32

LONG WINAPI GixCrashHandler(EXCEPTION_POINTERS *ex_info)
{
    bool bFailed = true;
    HANDLE hFile;
    char bfr[MAX_PATH];

    ExpandEnvironmentStrings("%TEMP%\\gix-ide.dmp", bfr, MAX_PATH);

    hFile = CreateFile(bfr, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile != INVALID_HANDLE_VALUE) {
        MINIDUMP_EXCEPTION_INFORMATION stMDEI;
        stMDEI.ThreadId = GetCurrentThreadId();
        stMDEI.ExceptionPointers = ex_info;
        stMDEI.ClientPointers = TRUE;

        if (MiniDumpWriteDump(
            GetCurrentProcess(),
            GetCurrentProcessId(),
            hFile,
            MiniDumpNormal,
            &stMDEI,
            NULL,
            NULL
        )) {
            bFailed = false;  // suceeded
        }
        CloseHandle(hFile);
    }

    if (bFailed) {
        return EXCEPTION_CONTINUE_SEARCH;
    }

    //FatalAppExit(-1, "Sorry, Gix-IDE crashed!");

    return EXCEPTION_CONTINUE_SEARCH;  // this will trigger the "normal" OS error-dialog
}

#endif

QDataStream &operator<<(QDataStream &out, DataEntry **const &rhs)
{
	out.writeRawData(reinterpret_cast<const char *>(&rhs), sizeof(rhs));
	return out;
}

QDataStream &operator >> (QDataStream &in, DataEntry * &rhs)
{
	in.readRawData(reinterpret_cast<char *>(&rhs), sizeof(rhs));
	return in;
}

int main(int argc, char *argv[])
{
    qRegisterMetaTypeStreamOperators<DataEntry *>("DataEntry *");
    qRegisterMetaType<DataEntry>("DataEntry");

#if 0 && defined(Q_OS_WIN) && defined(_DEBUG)
    _CrtSetDbgFlag(_CRTDBG_CHECK_ALWAYS_DF);
    _CrtSetDbgFlag(_CRTDBG_LEAK_CHECK_DF);
    _CrtSetDbgFlag(_CRTDBG_CHECK_CRT_DF);
#endif
    
    if (!qgetenv("QT_FONT_DPI").size()) {

        QSettings settings(ORG_NAME, APP_NAME);
        int dpi = settings.value("screen_resolution", 0).toInt();
        if (dpi > 0)
            qputenv("QT_FONT_DPI", QString::number(dpi).toUtf8().data());
    }
    
    QApplication app(argc, argv);

	Q_INIT_RESOURCE(icons);

	QCoreApplication::setApplicationName(APP_NAME);
	QCoreApplication::setOrganizationName(ORG_NAME);
    QCoreApplication::setApplicationVersion(getGixIdePrintableVersion());

#if 0 && defined(Q_OS_LINUX) && defined(_DEBUG)
    signal(SIGSEGV, handler);   // install our handler
    std::set_terminate( handler_t );
#endif

#ifdef _WIN32
    SetUnhandledExceptionFilter(GixCrashHandler);
#endif 
    
	QPixmap pixmap(":/icons/splash.png");
	QPainter painter(&pixmap);
	QFont font("Trebuchet MS", 8);
	font.setBold(true);
	painter.setFont(font);
	painter.setPen(QColor::fromRgb(52, 125, 180));
	painter.drawText(QPoint(282, 212), "v" + getGixIdePrintableVersion());

	QSplashScreen splash(pixmap);
	splash.show();

	app.processEvents();



#ifdef Q_OS_MACOS
    QApplication::setStyle("macintosh");
	QApplication::instance()->setAttribute(Qt::AA_DontShowIconsInMenus, true);
#endif

	qsrand(QDateTime::currentMSecsSinceEpoch() % UINT_MAX);

	QCommandLineParser parser;
	parser.setApplicationDescription("Gix IDE");
	parser.addHelpOption();
	parser.addVersionOption();
	parser.addPositionalArgument("file", "The file to open.");
	parser.process(app);

	GixGlobals::registerLogManager(new IdeLogManager());

	Ide::init();

	MainWindow mainWin;
	mainWin.setWindowFlags(Qt::WindowTitleHint | 
						   Qt::WindowMinimizeButtonHint | 
						   Qt::WindowMaximizeButtonHint | 
						   Qt::WindowCloseButtonHint | 
						   Qt::WindowSystemMenuHint);

    mainWin.setDockOptions(QMainWindow::AllowNestedDocks | QMainWindow::QMainWindow::AllowTabbedDocks);

	foreach(const QString &fileName, parser.positionalArguments())
		mainWin.openFile(fileName);

	mainWin.show();
	splash.finish(&mainWin);

    return app.exec();


}
