#include <QApplication>
#include <QCommandLineParser>
#include <QCommandLineOption>
#include <QSplashScreen>
#include <QDesktopWidget>

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

#if defined(Q_OS_LINUX) && defined(_DEBUG)
void handler(int sig) {
  void *array[10];
  size_t size;

  // get void*'s for all entries on the stack
  size = backtrace(array, 10);

  // print out all the frames to stderr
  fprintf(stderr, "Error: signal %d:\n", sig);
  backtrace_symbols_fd(array, size, STDERR_FILENO);
  exit(1);
}
#endif

int main(int argc, char *argv[])
{
#if defined(Q_OS_WIN) && defined(_DEBUG)
	//_CrtSetDbgFlag(_CRTDBG_CHECK_ALWAYS_DF);
#endif

	Q_INIT_RESOURCE(icons);

#if defined(Q_OS_LINUX) && defined(_DEBUG)
    signal(SIGSEGV, handler);   // install our handler
#endif

	//QApplication::setAttribute(Qt::AA_EnableHighDpiScaling, false);

	QApplication app(argc, argv);

	QPixmap pixmap(":/icons/splash.png");
	QSplashScreen splash(pixmap);

	splash.show();
	app.processEvents();

	QCoreApplication::setApplicationName("gix-ide");
	QCoreApplication::setOrganizationName("MediumGray");
	QCoreApplication::setApplicationVersion("0.0.1");

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

	foreach(const QString &fileName, parser.positionalArguments())
		mainWin.openFile(fileName);
	
	mainWin.show();
	
	splash.finish(&mainWin);

	return app.exec();
}
