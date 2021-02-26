#include <QtCore>
#include "GixPreProcessor.h"
#include "TPSourceConsolidation.h"
#include "TPESQLProcessing.h"

#undef LOG_QT_MSGS
//#define LOG_QT_MSGS

static int rc = 0;

#ifdef LOG_QT_MSGS
void msgOutput(QtMsgType type, const QMessageLogContext &context, const QString &msg)
{
	QByteArray localMsg = msg.toLocal8Bit();
	const char *file = context.file ? context.file : "";
	const char *function = context.function ? context.function : "";
	switch (type) {
		case QtDebugMsg:
			fprintf(stderr, "Debug: %s (%s:%u, %s)\n", localMsg.constData(), file, context.line, function);
			break;
		case QtInfoMsg:
			fprintf(stderr, "Info: %s (%s:%u, %s)\n", localMsg.constData(), file, context.line, function);
			break;
		case QtWarningMsg:
			fprintf(stderr, "Warning: %s (%s:%u, %s)\n", localMsg.constData(), file, context.line, function);
			break;
		case QtCriticalMsg:
			fprintf(stderr, "Critical: %s (%s:%u, %s)\n", localMsg.constData(), file, context.line, function);
			break;
		case QtFatalMsg:
			fprintf(stderr, "Fatal: %s (%s:%u, %s)\n", localMsg.constData(), file, context.line, function);
			break;
	}
}
#endif

class Task : public QObject
{
	Q_OBJECT
public:
	Task(QObject *parent = 0) : QObject(parent) {}

public slots:
	void run()
	{
		GixPreProcessor gp;
		CopyResolver copy_resolver;
		QStringList copy_dirs;

		// Do processing here
		QCoreApplication *app = (QCoreApplication *)this->parent();
		auto args = app->arguments();

		QCommandLineParser parser;

		QCommandLineOption opt(QStringList() << QStringLiteral("h") << QStringLiteral("help"), tr("Displays help on commandline options."));
		parser.addOption(opt);

		QCommandLineOption o1("I", "COPY file path list", "copypath");

		parser.addOption(o1);
		parser.addOption({ "i", "input file", "infile" });
		parser.addOption({ "o", "output file", "outfile" });
		parser.addOption({ "s", "output symbol file", "symfile" });
		parser.addOption({ {"e", "esql" }, "preprocess for ESQL (single file mode takes precedence)" });
		parser.addOption({ {"p", "esql-preprocess-copy" }, "ESQL: preprocess copy files outside EXEC SQL INCLUDE statements" });
		parser.addOption({ "E", "esql-copy-exts", "ESQL: copy files extension list (comma-separated)" });
		parser.addOption({ {"a", "esql-anon-params" }, "ESQL: use anonymous (not numbered) parameters" });
		parser.addOption({ {"S", "esql-static-calls" }, "ESQL: emit static calls" });
		parser.addOption({ {"g", "debug-info" }, "generate debug info" });
		parser.addOption({ "c", "consolidate source to single-file (CP)" });
		parser.addOption({ "k", "keep temporary files" });
		parser.addOption({ "v", "Verbose" });
		parser.addOption({ "d", "Verbose (debug)" });

		parser.process(*app);

		if (parser.isSet("help")) {
			rc = 0;
			parser.showHelp();
			goto end;
		}

		copy_dirs = parser.value("I").split(QDir::listSeparator());
		copy_resolver.setCopyDirs(copy_dirs);

		//gp.out_sym_file = parser.value("s");
		gp.setCopyResolver(&copy_resolver);

		if (parser.isSet("c"))
			gp.addStep(new TPSourceConsolidation(&gp));

		if (parser.isSet("e")) {
			gp.setOpt("emit_static_calls", parser.isSet("S"));
			gp.setOpt("anonymous_params", parser.isSet("a"));
			gp.setOpt("preprocess_copy_files", parser.isSet("p"));
			gp.addStep(new TPESQLProcessing(&gp));
			QString e = parser.value("E");
			copy_resolver.setExtensions(parser.value("E").split(","));
		}

		gp.setOpt("emit_debug_info", parser.isSet("g"));
		gp.verbose = parser.isSet("v");
		gp.verbose_debug = parser.isSet("d");

		gp.setInputFile(parser.value("i"));
		gp.setOutputFile(parser.value("o"));

		bool b = gp.process();
		if (!b) {
			rc = gp.err_code;
			for (QString m : gp.err_messages)
				fprintf(stderr, "Error: %s\n", m.toLocal8Bit().data());

			app->exit(gp.err_code);
		}

	end:
		emit finished();
	}

signals:
	void finished();
};

#include "main.moc"

int main(int argc, char *argv[])
{
#ifdef LOG_QT_MSGS
	qInstallMessageHandler(msgOutput);
#endif
	QCoreApplication a(argc, argv);

	// Task parented to the application so that it
	// will be deleted by the application.
	Task *task = new Task(&a);

	// This will cause the application to exit when
	// the task signals finished.    
	QObject::connect(task, SIGNAL(finished()), &a, SLOT(quit()));

	// This will run the task from the application event loop.
	QTimer::singleShot(0, task, SLOT(run()));

	int app_rc = a.exec();
	return app_rc | rc;
}