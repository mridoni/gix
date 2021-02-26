#include "StdStreamRedirect.h"


#include <QDir>
#include <QTextStream>

StdStreamRedirect::StdStreamRedirect(FILE *_stream, IdeTaskManager *_ide_task_manager, QObject   *parent)
	: QObject(parent)
{
	stream = _stream;

	// Store the pointer to the error log window
	ide_task_manager = _ide_task_manager;

	// Create a temporary filename: first find the path:
	tmpFileNameQtFormat = QDir::tempPath();

	// Make sure the closing slash is present:
	if (!tmpFileNameQtFormat.endsWith(QChar('/')))
		tmpFileNameQtFormat.append(QChar('/'));

	// Add the file name itself:
	tmpFileNameQtFormat.append(stream == stdout ? "nb_stdoutlog" : "nb_stderrlog");

	// Obtain a version of the filename in the operating system's native format:
	tmpFileNameNativeFormat = QDir::toNativeSeparators(tmpFileNameQtFormat);

	// Set up redirection to this file:
	freopen(tmpFileNameNativeFormat.toLatin1().constData(), "a+", stream);

	// Initialise the QFileSystemWatcher:
	connect(&watcher, SIGNAL(fileChanged(const QString &)),
		this, SLOT(fileChanged(const QString &)));
	watcher.addPath(tmpFileNameQtFormat);

	tmp.setFileName(tmpFileNameQtFormat);
}

StdStreamRedirect::~StdStreamRedirect()
{
	// Ensure the temporary file is properly deleted:
	fclose(stream);
	tmp.close();
	//tmp.open(QIODevice::ReadWrite);
	//tmp.remove();
}

void StdStreamRedirect::fileChanged(const QString &filename)
{
	tmp.open(QIODevice::ReadOnly);
	QTextStream stream(&tmp);
	QString content = stream.readAll();
	tmp.close();

	// Identify what's new, and just send this to the window:
	int newchars = content.size() - oldContent.size();
	if (newchars)
	{
		//m_errorLog->append(content.right(newchars));
		ide_task_manager->logMessage(GIX_CONSOLE_LOG, content.right(newchars), QLogger::LogLevel::Error);
		oldContent = content;
	}
}