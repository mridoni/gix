/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
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