#pragma once

#include <QObject>
#include <QFile>
#include <QFileSystemWatcher>

#include "IdeTaskManager.h"

class StdStreamRedirect : public QObject
{
	Q_OBJECT

public:
	// Constructor
	StdStreamRedirect(FILE *_stream, IdeTaskManager *_ide_task_manager, QObject   *parent = NULL);

	// Destructor
	~StdStreamRedirect();

private slots:
	void fileChanged(const QString &filename);

private:
	FILE* stream;
	QFile              tmp;
	QFileSystemWatcher watcher;
	QString            tmpFileNameQtFormat;
	QString            tmpFileNameNativeFormat;

	//QTextEdit *m_errorLog;
	QString   oldContent;

	IdeTaskManager *ide_task_manager;
};
