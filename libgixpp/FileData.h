#pragma once

#include <QString>
#include <QList>
#include <QMap>
#include <QDateTime>

class FileData;

class FileData
{
public:
	FileData();
	~FileData();

	// Full path
	QString filename;


	QDateTime file_ts;

	FileData *parent;
	QMap<int, FileData *> refs;
	QStringList lines;

};

