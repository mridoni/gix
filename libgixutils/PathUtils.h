#pragma once

#include <QString>
#include <QFileInfo>
#include <QDir>

class PathUtils
{
public:
	static QString combine(QString, QString);
	static QString combine(std::initializer_list<QString> a_args);
	static QString getFilename(QString);
	static QString getDirectory(QString p);
	static QString getAbsoluteDirectory(QString p);
	static bool isValidDirectoryName(QString s);
	static bool isValidFileName(QString s);
	static QString toModuleName(QString s);
	static QString quote(QString s);
	static QString changeExtension(QString, QString);
	static QString rebasePath(QString child, QString parent);
};

