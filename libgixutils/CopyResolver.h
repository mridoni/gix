#pragma once

#include <QString>
#include <QStringList>
#include <QMap>

#include "libgixutils_global.h"

class CopyResolver
{
public:
	CopyResolver(const QStringList& _copy_dirs);
	CopyResolver();

	void resetCache();
	void setCopyDirs(const QStringList& _copy_dirs);
	void setExtensions(const QStringList& _copy_exts);
	void setBaseDir(const QString base_dir);
	QStringList& getCopyDirs() const;
	bool resolveCopyFile(const QString copy_name, QString &copy_file);

private:
	QStringList copy_dirs;
	QStringList copy_exts;
	QString base_dir;
	QString hash;

	QMap<QString, QString> resolve_cache;
};

