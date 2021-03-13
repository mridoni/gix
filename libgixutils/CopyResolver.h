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

