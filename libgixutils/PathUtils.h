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

