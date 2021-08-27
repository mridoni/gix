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

#include "CopyResolver.h"

#include <QFile>
#include <QDir>

CopyResolver::CopyResolver(const QStringList &_copy_dirs)
{
	copy_dirs = _copy_dirs;
}

CopyResolver::CopyResolver()
{

}

void CopyResolver::resetCache()
{
	resolve_cache.clear();
}

void CopyResolver::setCopyDirs(const QStringList &_copy_dirs)
{
	QString cd = _copy_dirs.join(QDir::listSeparator());
	if (cd == hash)
		return;

	resolve_cache.clear();
	copy_dirs = _copy_dirs;
	hash = cd;
}

void CopyResolver::addCopyDir(const QString &copy_dir)
{
	if (!copy_dirs.contains(copy_dir))
		copy_dirs.append(copy_dir);
}

void CopyResolver::setExtensions(const QStringList &_copy_exts)
{
	copy_exts = _copy_exts;
}

void CopyResolver::setBaseDir(const QString _base_dir)
{
	base_dir = _base_dir;
}

QStringList &CopyResolver::getCopyDirs() const
{
	return const_cast<QStringList&>(copy_dirs);
}

bool CopyResolver::resolveCopyFile(const QString copy_name, QString &copy_file)
{
	QFile the_file;

	if (copy_name.isEmpty()) {
		fprintf(stderr, "Invalid copy name\n");
		return false;
	}

	if (resolve_cache.contains(copy_name)) {
		copy_file = resolve_cache[copy_name];
		return true;
	}

	for (QString ext : copy_exts) {

		if (ext == ".")
			ext = "";

		the_file.setFileName(base_dir + QDir::separator() + copy_name + ext);
		if (the_file.exists()) {
			copy_file = QFileInfo(the_file).absoluteFilePath();
			resolve_cache[copy_name] = copy_file;
			return true;
		}
	}

	if (copy_dirs.empty())
		return false;

	for (QString copy_dir : copy_dirs) {
		QString cn = copy_dir + QDir::separator() + copy_name.trimmed();

		for (QString ext : copy_exts) {

			if (ext == ".")
				ext = "";

			the_file.setFileName(cn + ext);
			if (the_file.exists()) {
				copy_file = QFileInfo(the_file).absoluteFilePath();
				resolve_cache[copy_name] = copy_file;
				return true;
			}
		}

	}

	return false;
}