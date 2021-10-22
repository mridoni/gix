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
#include "libcpputils.h"

#include <filesystem>


CopyResolver::CopyResolver(const std::vector<std::string> &_copy_dirs)
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

void CopyResolver::setCopyDirs(const std::vector<std::string> &_copy_dirs)
{
	std::string cd = vector_join(_copy_dirs, PATH_SEPARATOR);
	if (cd == hash)
		return;

	resolve_cache.clear();
	copy_dirs = _copy_dirs;
	hash = cd;
}

void CopyResolver::addCopyDir(const std::string &copy_dir)
{
	if (!vector_contains<std::string>(copy_dirs, copy_dir))
		copy_dirs.push_back(copy_dir);
}

void CopyResolver::setExtensions(const std::vector<std::string> &_copy_exts)
{
	copy_exts = _copy_exts;
}

void CopyResolver::setBaseDir(const std::string _base_dir)
{
	base_dir = _base_dir;
}

std::vector<std::string> &CopyResolver::getCopyDirs() const
{
	return const_cast<std::vector<std::string>&>(copy_dirs);
}

bool CopyResolver::resolveCopyFile(const std::string copy_name, std::string &copy_file)
{
	std::filesystem::path the_file;
	
	if (copy_name.empty()) {
		fprintf(stderr, "Invalid copy name\n");
		return false;
	}

	if (map_contains<std::string>(resolve_cache, copy_name)) {
		copy_file = resolve_cache[copy_name];
		return true;
	}

	for (std::string ext : copy_exts) {

		if (ext == ".")
			ext = "";

		the_file.replace_filename(base_dir + PATH_SEPARATOR + copy_name + ext);
		if (std::filesystem::exists(the_file)) {
			copy_file = filename_absolute_path(the_file);
			resolve_cache[copy_name] = copy_file;
			return true;
		}
	}

	if (copy_dirs.empty())
		return false;

	for (std::string copy_dir : copy_dirs) {
		std::string cn = copy_dir + PATH_SEPARATOR + trim_copy(copy_name);

		for (std::string ext : copy_exts) {

			if (ext == ".")
				ext = "";

			the_file.replace_filename(cn + ext);
			if (std::filesystem::exists(the_file)) {
				copy_file = filename_absolute_path(the_file);
				resolve_cache[copy_name] = copy_file;
				return true;
			}
		}

	}

	return false;
}