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

#include "BuildActionHandler.h"

class BuildActionLinkHandler : public BuildActionHandler
{
public:
	BuildActionLinkHandler();
	~BuildActionLinkHandler();

	// Inherited via BuildActionHandler
	virtual bool startBuild() override;

private:
	QStringList retrieve_link_dirs();
	QStringList retrieve_link_libs();

		
	bool generateDebugHelperObj(QStringList srclist, QString target_path, QString build_dir, QString &dbg_helper_obj);
	bool rebuildMetadata(const QStringList& mod_src_list);
	bool compileDebugHelperObj(QString build_dir, QString c_filename, QString obj_filename);
};

