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

class BuildConsts {

public:
	const static QString MODULE_EXECUTABLE;
	const static QString MODULE_DYNLOAD;
	const static QString MODULE_DEFAULT;

	const static QString BUILD_ACTION_COMPILE;
	const static QString BUILD_ACTION_PREPROC_ESQL;
	const static QString BUILD_ACTION_LINK;
	const static QString BUILD_ACTION_GENERATE_SYMBOLS;
	const static QString BUILD_ACTION_COPY;
	const static QString BUILD_ACTION_MK_LISTING;
	const static QString BUILD_ACTION_NONE;
		
	const static QString TYPE_OBJ;
	const static QString TYPE_OBJ_MAIN;
	const static QString TYPE_LIB;
	const static QString TYPE_CBSQL;
	const static QString TYPE_CBLPP;
	const static QString TYPE_CBCICS;
	const static QString TYPE_COBOL;

	const static QString TYPE_LISTING;

	const static QString TYPE_FINAL;
	const static QString TYPE_NONE;
};