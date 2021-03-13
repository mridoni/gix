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

#include "BuildConsts.h"

const QString BuildConsts::MODULE_EXECUTABLE = "exe";
const QString BuildConsts::MODULE_DYNLOAD = "dll";
const QString BuildConsts::MODULE_DEFAULT = "";

const QString BuildConsts::BUILD_ACTION_COMPILE = "compile";
const QString BuildConsts::BUILD_ACTION_PREPROC_ESQL = "preprocess_esql";
const QString BuildConsts::BUILD_ACTION_LINK = "link";
const QString BuildConsts::BUILD_ACTION_GENERATE_SYMBOLS = "generate_symbols";
const QString BuildConsts::BUILD_ACTION_COPY = "copy";
const QString BuildConsts::BUILD_ACTION_MK_LISTING = "mk_listing";
const QString BuildConsts::BUILD_ACTION_NONE = "none";

const QString BuildConsts::TYPE_OBJ = "obj";
const QString BuildConsts::TYPE_OBJ_MAIN = "objm";
const QString BuildConsts::TYPE_LIB = "lib";
const QString BuildConsts::TYPE_CBSQL = "cbsql";
const QString BuildConsts::TYPE_CBCICS = "cbcics";
const QString BuildConsts::TYPE_COBOL = "cbl";
const QString BuildConsts::TYPE_CBLPP = "cblpp";

const QString BuildConsts::TYPE_LISTING = "lst";



const QString BuildConsts::TYPE_FINAL = "__final_target";	// Dummy target for multi-target/multi-binary projects
const QString BuildConsts::TYPE_NONE = "";

