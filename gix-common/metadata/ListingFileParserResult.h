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

#if 0

#include <QList>
#include <QMap>
#include <QString>
#include <QStringList>
#include <QDateTime>

#include "DataEntry.h"

class ListingFileParserResult
{
public:

	QDateTime last_parsed;
	QStringList copy_deps;

	QString module_name;
	QString linkage_section_text;
	QList<DataEntry *> ws_entries;
	QList<DataEntry *> ls_entries;
	QList<DataEntry *> fs_entries;
	QMap<QString, Paragraph *> paragraphs;
};

#endif