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

#include "MapFileReader.h"
#include "SysUtils.h"
#include "linq/linq.hpp"

#include <QQueue>
#include <QRegularExpression>

static QRegularExpression rxSectionName("^\[(.*)\]$");

MapFileReader::MapFileReader(const QString &_filename)
{
    filename = _filename;
}

MapFileReader::~MapFileReader()
{}

bool MapFileReader::getSectionData(const QString &section_name, QStringList &items) const
{
    if (!sections.contains(section_name) || !data.contains(section_name))
        return false;

    items = data[section_name];
    return true;
}

bool MapFileReader::read()
{
    if (!QFile(filename).exists())
        return false;

    QStringList lines = SysUtils::FileReadAllLines(filename);
    if (!lines.size())
        return false;

    int idx = 0;
    QString cur_section = "";
    QStringList cur_section_contents;

    while(idx < lines.length()) {
        QString ln = lines[idx++];

        if (ln.startsWith("[")) {
            cur_section = ln.mid(1).chopped(1);
            continue;
        }

        if (ln.trimmed() == "") {
            if (cur_section != "") {
                sections.append(cur_section);
                data[cur_section] = cur_section_contents;
                cur_section = "";
                cur_section_contents.clear();
            }
            continue;
        }

        if (cur_section != "") {
            cur_section_contents.append(ln);
        }
    }

    return true;

}
