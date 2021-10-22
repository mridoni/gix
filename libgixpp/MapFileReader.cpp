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
#include "libcpputils.h"
#include "linq/linq.hpp"

#include <queue>

MapFileReader::MapFileReader(const std::string &_filename)
{
    filename = _filename;
}

MapFileReader::~MapFileReader()
{}

bool MapFileReader::getSectionData(const std::string &section_name, std::vector<std::string> &items) const
{
    if (!vector_contains(sections, section_name) || !map_contains<std::string, std::vector<std::string>>(data, section_name))
        return false;

    items = data.at(section_name);

    return true;
}

bool MapFileReader::read()
{
    if (!file_exists(filename))
        return false;

    std::vector<std::string> lines = file_read_all_lines(filename);
    if (!lines.size())
        return false;

    int idx = 0;
    std::string cur_section = "";
    std::vector<std::string> cur_section_contents;

    while(idx < lines.size()) {
        std::string ln = lines[idx++];

        if (starts_with(ln, "[")) {
            cur_section = string_chop(ln.substr(1), 1);
            continue;
        }

        if (trim_copy(ln) == "") {
            if (cur_section != "") {
                sections.push_back(cur_section);
                data[cur_section] = cur_section_contents;
                cur_section = "";
                cur_section_contents.clear();
            }
            continue;
        }

        if (cur_section != "") {
            cur_section_contents.push_back(ln);
        }
    }

    return true;

}
