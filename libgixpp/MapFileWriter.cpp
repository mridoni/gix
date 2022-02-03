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

#include "MapFileWriter.h"
#include "libcpputils.h"

#include <filesystem>
#include <fstream>

void MapFileWriter::addSection(const std::string &section_name, const std::vector<std::string> &section_contents)
{
    sections.push_back(section_name);
    data[section_name] = section_contents;
}

void MapFileWriter::setSectionContents(const std::string & section_name, const std::vector<std::string> & section_contents)
{
    data[section_name] = section_contents;
}

void MapFileWriter::appendToSectionContents(const std::string & section_name, const std::vector<std::string> & more_contents)
{
    std::vector<std::string> cur_contents = data.find(section_name) == data.end() ? std::vector<std::string>() : data[section_name];
    cur_contents.insert(cur_contents.end(), more_contents.begin(), more_contents.end());
    data[section_name] = cur_contents;
}

void MapFileWriter::appendToSectionContents(const std::string & section_name, const std::string & content)
{
    std::vector<std::string> cur_contents = data.find(section_name) == data.end() ? std::vector<std::string>() : data[section_name];
    cur_contents.push_back(content);
    data[section_name] = cur_contents;
}

void MapFileWriter::appendToSectionContents(const std::string &section_name, int content)
{
    std::vector<std::string> cur_contents = data.find(section_name) == data.end() ? std::vector<std::string>() : data[section_name];
    cur_contents.push_back(std::to_string(content));
    data[section_name] = cur_contents;
}

bool MapFileWriter::writeToFile(const std::string& filename)
{
    std::filesystem::path filepath(filename);

    std::ofstream ofs(filepath);

    for (std::string section_name : sections) {
        ofs << string_format("[%s]\n", trim_copy(section_name));
        for (std::string ln : data[section_name]) {
            ofs << string_format("%s\n", trim_copy(ln));
        }
        ofs << "\n";
    }

    ofs.close();


    return true;
}
