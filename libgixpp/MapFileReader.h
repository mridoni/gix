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

#include <vector>
#include <string>
#include <map>

class MapFileReader
{
public:
	MapFileReader(const std::string &filename);
	~MapFileReader();

	bool read();
	bool getSectionData(const std::string &section_name, std::vector<std::string> &items) const;

private:
	std::string filename;
	std::vector<std::string> sections;
	std::map<std::string, std::vector<std::string>> data;
};

