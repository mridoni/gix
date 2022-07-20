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

#include <string>
#include <vector>

#include "ESQLDefinitions.h"
#include "gix_esql_driver.hh"

#define BY_REFERENCE	true
#define BY_VALUE		false

class ESqlCallParameter
{
public:
	std::string value;
	bool by_reference;
};

class ESQLCall
{
public:
	ESQLCall(bool _is_static);
	ESQLCall(const std::string _call_name, bool _is_static);

	void addParameter(std::string value, bool by_reference);
	void addParameter(int value, bool by_reference);
	void addParameter(gix_esql_driver *driver, hostref_or_literal_t *p, int varlen_sz = 0);

	std::vector<std::string> format(int indent_level = 0) const;
	std::string error() const;
	bool hasError() const;

private:
	bool is_static;
	std::string call_name;
	std::vector<ESqlCallParameter> params;

	bool has_error = false;
	std::string error_msg;
};

