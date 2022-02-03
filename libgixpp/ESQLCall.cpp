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

#include "ESQLCall.h"
#include "libcpputils.h"

#define LINE_PREFIX			 "GIXSQL     "

ESQLCall::ESQLCall(const std::string _call_name, bool _is_static)
{
	call_name = _call_name;
	is_static = _is_static;
}

ESQLCall::ESQLCall(bool _is_static)
{
	is_static = _is_static;
}

void ESQLCall::addParameter(std::string value, bool by_reference)
{
	params.push_back({ value, by_reference });
}

void ESQLCall::addParameter(int value, bool by_reference)
{
	params.push_back({ std::to_string(value), by_reference });
}

std::vector<std::string> ESQLCall::format() const
{
	std::vector<std::string> res;
	const char *lp = LINE_PREFIX;

	res.push_back(lp + string_format("CALL %s\"%s\"", (is_static ? "STATIC " : ""), call_name) + (params.size() ? " USING" : ""));
	
	for (auto p : params) {
		res.push_back(lp + string_format("    BY %s %s", (p.by_reference ? "REFERENCE" : "VALUE"), p.value));
	}

	res.push_back(lp + std::string("END-CALL"));

	return res;
}

