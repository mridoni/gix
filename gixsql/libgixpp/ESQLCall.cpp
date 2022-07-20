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

void ESQLCall::addParameter(gix_esql_driver *driver, hostref_or_literal_t *p, int varlen_sz)
{
	int f_type = 0, f_size = 0, f_scale = 0;

	if (!p || !p->is_set) {
		addParameter("x\"00\"", BY_REFERENCE);
		addParameter(0, BY_VALUE);
		return;
	}

	if (p->is_literal) {
		std::string n = unquote(p->name);
		addParameter("\"" + n + "\" & x\"00\"", BY_REFERENCE);
		addParameter(0, BY_VALUE);
	}
	else {
		if (!map_contains<std::string, cb_field_ptr>(driver->field_map, p->name.substr(1)) || !driver->field_map[p->name.substr(1)]) {
			this->error_msg = "Invalid or undefined parameter name: " + p->name.substr(1);
			this->has_error = true;
			return;
		}

		// N.B. If we are dealing with a varlen field, we are pointing to the root element of the varlen group
		addParameter(p->name.substr(1), BY_REFERENCE);

		if (!varlen_sz) {	// Not a variable length field
			addParameter(driver->field_map[p->name.substr(1)]->picnsize, BY_VALUE);
		}
		else {
			addParameter(-varlen_sz, BY_VALUE);
		}
	}

}

void ESQLCall::addParameter(int value, bool by_reference)
{
	params.push_back({ std::to_string(value), by_reference });
}

std::vector<std::string> ESQLCall::format(int indent_level) const
{
	std::string indent = std::string(indent_level * 4, ' ');

	std::vector<std::string> res;
	const char *lp = LINE_PREFIX;

	res.push_back(lp + string_format(indent + "CALL %s\"%s\"", (is_static ? "STATIC " : ""), call_name) + (params.size() ? " USING" : ""));
	
	for (auto p : params) {
		res.push_back(lp + string_format(indent + "    BY %s %s", (p.by_reference ? "REFERENCE" : "VALUE"), p.value));
	}

	res.push_back(lp + indent + std::string("END-CALL"));

	return res;
}

std::string ESQLCall::error() const
{
	return error_msg;
}

bool ESQLCall::hasError() const
{
	return has_error;
}
