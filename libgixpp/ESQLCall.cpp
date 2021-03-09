/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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

#define LINE_PREFIX			 "GIXSQL     "

ESQLCall::ESQLCall(const QString _call_name, bool _is_static)
{
	call_name = _call_name;
	is_static = _is_static;
}

ESQLCall::ESQLCall(bool _is_static)
{
	is_static = _is_static;
}

void ESQLCall::addParameter(QString value, bool by_reference)
{
	params.append({ value, by_reference });
}

void ESQLCall::addParameter(int value, bool by_reference)
{
	params.append({ QString::number(value), by_reference });
}

QStringList ESQLCall::format() const
{
	QStringList res;
	const char *lp = LINE_PREFIX;

	res.append(lp + QString("CALL %1\"%2\"").arg(is_static ? "STATIC " : "", call_name) + (params.size() ? " USING" : ""));
	
	for (auto p : params) {
		res.append(lp + QString("    BY %1 %2").arg(p.by_reference ? "REFERENCE" : "VALUE", p.value));
	}

	res.append(lp + QString("END-CALL"));

	return res;
}

