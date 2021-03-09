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

#pragma once

#include <QString>
#include <QList>

#define BY_REFERENCE	true
#define BY_VALUE		false

class ESqlCallParameter
{
public:
	QString value;
	bool by_reference;
};

class ESQLCall
{
public:
	ESQLCall(bool _is_static);
	ESQLCall(const QString _call_name, bool _is_static);

	void addParameter(QString value, bool by_reference);
	void addParameter(int value, bool by_reference);

	QStringList format() const;

private:
	bool is_static;
	QString call_name;
	QList<ESqlCallParameter> params;
};

