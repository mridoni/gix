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
#include <QVariant>
#include <QMap>
#include <QProcessEnvironment>

#include "PropertySource.h"
#include "gixcommon_global.h"

class GIXCOMMON_EXPORT MacroManager
{
public:
	MacroManager();
	MacroManager(const QMap<QString, QVariant>&);
	~MacroManager();

	void add(PropertySource *);
	void add(QProcessEnvironment&);
	void add(QMap<QString, QString>);
	void add(QMap<QString, QVariant>);
	void add(const QString &k, const QString &v);

	QString translate(QString);

	QVariant getItem(const QString &k) const;
	QVariantMap getItems();

private:
	QMap<QString, QVariant> items;
};

