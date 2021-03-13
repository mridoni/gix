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

#include <QList>
#include <QCoreApplication>
#include <QString>
#include "PropertyDefinition.h"
#include "gixcommon_global.h"


class PropertyGroup {

public:

	PropertyGroup(QString, QString);

	QString id;
	QString name;
};

class PropertyDefinitionCollection
{
	Q_DECLARE_TR_FUNCTIONS(PropertyDefinitionCollection)

public:

	GIXCOMMON_EXPORT PropertyDefinitionCollection();
	
	GIXCOMMON_EXPORT int size();
	GIXCOMMON_EXPORT bool existsPropertyWithName(QString name);
	GIXCOMMON_EXPORT PropertyDefinition* getPropertyDefinition(QString name);
	GIXCOMMON_EXPORT QList<PropertyDefinition*> enumerateByGroup();
	GIXCOMMON_EXPORT QString getPropertyGroupName(QString);
	GIXCOMMON_EXPORT int getGroupCount();
	GIXCOMMON_EXPORT QList<PropertyDefinition *> &getAll() const;

protected:
	QList<PropertyDefinition*> defs;
	QList<PropertyGroup *> property_groups;
};

