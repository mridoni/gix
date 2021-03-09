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
#include <functional>

#include "gixcommon_global.h"

class PropertySource;

typedef std::function<bool(PropertySource *)> showDependingOnFunc;

enum PropertyType {
	PropertyTypeUndefined = 0,
	PropertyTypeText = 1,
	PropertyTypeNumeric = 2,
	PropertyTypeBoolean = 3,
	PropertyTypeOption = 4,
	PropertyTypeDirPath = 5,
	PropertyTypeFilePath = 6, 
	PropertyTypeDirPathList = 7,
	PropertyTypeEnvVarsList = 8,
	PropertyTypeUrl = 9,
	PropertyTypeConditionalPropertySet = 10,
};

class PropertyDefinition
{
public:
	GIXCOMMON_EXPORT PropertyDefinition();
	GIXCOMMON_EXPORT PropertyDefinition(QString _name, QString _description, PropertyType _type, QVariant _default_value, bool _read_only, QMap<QString, QVariant> *_options = nullptr, QString _group = "general");

	GIXCOMMON_EXPORT ~PropertyDefinition();

	QString Group;
	QString Name;
	QString Description;
	PropertyType Type;
	QVariant DefaultValue;
	bool ReadOnly;
	QMap<QString, QVariant> *Options;

	GIXCOMMON_EXPORT QVariant parse(QString);
	GIXCOMMON_EXPORT QString serialize(QVariant);

	showDependingOnFunc show_depending_on;
};

