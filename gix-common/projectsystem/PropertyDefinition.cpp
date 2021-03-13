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

#include "PropertyDefinition.h"



PropertyDefinition::PropertyDefinition()
{
	this->Name = "";
	this->Description = "";
	this->Type = PropertyType::PropertyTypeUndefined;
	this->DefaultValue = "";
	this->ReadOnly = "";
	this->Options = nullptr;
	this->Group = "general";
}

PropertyDefinition::PropertyDefinition(QString _name, QString _description, PropertyType _type, QVariant _default_value, bool _read_only, QMap<QString, QVariant>* _options, QString _group) : PropertyDefinition()
{
	this->Name = _name;
	this->Description = _description;
	this->Type = _type;
	this->DefaultValue = _default_value;
	this->ReadOnly = _read_only;
	this->Options = _options;
	this->Group = _group;
}



PropertyDefinition::~PropertyDefinition()
{
}

QVariant PropertyDefinition::parse(QString s)
{
	switch (Type) {
		case PropertyType::PropertyTypeText:
		case PropertyType::PropertyTypeFilePath:
		case PropertyType::PropertyTypeDirPath:
			return QVariant(s);

		case PropertyType::PropertyTypeBoolean:
			return QVariant(s == "true");

		case PropertyType::PropertyTypeOption:
			return QVariant(s);

		case PropertyType::PropertyTypeDirPathList:
		case PropertyType::PropertyTypeEnvVarsList:
		{
			QStringList l = (!s.isEmpty()) ? s.split("|") : QStringList();
			QVariant v;
			v.setValue(l);
			return v;
		}

		case PropertyType::PropertyTypeConditionalPropertySet:
			return QVariant();
	}
	return QVariant();
}

QString PropertyDefinition::serialize(QVariant value)
{
	switch (Type) {
		case PropertyType::PropertyTypeText:
		case PropertyType::PropertyTypeFilePath:
		case PropertyType::PropertyTypeDirPath:
			return value.toString();

		case PropertyType::PropertyTypeBoolean:
			return value.toBool() ? "true" : "false";

		case PropertyType::PropertyTypeOption:
			return value.toString();

		case PropertyType::PropertyTypeDirPathList:
		case PropertyType::PropertyTypeEnvVarsList:
		{	
			QStringList l = value.toStringList();
			return l.join('|'); 
		}

		case PropertyType::PropertyTypeConditionalPropertySet: 
		{
			return "";
		}

	}
	return QString();
}
