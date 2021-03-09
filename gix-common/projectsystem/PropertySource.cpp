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

#include "PropertySource.h"

PropertySource::PropertySource()
{
	qRegisterMetaType<customPropertyFunc>("customPropertyFunc");

	properties = new QMap<QString, QVariant>();
}

PropertySource::~PropertySource()
{
	delete properties;
}

QMap<QString, QVariant> *PropertySource::PropertyGetCurrentValues()
{
	return properties;
}

QMap<QString, QVariant> &PropertySource::getRuntimeProperties() const
{
	return const_cast<QMap<QString, QVariant>&>(runtime_properties);
}

QVariant PropertySource::PropertyGetValue(QString name, QVariant default_value)
{
	QVariant res;

	if (properties->contains(name))
		res = (*properties)[name];
	else {
		if (default_value.isValid()) {
			res = default_value;
		}
		else {
			PropertyDefinition *pd = PropertyGetDefinitions().getPropertyDefinition(name);
			if (pd != nullptr) {
				res = pd->DefaultValue;
			}
			//else {
			//	return QVariant();
			//}
		}
	}

	if (res.value<customPropertyFunc>() != nullptr) {
		customPropertyFunc fcn = res.value<customPropertyFunc>();
		res = fcn();
	}
	return res;
}

void PropertySource::PropertySetValue(QString name, QVariant value)
{
	properties->insert(name, value);
}

bool PropertySource::PropertyExists(QString name)
{
	return properties->contains(name);
}
