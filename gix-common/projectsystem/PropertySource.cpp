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

#include "PropertySource.h"
#include "SysUtils.h"

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
		res = properties->value(name);
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

QVariant PropertySource::getSubProperty(QString prop_id, QString sub_prop_id)
{
	if (!PropertyExists(prop_id))
		return QVariant();

	QString serialized_sub_props = PropertyGetValue(prop_id).toString();

	auto sub_props = SysUtils::deserializeMap(serialized_sub_props);
	if (sub_props && sub_props->contains(sub_prop_id) && !sub_props->value(sub_prop_id).isNull()) {
		return sub_props->value(sub_prop_id);
	}
	return QVariant();
}

bool PropertySource::setSubProperty(QString prop_id, QString sub_prop_id, QVariant sub_prop_value)
{

	QVariantMap *map = PropertyExists(prop_id) ? SysUtils::deserializeMap(PropertyGetValue(prop_id).toString()) : new QVariantMap();

	if (!map)
		return false;

	map->insert(sub_prop_id, sub_prop_value);

	PropertySetValue(prop_id, SysUtils::serializeMap(map));

	return true;
}