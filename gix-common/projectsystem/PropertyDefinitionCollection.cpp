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

#include "PropertyDefinitionCollection.h"
#include "linq/linq.hpp"

#include <QVector>

using namespace cpplinq;

PropertyDefinitionCollection::PropertyDefinitionCollection()
{
	property_groups.append(new PropertyGroup("general", "General"));
}

int PropertyDefinitionCollection::size()
{
	return defs.size();
}

QList<PropertyDefinition*> PropertyDefinitionCollection::enumerateByGroup()
{
	QList<PropertyDefinition*> res;
	auto general_props = from(defs).where([](PropertyDefinition *a) {return a->Group == "general"; }).to_vector();
	res.append(QList<PropertyDefinition*>::fromVector(QVector<PropertyDefinition*>::fromStdVector(general_props)));

	auto other_props = from(defs).where([](PropertyDefinition *a) {return a->Group != "general"; }).to_vector();
	sort(other_props.begin(), other_props.end(), [](PropertyDefinition *a, PropertyDefinition *b) { return a->Group > b->Group; });
	res.append(QList<PropertyDefinition*>::fromVector(QVector<PropertyDefinition*>::fromStdVector(other_props)));

	return res;
}

QString PropertyDefinitionCollection::getPropertyGroupName(QString gid)
{
	QString name = from(property_groups)
		.where([gid](PropertyGroup *a) {return a->id == gid; }).to_vector().at(0)->name;

	return name;
}

int PropertyDefinitionCollection::getGroupCount()
{
	return property_groups.size();
}

QList<PropertyDefinition *> &PropertyDefinitionCollection::getAll() const
{
	return const_cast<QList<PropertyDefinition *> &>(defs);
}

bool PropertyDefinitionCollection::existsPropertyWithName(QString name)
{
	for (int i = 0; i < defs.size(); i++) {
		if (defs.at(i)->Name == name)
			return true;
	}
	return false;
}

PropertyDefinition *PropertyDefinitionCollection::getPropertyDefinition(QString name)
{
	for (int i = 0; i < defs.size(); i++) {
		if (defs.at(i)->Name == name)
			return defs.at(i);
	}
	return nullptr;
}

PropertyGroup::PropertyGroup(QString _id, QString _name)
{
	id = _id;
	name = _name;
}
