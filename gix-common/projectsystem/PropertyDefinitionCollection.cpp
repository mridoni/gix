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
