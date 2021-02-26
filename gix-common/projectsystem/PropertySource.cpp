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
