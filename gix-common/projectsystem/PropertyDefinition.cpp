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
