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

