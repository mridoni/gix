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

