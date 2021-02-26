#pragma once

#include <QString>
#include <QMetaType>

#include "PropertyDefinition.h"
#include "PropertyDefinitionCollection.h"
#include "gixcommon_global.h"

class PropertySource;

//typedef QVariant(*customPropertyFuncPtr)();      
typedef std::function<QVariant()> customPropertyFunc;

Q_DECLARE_METATYPE(customPropertyFunc);

class GIXCOMMON_EXPORT PropertySource {

public:
	PropertySource();
	virtual ~PropertySource();

	// Inherited via IPropertySource
	virtual PropertyDefinitionCollection& PropertyGetDefinitions() = 0;

    virtual QMap<QString, QVariant> *PropertyGetCurrentValues();
    virtual QMap<QString, QVariant>& getRuntimeProperties() const;
	virtual QVariant PropertyGetValue(QString, QVariant default_value = QVariant());
	virtual void PropertySetValue(QString name, QVariant value);
	virtual bool PropertyExists(QString name);

protected:
	QMap<QString, QVariant> *properties;
	QMap<QString, QVariant> runtime_properties;
};
