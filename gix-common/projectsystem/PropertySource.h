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

	virtual QVariant getSubProperty(QString prop_id, QString sub_prop_id);
	virtual bool setSubProperty(QString prop_id, QString sub_prop_id, QVariant sub_prop_value);

protected:
	QMap<QString, QVariant> *properties;
	QMap<QString, QVariant> runtime_properties;
};
