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

#include "PropertyOptionsDialog.h"
#include "SoapOptionsDialog.h"
#include "RestOptionsDialog.h"
#include <PropertySource.h>
#include <SysUtils.h>

typedef QMap<QString, PropertyOptionsDialog*(*)(QMainWindow*)> map_type;

template<typename T> PropertyOptionsDialog* createInstance(QMainWindow *mw) { return new T(mw); }

static map_type dlg_map;

PropertyOptionsDialog::PropertyOptionsDialog(QMainWindow* mw) : QDialog(mw)
{

}

PropertyOptionsDialog * PropertyOptionsDialog::get(QString type_id, QMainWindow *mw)
{
	if (dlg_map.size() == 0) {
		dlg_map["SoapOptionsDialog"] = &createInstance<SoapOptionsDialog>;
		dlg_map["RestOptionsDialog"] = &createInstance<RestOptionsDialog>;
	}
	return dlg_map[type_id](mw);
}

bool PropertyOptionsDialog::init(ProjectItem* pi, PropertyDefinition* pd)
{
	item = pi;
	prop_def = pd;
	PropertySource* ps = dynamic_cast<PropertySource*>(pi);

	if (ps == nullptr) {
		sub_properties.reset(nullptr);
		return false;
	}

	QMap<QString, QVariant>* cur_values = ps->PropertyGetCurrentValues();
	QVariant cur_property_value = cur_values->contains(pd->Name) ? cur_values->value(pd->Name) : pd->DefaultValue;

	sub_properties.reset(SysUtils::deserializeMap(cur_property_value.toString()));

	return true;
}

bool PropertyOptionsDialog::commitSubProperties()
{
	PropertySource* ps = dynamic_cast<PropertySource*>(item);
	if (ps == nullptr) {
		return false;
	}

	ps->PropertySetValue(prop_def->Name, SysUtils::serializeMap(sub_properties.get()));
	
	return true;
}
