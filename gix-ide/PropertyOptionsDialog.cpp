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
