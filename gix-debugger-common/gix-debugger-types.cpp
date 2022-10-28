#include "gix-debugger-types.h"

VariableDisplayData::~VariableDisplayData()
{
}

json11::Json VariableDisplayData::to_json() const {

	return json11::Json::object{
	{ "__obj_type__", "VariableDisplayData"},
	{ "var_name", var_name },
	{ "module_name", module_name },
	{ "var_path", var_path },
	{ "type", (int)type },
	{ "level", level },
	{ "display_size", display_size },
	{ "is_signed", is_signed },
	{ "decimals", decimals },
	{ "format", format },
	{ "storage_type", storage_type },
	{ "storage", storage },
	{ "occurs", occurs },
	{ "redefines", redefines },
	{ "base_var_name", base_var_name },
	{ "local_sym_name", local_sym_name },
	{ "local_addr", local_addr },
	{ "storage_size", storage_size },
	{ "display_data", display_data }
	};
}

VariableDisplayData VariableResolverData::toDisplayData()
{
	VariableDisplayData vd;
	vd.var_name = this->var_name;
	vd.module_name = this->module_name;
	vd.var_path = this->var_path;
	vd.type = this->type;
	vd.level = this->level;
	vd.display_size = this->display_size;
	vd.is_signed = this->is_signed;
	vd.decimals = this->decimals;
	vd.format = this->format;
	vd.storage_type = this->storage_type;
	vd.storage = this->storage;
	vd.occurs = this->occurs;
	vd.redefines = this->redefines;
	vd.base_var_name = this->base_var_name;
	vd.local_sym_name = this->local_sym_name;
	vd.local_addr = this->local_addr;
	vd.storage_size = this->storage_size;
	return vd;
}
