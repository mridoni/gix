#pragma once

#include "PropertyDefinitionCollection.h"

class ProjectPropertyDefinitionCollection : public PropertyDefinitionCollection
{

public:
	ProjectPropertyDefinitionCollection();
	~ProjectPropertyDefinitionCollection();

private:
	QMap<QString, QVariant> *build_type_opts;
	QMap<QString, QVariant> *build_type_opts_module;
	QMap<QString, QVariant> *compiler_dialect_opts;
	QMap<QString, QVariant> *esql_opts;
};

