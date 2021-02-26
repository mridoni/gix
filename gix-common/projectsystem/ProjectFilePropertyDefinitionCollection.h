#pragma once
#include "PropertyDefinitionCollection.h"

class ProjectFilePropertyDefinitionCollection :
	public PropertyDefinitionCollection
{
public:
	ProjectFilePropertyDefinitionCollection();
	~ProjectFilePropertyDefinitionCollection();

private:
	QMap<QString, QVariant> *build_action_opts;
	QMap<QString, QVariant> *build_type_opts;
};

