#include "ProjectFolderPropertyDefinitionCollection.h"



ProjectFolderPropertyDefinitionCollection::ProjectFolderPropertyDefinitionCollection()
{
	defs.append(new PropertyDefinition("name", "Name", PropertyType::PropertyTypeText, "", true));
	defs.append(new PropertyDefinition("is_virtual", "Is Virtual", PropertyType::PropertyTypeBoolean, false, true));
}


ProjectFolderPropertyDefinitionCollection::~ProjectFolderPropertyDefinitionCollection()
{
}
