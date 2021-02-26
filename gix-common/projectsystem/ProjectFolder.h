#pragma once

#include <QString>

#include "ProjectItem.h"
#include "PropertySource.h"
#include "BuildTarget.h"
#include "IBuildableItem.h"

class ProjectFile;

class GIXCOMMON_EXPORT ProjectFolder : public ProjectItem, public PropertySource
{
public:
	ProjectFolder(QString, bool);
	~ProjectFolder();

	bool CreateIfNotExists();
	void getFilesRecursively(QList<ProjectFile *>&);

	// Inherited via ProjectItem
	virtual ProjectItemType GetItemType() override;
	virtual QString GetDisplayName() override;
	virtual QString GetBaseDir() override;

private:
	bool is_virtual;

	// Inherited via PropertySource
	virtual PropertyDefinitionCollection & PropertyGetDefinitions() override;
};

