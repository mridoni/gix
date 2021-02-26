#include "ProjectFolder.h"
#include "ProjectFile.h"
#include "ProjectFolderPropertyDefinitionCollection.h"

#include <QDir>

static ProjectFolderPropertyDefinitionCollection property_defs;

ProjectFolder::ProjectFolder(QString name, bool _is_virtual)
{
	is_virtual = _is_virtual;
	filepath = name;
}


ProjectFolder::~ProjectFolder()
{
}

bool ProjectFolder::CreateIfNotExists()
{
	QString fp = this->GetFileFullPath();
	QDir dir(fp);
	if (!dir.exists()) {
		return dir.mkdir(".");
	}
	return false;
}

void ProjectFolder::getFilesRecursively(QList<ProjectFile*>& files)
{
	for (ProjectItem *pi : *this->GetChildren()) {
		if (pi->GetItemType() == ProjectItemType::TFile) {
			files.append(static_cast<ProjectFile *>(pi));
		}

		if (pi->GetItemType() == ProjectItemType::TFolder) {
			static_cast<ProjectFolder *>(pi)->getFilesRecursively(files);
		}
	}
}

ProjectItemType ProjectFolder::GetItemType()
{
	return ProjectItemType::TFolder;
}

QString ProjectFolder::GetDisplayName()
{
	return filepath;
}

QString ProjectFolder::GetBaseDir()
{
	return GetFileFullPath();
}

PropertyDefinitionCollection & ProjectFolder::PropertyGetDefinitions()
{
	return property_defs;
}
