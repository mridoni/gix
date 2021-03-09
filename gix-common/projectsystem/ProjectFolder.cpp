/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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
