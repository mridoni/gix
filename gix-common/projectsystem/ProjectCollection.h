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

#pragma once

#include <QString>
#include <QList>
#include <QAbstractItemModel>

#include "Project.h"
#include "IBuildableItem.h"

class Project;

class ProjectCollection : public ProjectItem, public PropertySource, public IPersistableProjectItem, public IBuildableItem
{

public:
	GIXCOMMON_EXPORT ProjectCollection();
	GIXCOMMON_EXPORT virtual ~ProjectCollection();

	GIXCOMMON_EXPORT static int getLoadableItemCount(QString);
	GIXCOMMON_EXPORT static ProjectCollection* newProjectCollection(ProjectType add_main_project_type, QString ppj_filepath, const QVariantMap opts);

	GIXCOMMON_EXPORT ProjectFile *locateProjectFileByPath(QString, bool include_copy_files = false);
	
	GIXCOMMON_EXPORT int getLoadableItemCount();
	GIXCOMMON_EXPORT void addProject(Project* prj);

	// Inherited via ProjectItem
	GIXCOMMON_EXPORT virtual ProjectItemType GetItemType() override;
	GIXCOMMON_EXPORT virtual QString GetDisplayName() override;

	// Inherited via IPropertySource
	GIXCOMMON_EXPORT virtual PropertyDefinitionCollection& PropertyGetDefinitions() override;

	// Inherited via IPersistableProjectItem
	GIXCOMMON_EXPORT virtual bool load(ProjectItem * owner, QString filepath) override;
	GIXCOMMON_EXPORT virtual bool save(ProjectItem* owner = nullptr, QString filepath = QString()) override;
	GIXCOMMON_EXPORT virtual bool revert() override;
	GIXCOMMON_EXPORT virtual bool isDirty() override;
	GIXCOMMON_EXPORT virtual void setDirty(bool b) override;

	// Inherited via IBuildableItem
	GIXCOMMON_EXPORT virtual BuildTarget *getBuildTarget(QMap<QString, QVariant>, BuildTarget *parent) override;
	GIXCOMMON_EXPORT virtual QList<IBuildableItem *> getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership) override;

private:

	void dump_children(QDomNode&, ProjectItem *);

};

