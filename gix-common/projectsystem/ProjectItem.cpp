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

#include "ProjectItem.h"
#include "PathUtils.h"
#include "PropertyDefinition.h"
#include "PropertySource.h"

ProjectItem::ProjectItem()
{
	parent_item = nullptr;
	children = new QList<ProjectItem *>();
	is_virtual = false;
	guid = QUuid::createUuid();
}


ProjectItem::~ProjectItem()
{
	for (ProjectItem *c : *children) {
		delete c;
	}

	delete children;
}

QString ProjectItem::GetFileRelativePath()
{
	return filepath;
}

QString ProjectItem::GetFileFullPath()
{
	QString res = filepath;
	ProjectItem *cur = this;
	while (cur->parent_item != nullptr) {
		QString pfp = cur->parent_item->filepath;
		res = PathUtils::combine(PathUtils::getDirectory(cur->parent_item->filepath), res);
		cur = cur->parent_item;
	}
	return QDir::cleanPath(res);
}

QString ProjectItem::GetFilename()
{
	return PathUtils::getFilename(filepath);
}

QString ProjectItem::GetBaseDir()
{
	QString fullpath = GetFileFullPath();
	return PathUtils::getDirectory(fullpath);
}

bool ProjectItem::HasChildren()
{
	return children->size() > 0;
}

bool ProjectItem::isVirtual()
{
	return is_virtual;
}

void ProjectItem::setVirtual(bool f)
{
	is_virtual = f;
}

QList<ProjectItem*> *ProjectItem::GetChildren()
{
	return children;
}

ProjectItem * ProjectItem::GetParent()
{
	return parent_item;
}

void ProjectItem::SetParent(ProjectItem *p)
{
	parent_item = p;
}

int ProjectItem::getIndex()
{
	if (parent_item != nullptr) {
		return parent_item->children->indexOf((ProjectItem*) this);
	}
	return 0;
}

int ProjectItem::columnCount() const
{
	return 1;
}

QString ProjectItem::getGuid()
{
	return guid.toString();
}

bool ProjectItem::isPropertyVisible(PropertyDefinition *pd)
{
	if (!pd->show_depending_on)
		return true;

	return pd->show_depending_on(dynamic_cast<PropertySource *>(this));
}

