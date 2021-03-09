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
#include <QVariant>
#include <QMap>
#include <QUuid>

#include "ProjectItemType.h"
#include "gixcommon_global.h"

class PropertyDefinition;

class GIXCOMMON_EXPORT ProjectItem
{

public:
	ProjectItem();
	virtual ~ProjectItem();

	QString GetFileRelativePath();
	QString GetFileFullPath();
	QString GetFilename();

	virtual QString GetBaseDir();

	bool HasChildren();

	bool isVirtual();
	void setVirtual(bool);

	QList<ProjectItem *> *GetChildren();
	ProjectItem *GetParent();
	void SetParent(ProjectItem *);

	int getIndex();
	int columnCount() const;

	QString getGuid();

	virtual bool isPropertyVisible(PropertyDefinition *);

	virtual ProjectItemType GetItemType() = 0;
	virtual QString GetDisplayName() = 0;
	

protected:
	ProjectItem *parent_item;
	QString filepath;
	QList<ProjectItem *> *children;
	bool is_virtual;

	QUuid guid;
};

