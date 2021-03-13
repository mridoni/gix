/*
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/


#pragma once

#include "ServerConfig.h"
#include "DataEntry.h"

#include <QList>

class ServiceModuleInfo
{
public:
	static ServiceModuleInfo *load(ServiceConfig *svc);

	DataEntry *buildInterfaceEntryTree(const QString &root_field_id);
	bool containsEntry(const QString &name);
	DataEntry *getEntry(const QString &name);
	QList<DataEntry *>getEntryList() const;

	QMap<QString, DataEntry *> entries;

	static ServiceModuleInfo *extractSharedModuleInfo(const QString &shared_module);

private:
	void add_tree_children(DataEntry *e);

};

