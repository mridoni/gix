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

#include "ExternalInterfaceData.h"
#include "ServerConfig.h"
#include "ServiceModuleInfo.h"

#include <QFileInfo>
#include <QDateTime>
#include <QLogger.h>

#include "PathUtils.h"
#include "SysUtils.h"
#include "DataEntry.h"
#include "SymbolBufferReader.h"


ExternalInterfaceData *ExternalInterfaceData::build(ServiceModuleInfo *smi, const QString &field_name)
{
	if (!smi->containsEntry(field_name))
		return nullptr;

	ExternalInterfaceData *ed = new ExternalInterfaceData();
	ed->entry_tree = smi->buildInterfaceEntryTree(field_name);
	ed->storage_size = ed->entry_tree->computeTotalStorageSize();

	return ed;
}

DataEntry *ExternalInterfaceData::getEntryTree() const
{
	return entry_tree;
}

int ExternalInterfaceData::getStorageSize()
{
	if (!entry_tree)
		return 0;

	return storage_size;
}
