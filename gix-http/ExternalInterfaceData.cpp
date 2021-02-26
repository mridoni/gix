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
