#pragma once

#include "ServerConfig.h"
#include "DataEntry.h"
#include "gixcommon_global.h"

#include <QMap>

enum class ExternalInterfaceDataType
{
	In = 1,
	Out = 2
};

class ExternalInterfaceData;

class ExternalInterfaceData
{
public:

	static ExternalInterfaceData *build(ServiceModuleInfo *smi, const QString& field_name);

	DataEntry *getEntryTree() const;

	int getStorageSize();

private:
	int storage_size = 0;

	DataEntry *entry_tree = nullptr;
};

