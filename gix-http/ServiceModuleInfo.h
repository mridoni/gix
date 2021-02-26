#pragma once

#include "ServerConfig.h"
#include "DataEntry.h"

#include <QList>

class ServiceModuleInfo
{
public:
	static ServiceModuleInfo *load(ServiceConfig *svc);

	DataEntry *ServiceModuleInfo::buildInterfaceEntryTree(const QString &root_field_id);
	bool containsEntry(const QString &name);
	DataEntry *getEntry(const QString &name);
	QList<DataEntry *>getEntryList() const;

	QMap<QString, DataEntry *> entries;

	static ServiceModuleInfo *extractSharedModuleInfo(const QString &shared_module);

private:
	void ServiceModuleInfo::add_tree_children(DataEntry *e);

};

