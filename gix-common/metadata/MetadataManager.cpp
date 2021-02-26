#include "MetadataManager.h"
#include "GixGlobals.h"
#include "linq/linq.hpp"

MetadataManager::MetadataManager(QObject *parent) : QObject(parent)
{
	thread = new QThread(this);
	worker = new MetadataWorker();
	worker->moveToThread(thread);

	connect(this, &MetadataManager::scanCobolModule, worker, &MetadataWorker::scanCobolModule);

	connect(this, &MetadataManager::scanModulesBatch, worker, &MetadataWorker::scanModulesBatch);

	connect(thread, &QThread::finished, worker, &QObject::deleteLater);
	thread->start();

}

MetadataManager::~MetadataManager()
{
	thread->quit();
	thread->wait();
}

CobolModuleMetadata *MetadataManager::getModuleMetadata(QString module_name)
{
	return (by_module_map.contains(module_name)) ? by_module_map.value(module_name) : nullptr;
}

CobolModuleMetadata *MetadataManager::getModuleMetadataBySource(QString src_filename)
{
#if !_WIN32
	return (by_filename_map.contains(src_filename)) ? by_filename_map.value(src_filename) : nullptr;
#else
	for (QString k : by_filename_map.keys()) {
		if (k.toUpper() == src_filename.toUpper())
			return by_filename_map[k];
	}
	return nullptr;
#endif
}

bool MetadataManager::removeModuleMetadata(QString module_name)
{
	CobolModuleMetadata *cmm = getModuleMetadata(module_name);
	if (!cmm)
		return false;

	QString filename = cmm->originalFile();
	by_filename_map.remove(filename);
	by_module_map.remove(module_name);
	delete cmm;

	return true;
}

bool MetadataManager::addModuleMetadata(CobolModuleMetadata *cmm)
{
	if (!cmm)
		return false;

	if (by_module_map.contains(cmm->getModuleName()) || by_filename_map.contains(cmm->originalFile()))
		return false;

	by_module_map.insert(cmm->getModuleName(), cmm);
	by_filename_map.insert(cmm->originalFile(), cmm);

	return true;
}
