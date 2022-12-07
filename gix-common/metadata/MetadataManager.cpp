/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
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

#include "MetadataManager.h"
#include "GixGlobals.h"
#include "linq/linq.hpp"
#include "CobolUtils.h"
#include "PathUtils.h"

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

void MetadataManager::get_module_sources(QString module_name_filter)
{
	ProjectCollection *prj_coll = GixGlobals::getCurrentProjectCollection();
	if (!prj_coll)
		return;

	auto prjs = cpplinq::from(*(prj_coll->GetChildren())).where([](ProjectItem *a) { return a->GetItemType() == ProjectItemType::TProject;  }).to_vector();
	for (ProjectItem *ppi : prjs) {
		Project *prj = (Project *)ppi;

		for (ProjectFile *pf : prj->getAllCompilableFiles()) {
			QString program_id = CobolUtils::extractProgramId(pf->GetFileFullPath());
			if (!program_id.isEmpty() && (module_name_filter.isEmpty() || module_name_filter == program_id))
				module_srcs.insert(program_id, pf);
		}
	}
}

CobolModuleMetadata *MetadataManager::getModuleMetadata(QString module_name)
{
#ifdef _DEBUG
    QStringList mmap_list;
    for (auto mm : by_module_map.keys()) {
        mmap_list.append("[" + mm + "] -> [" + by_module_map.value(mm)->getModuleName() + "]");
    }

	GixGlobals::getLogManager()->debug(LOG_IDE, "MetadataManager: looking for {}, module list is: {}", module_name, mmap_list.join(", "));
#endif
    if (by_module_map.contains(module_name))
		return by_module_map.value(module_name);

	return nullptr;
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

CobolModuleMetadata *MetadataManager::getModuleMetadataByRunningSource(QString src_filename, bool ignore_project_path)
{
	QString f = QDir::cleanPath(src_filename);
	if (ignore_project_path) {
		f = PathUtils::getFilename(f);
	}
	for (auto mm : by_module_map) {
		QString r = mm->runningFile();
		if (ignore_project_path) {
			r = PathUtils::getFilename(r);
			if (r.startsWith("#") && r.size() > 1)
				r = r.mid(1);
		}

		if (r == f) {
			return mm;
		}
	}
	return nullptr;
}

bool MetadataManager::removeModuleMetadata(QString module_name)
{
	if (!by_module_map.contains(module_name))
		return true;

	CobolModuleMetadata *cmm = by_module_map.value(module_name);
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

bool MetadataManager::existsMetadata(const QString &module_name)
{
	return by_module_map.contains(module_name);
}

const QMap<QString, ProjectFile *>& MetadataManager::getModulesSourceMap()
{
	return module_srcs;
}

CobolModuleMetadata* MetadataManager::findFirstModuleWithCopy(QString copy_filename)
{
	copy_filename = QDir::cleanPath(copy_filename);

	for (auto m : by_module_map.values()) {

		auto dep_entries = m->getFileDependencies();
		for (auto dep_entry : dep_entries) {
			for (auto sub_entry : dep_entry) {
				if (QDir::cleanPath(sub_entry) == copy_filename)
					return m;
			}
		}
	}

	return nullptr;
}

void MetadataManager::reset()
{
	by_module_map.clear();
	by_filename_map.clear();
	module_srcs.clear();

	get_module_sources();
}

bool MetadataManager::rebuildMetadata(QString module_name)
{
	ProjectCollection *prj_coll = GixGlobals::getCurrentProjectCollection();
	QList<ProjectFile *> pfiles;

	if (!prj_coll)
		return false;

	get_module_sources(module_name);
	if (!module_srcs.values().size())
		return true;

	bool res;
	
	QEventLoop *loop = new QEventLoop();
	auto c = connect(GixGlobals::getMetadataManager(), &MetadataManager::updatedModuleMetadataBatch, this, [this, &res, loop](bool b) {
		loop->quit();
		loop->deleteLater();
		res = b;
	}, Qt::ConnectionType::QueuedConnection);

	auto logger = GixGlobals::getLogManager();

	logger->trace(LOG_METADATA, "Starting to rebuild metadata");
	emit GixGlobals::getMetadataManager()->scanModulesBatch(module_srcs.values());

	loop->exec();
	
	disconnect(c);
	logger->trace(LOG_METADATA, "Finished rebuilding metadata");

	return res;

}
