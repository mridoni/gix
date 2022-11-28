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

#include "MetadataWorker.h"
#include "ProjectFile.h"
#include "SysUtils.h"
#include "GixGlobals.h"
#include "GixPreProcessor.h"
#include "CopyResolver.h"
#include "TPESQLProcessing.h"
#include "MetadataManager.h"
#include "ESQLConfiguration.h"
#include "CompilerConfiguration.h"

#include <QFile>

MetadataWorker::MetadataWorker(QObject *parent) : QObject(parent)
{
	_waitMutex.lock();
}

MetadataWorker::~MetadataWorker()
{
	_waitCondition.wakeAll();
	_waitMutex.unlock();
}

void MetadataWorker::resume()
{
	_waitCondition.wakeAll();
}

void MetadataWorker::suspend()
{
	QMetaObject::invokeMethod(this, &MetadataWorker::suspendImpl);
	// acquiring mutex to block the calling thread
	_waitMutex.lock();
	_waitMutex.unlock();
}

void MetadataWorker::scanModulesBatch(QList<ProjectFile *> pfiles)
{
	bool res = true;
	for (ProjectFile *pf : pfiles) {
		res |= (scanCobolModuleInternal(pf) != nullptr);
	}

	emit GixGlobals::getMetadataManager()->updatedModuleMetadataBatch(res);
}

void MetadataWorker::scanCobolModule(ProjectFile *pf)
{
	CobolModuleMetadata *cmm = scanCobolModuleInternal(pf);
	if (cmm)
		emit GixGlobals::getMetadataManager()->updatedModuleMetadata(cmm);
}

void MetadataWorker::suspendImpl()
{
	_waitCondition.wait(&_waitMutex);
}

QString MetadataWorker::extract_program_id(const QString &filename)
{
	if (filename.isEmpty() || !QFile::exists(filename))
		return QString();

	QList<QString> lines = SysUtils::FileReadLines(filename, 250);
	for (QString ln : lines) {
		if (ln.contains("PROGRAM-ID", Qt::CaseSensitivity::CaseInsensitive)) {
			QString pid = ln.replace("PROGRAM-ID", "", Qt::CaseSensitivity::CaseInsensitive);
			pid = pid.replace(".", "").trimmed();
			return pid;
		}
	}

	return QString();
}

CobolModuleMetadata *MetadataWorker::scanCobolModuleInternal(ProjectFile *pf)
{
	bool metadata_was_built = false;

	if (!pf)
		return nullptr;

	QString configuration = GixGlobals::getCurrentConfiguration();
	QString platform = GixGlobals::getCurrentPlatform();

	GixGlobals::getLogManager()->trace(LOG_METADATA, "Scanning {}", pf->GetFileFullPath(), spdlog::level::trace);

	if (!QFile::exists(pf->GetFileFullPath())) {
		GixGlobals::getLogManager()->trace(LOG_METADATA, "No such file {}", pf->GetFileFullPath(), spdlog::level::trace);
		return nullptr;
	}

	QString program_id = extract_program_id(pf->GetFileFullPath());
	if (program_id.isEmpty()) {
		GixGlobals::getLogManager()->trace(LOG_METADATA, "Invalid program ID in {}", pf->GetFileFullPath(), spdlog::level::trace);
		return nullptr;
	}

	bool metadata_exists = GixGlobals::getMetadataManager()->existsMetadata(program_id);
	if (metadata_exists) {
		CobolModuleMetadata *metadata = GixGlobals::getMetadataManager()->getModuleMetadata(program_id);
		if (metadata && metadata->isUpToDate()) {
			GixGlobals::getLogManager()->trace(LOG_METADATA, "Metadata for module {} ({}) is up to date", program_id, pf->GetFileFullPath(), spdlog::level::trace);
			return metadata;
		}
	}	
	
	if (metadata_exists) {
		emit GixGlobals::getMetadataManager()->invalidateModuleMetadata(program_id, pf);
		GixGlobals::getMetadataManager()->removeModuleMetadata(program_id);
	}

	Project *prj = pf->getParentProject();

	ErrorData err_data;
	CobolModuleMetadata *cmm = CobolModuleMetadata::build(pf, &err_data);
	if (!cmm) {
		GixGlobals::getLogManager()->trace(LOG_METADATA, "Error while building or retrieving metadata for module {} ({})", program_id, err_data.err_code, spdlog::level::trace);
		for (auto errmsg : err_data.err_messages) {
			GixGlobals::getLogManager()->trace(LOG_METADATA, "   {}", errmsg);
		}
		return nullptr;
	}

	GixGlobals::getMetadataManager()->addModuleMetadata(cmm);
#if _DEBUG
	QString dump_file = PathUtils::combine(prj->getBuildDirectory(configuration, platform), cmm->getModuleName() + ".sym");
	cmm->dumpToFile(dump_file, true);
#endif

    GixGlobals::getLogManager()->trace(LOG_METADATA, "Finished scanning {}", pf->GetFileFullPath(), spdlog::level::trace);

	return cmm;
}
