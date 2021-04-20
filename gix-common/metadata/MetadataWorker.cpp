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

	QList<QString> lines = SysUtils::FileReadLines(filename, 20);
	for (QString ln : lines) {
		if (ln.contains("PROGRAM-ID")) {
			QString pid = ln.replace("PROGRAM-ID", "");
			pid = pid.replace(".", "").trimmed();
			return pid;
		}
	}

	return QString();
}

CobolModuleMetadata *MetadataWorker::scanCobolModuleInternal(ProjectFile *pf)
{
	if (!pf)
		return nullptr;

	GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, "Scanning " + pf->GetFileFullPath(), QLogger::LogLevel::Trace);

	if (!QFile::exists(pf->GetFileFullPath())) {
		GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, "No such file " + pf->GetFileFullPath(), QLogger::LogLevel::Trace);
		return nullptr;
	}

	QString program_id = extract_program_id(pf->GetFileFullPath());
	if (program_id.isEmpty()) {
		GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, "Invalid program ID in " + pf->GetFileFullPath(), QLogger::LogLevel::Trace);
		return nullptr;
	}

	CobolModuleMetadata *metadata = GixGlobals::getMetadataManager()->getModuleMetadata(program_id);

	if (metadata && metadata->isUpToDate()) {
		GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, QString("Metadata for module %1 (%2) is up to date").arg(program_id).arg(pf->GetFileFullPath()), QLogger::LogLevel::Trace);
		return metadata;
	}

	Project *prj = pf->getParentProject();

	GixPreProcessor gp;
	CopyResolver copy_resolver;

	QStringList copy_dirs = prj->getCopyDirList();

	if (prj->isEsql()) {
		QSettings settings;
		QString configuration = GixGlobals::getCurrentConfiguration();
		QString platform = GixGlobals::getCurrentPlatform();
		QScopedPointer<CompilerConfiguration> ccfg(CompilerConfiguration::get(configuration, platform));
		
		QString esql_cfg_id = settings.value("esql_preprocessor_id", ESQLConfigurationType::GixInternal).toString();
		CompilerEnvironment esql_cfg_env = ccfg.get()->getCompilerEnvironment();
		QScopedPointer<ESQLConfiguration> esql_cfg(ESQLConfiguration::get(esql_cfg_id, esql_cfg_env, configuration, platform));
		if (!esql_cfg.isNull()) {
			QStringList esql_copy_dirs = esql_cfg->getCopyPathList();
			if (!esql_copy_dirs.isEmpty())
				copy_dirs.append(esql_copy_dirs);
		}
	}

	copy_resolver.setCopyDirs(copy_dirs);
	copy_resolver.setExtensions(prj->getCopyExtList());
	gp.setCopyResolver(&copy_resolver);

	gp.setOpt("no_output", true);
	gp.setOpt("preprocess_copy_files", true);

	gp.setOpt("emit_static_calls", prj->PropertyGetValue("esql_static_calls", true));
	gp.setOpt("anonymous_params", prj->PropertyGetValue("esql_anon_params", true));

	TPESQLProcessing *pp = new TPESQLProcessing(&gp);
	gp.addStep(pp);

	gp.setOpt("emit_debug_info", true);
	gp.verbose = false;
	gp.verbose_debug = false;

	gp.setInputFile(pf->GetFileFullPath());
	gp.setOutputFile("");

	bool b = gp.process();
	if (!b) {
		GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, QString("Error while retrieving metadata for module %1 (%2)").arg(program_id).arg(gp.err_code), QLogger::LogLevel::Trace);
		for (auto errmsg : gp.err_messages) {
			GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, "   " + errmsg, QLogger::LogLevel::Trace);
		}
		return nullptr;
	}

	CobolModuleMetadata *cmm = GixGlobals::getMetadataManager()->getModuleMetadata(program_id);
	if (cmm) {
		emit GixGlobals::getMetadataManager()->invalidateModuleMetadata(program_id, pf);
		GixGlobals::getMetadataManager()->removeModuleMetadata(program_id);
	}

	cmm = CobolModuleMetadata::build(pf, pp);
	if (!cmm) {
		GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, QString("Error while building metadata for module %1 (%2)").arg(program_id).arg(gp.err_code), QLogger::LogLevel::Trace);
		for (auto errmsg : gp.err_messages) {
			GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, "   " + errmsg, QLogger::LogLevel::Trace);
		}
		return nullptr;
	}

	GixGlobals::getMetadataManager()->addModuleMetadata(cmm);

	return cmm;
}
