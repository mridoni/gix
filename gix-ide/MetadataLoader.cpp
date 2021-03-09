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

//#include "MetadataLoader.h"
//#include "ListingFileParser.h"
//#include "Ide.h"
//#include "IdeTaskManager.h"
//#include "PathUtils.h"
//#include "SysUtils.h"
//#include "linq/linq.hpp"
//
//#include <QFile>
//#include <QDir>
//#include <GixPreProcessor.h>
//
//static QRegularExpression rxUserDefinedCobolWord(R"(^[A-Za-z0-9]+([\-]+[A-Za-z0-9]+)*$)");
//
//MetadataLoader::MetadataLoader()
//{
//	scan_target = nullptr;
//	connect(this, &QThread::finished, this, &QThread::deleteLater);
//}
//
//
//MetadataLoader::~MetadataLoader()
//{
//
//}
//
//void MetadataLoader::setScanTarget(ProjectItem* pi)
//{
//	scan_target = pi;
//}
//
//void MetadataLoader::setConfiguration(QString c)
//{
//	config = c;
//}
//
//void MetadataLoader::setPlatform(QString p)
//{
//	platform = p;
//}
//
//void MetadataLoader::run()
//{
//	if (is_running || !Ide::TaskManager()->backgroundTasksEnabled())
//		return;
//
//	if (!scan_target)
//		return;
//
//	is_running = true;
//
//	QList<ProjectFile*> files;
//
//	switch (scan_target->GetItemType()) {
//		case ProjectItemType::TProjectCollection:
//			{
//				ProjectCollection* prj_coll = dynamic_cast<ProjectCollection*>(scan_target);
//				auto prjs = cpplinq::from(*(prj_coll->GetChildren())).where([](ProjectItem* a) { return a->GetItemType() == ProjectItemType::TProject;  }).to_vector();
//				for (ProjectItem* ppi : prjs) {
//					Project* prj = (Project*)ppi;
//					files.append(prj->getAllCompilableFiles());
//				}
//			}
//			break;
//
//		case ProjectItemType::TProject:
//			{
//				Project *prj = dynamic_cast<Project *>(scan_target);
//				files.append(prj->getAllCompilableFiles());
//			}
//			break;
//
//		case ProjectItemType::TFile:
//			{
//				ProjectFile* pf = dynamic_cast<ProjectFile*>(scan_target);
//				if (pf->isCompilable())
//					files.append(pf);
//			}
//			break;
//
//		default:
//			is_running = false;
//			return;
//	}
//
//	for (ProjectFile* pf : files) {
//		bool b = updateFileMetadata(pf);
//	}
//
//	emit finishedUpdating(true);	// TODO: should check if metadata really changed
//	
//	is_running = false;
//}
//
//bool MetadataLoader::updateFileMetadata(ProjectFile* pf)
//{
//	if (!pf)
//		return false;
//
//	QString program_id = extract_program_id(pf->GetFileFullPath());
//	if (program_id.isEmpty())
//		return false;
//
//
//	CobolModuleMetadata *metadata = Ide::TaskManager()->getModuleMetadata(program_id);
//
//	//if (metadata) // && metadata->isUpToDate()
//	//	return true;
//
//	//Project* prj = pf->getParentProject();
//	//if (!prj)
//	//	return false;
//
//	CobolModuleMetadata* cfm = CobolModuleMetadata::loadFromFile(pf->getSymbolFilename());
//	if (cfm)
//		Ide::TaskManager()->setModuleMetadata(pf, cfm);
//
//	return true;
//}
//
//QString MetadataLoader::extract_program_id(const QString &filename)
//{
//	if (filename.isEmpty() || ! QFile::exists(filename))
//		return QString();
//
//	QList<QString> lines = SysUtils::FileReadLines(filename, 20);
//	for (QString ln : lines) {
//		if (ln.contains("PROGRAM-ID")) {
//			QString pid = ln.replace("PROGRAM-ID", "");
//			pid = pid.replace(".", "").trimmed();
//			return pid;
//		}
//	}
//
//	return QString();
//}
