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

//#pragma once
//
//#include <QString>
//#include <QList>
//#include <QDateTime>
//#include <QThread>
//
////#include "ProjectCollection.h"
////#include "ModuleDebugInfo.h"
//
//class DataEntry;
//class ProjectFile;
//class ProjectItem;
//
//class MetadataLoader : public QThread
//{
//	friend class IdeTaskManager;
//
//	Q_OBJECT
//
//public:
//	MetadataLoader();
//	~MetadataLoader();
//
//	void setScanTarget(ProjectItem *pi);
//	void setConfiguration(QString c);
//	void setPlatform(QString p);
//
//	void run() override;
//
//signals:
//	void finishedUpdating(bool);
//
//private:
//	ProjectItem* scan_target;
//
//	QString config;
//	QString platform;
//
//	bool updateFileMetadata(ProjectFile* pf);
//	QString extract_program_id(const QString &filename);
//
//	bool is_running = false;
//
//	
//};
//
