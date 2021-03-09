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

#pragma once

#include <QObject>
#include <QThread>
#include <QMap>

#include "MetadataWorker.h"
#include "gixcommon_global.h"

class ProjectFile;
class CobolModuleMetadata;

class GIXCOMMON_EXPORT MetadataManager
    : public QObject
{
    Q_OBJECT

public:
    explicit MetadataManager(QObject *parent = nullptr);
    ~MetadataManager();

    CobolModuleMetadata *getModuleMetadata(QString module_name);
    CobolModuleMetadata *getModuleMetadataBySource(QString src_filename);
    bool removeModuleMetadata(QString module_name);
    bool addModuleMetadata(CobolModuleMetadata *cmm);

    

signals:
    // to use the service, just call this signal to send a request:
    // logService->logEvent("event");
    void logEvent(const QString &event);
    void scanCobolModule(ProjectFile *pf);
    void invalidateModuleMetadata(const QString& program_id, ProjectFile *pf);
    void updatedModuleMetadata(CobolModuleMetadata *cmm);
    
    void scanModulesBatch(QList<ProjectFile *>);
    void updatedModuleMetadataBatch(bool res);

private:
    QThread *thread;
    MetadataWorker *worker;

    QMap<QString, CobolModuleMetadata *> by_module_map;
    QMap<QString, CobolModuleMetadata *> by_filename_map;
    //QList<CobolModuleMetadata *> metadata;
};

