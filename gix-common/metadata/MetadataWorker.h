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

#pragma once

#include <QObject>
#include <QThread>
#include <QMutex>
#include <QWaitCondition>

class ProjectFile;
class CobolModuleMetadata;

class MetadataWorker : public QObject
{
    Q_OBJECT

    friend class MetadataManager;

public:
    explicit MetadataWorker(QObject *parent = nullptr);
    ~MetadataWorker();

    // resume() must be called from the outer thread.
    void resume();

    // suspend() must be called from the outer thread.
    // the function would block the caller's thread until
    // the worker thread is suspended.
    void suspend();

public slots:
    // this slot will be executed by event loop (one call at a time)
    void scanCobolModule(ProjectFile *pf);
    void scanModulesBatch(QList<ProjectFile *>);

private slots:
    void suspendImpl();

private:
    QMutex _waitMutex;
    QWaitCondition _waitCondition;

    QString extract_program_id(const QString &filename);

    CobolModuleMetadata *scanCobolModuleInternal(ProjectFile *pf);
};