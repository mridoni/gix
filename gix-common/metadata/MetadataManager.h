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

