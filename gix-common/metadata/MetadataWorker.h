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