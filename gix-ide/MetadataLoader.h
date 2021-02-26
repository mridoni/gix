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
