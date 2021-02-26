#pragma once

#include <QThread>
#include <QList>

#include "Project.h"
#include "ProjectFile.h"

class ProjectDependencyTracker : public QThread
{
	Q_OBJECT

public:
	ProjectDependencyTracker(Project *);
	~ProjectDependencyTracker();

	QList<ProjectFile *> *getDependencies();

	void run() override;

signals:
	void finishedComputingDependencies();

private:
	Project *project;
	QList<ProjectFile *> *dependencies;
};

