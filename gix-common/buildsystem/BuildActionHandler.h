#pragma once

#include <QMap>
#include <QVariant>
#include <QString>
#include <QProcess>

#include "BuildTarget.h"
#include "BuildDriver.h"

class BuildActionHandler : public QObject
{
	Q_OBJECT

public:
	~BuildActionHandler();


	void addEnvironment(QMap<QString, QVariant>&);
	void addEnvironment(QString s, QVariant v);
	void setMainBuilder(BuildDriver *);

	static BuildActionHandler *get(BuildTarget *);
	
	virtual bool startBuild() = 0;

signals:
	void finished(int, QProcess::ProcessState);

protected:
	
	QMap<QString, QVariant> environment;
	BuildDriver *build_driver;
	BuildTarget *target;

	QString getBuildDirectory();
	void importProjectEnvironment();
	void importFileEnvironment();

public slots:
	void readStdOut(QProcess *p);
	void readStdErr(QProcess *p);
};

