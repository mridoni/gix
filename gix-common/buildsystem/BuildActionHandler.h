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

