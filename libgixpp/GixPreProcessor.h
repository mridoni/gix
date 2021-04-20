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

#include <QString>
#include <QMap>
#include <QStringList>
#include <QDateTime>
#include <QVariantMap>

#include "ITransformationStep.h"
#include "CopyResolver.h"

class FileData;

class GixPreProcessor
{
public:
	GixPreProcessor();
	~GixPreProcessor();

	bool check_update_status;

	bool keep_temp_files;
	bool verbose;
	bool verbose_debug;

	//const QStringList getCopyDirs();
	//void setCopyDirs(const QStringList cdl);

	void setCopyResolver(const CopyResolver *cr);
	CopyResolver *getCopyResolver() const;

	void addCustomStep(ITransformationStep *stp);

	int err_code;
	QStringList err_messages;

	bool process();
	
	void addStep(ITransformationStep *);
	bool setInputFile(QString infile);
	bool setOutputFile(QString outfile);

	QVariant getOpt(QString id, QVariant v = QVariant());
	void setOpt(QString id, QVariant v);

private:
	QString input_file;
	QString output_file;

	QList< ITransformationStep *> steps;
	QString copy_file_path;
	QStringList copy_dirs;

	QVariantMap opts;

	CopyResolver *copy_resolver;

	bool transform();
};

