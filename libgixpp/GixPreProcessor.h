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

	QVariant getOpt(QString id);
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

