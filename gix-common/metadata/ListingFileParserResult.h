#pragma once

#include <QList>
#include <QMap>
#include <QString>
#include <QStringList>
#include <QDateTime>

#include "DataEntry.h"

class ListingFileParserResult
{
public:

	QDateTime last_parsed;
	QStringList copy_deps;

	QString module_name;
	QString linkage_section_text;
	QList<DataEntry *> ws_entries;
	QList<DataEntry *> ls_entries;
	QList<DataEntry *> fs_entries;
	QMap<QString, Paragraph *> paragraphs;
};

