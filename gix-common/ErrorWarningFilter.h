#pragma once

#include <QString>
#include <QList>

#include <spdlog/spdlog.h>

#include "gixcommon_global.h"

enum class GIXCOMMON_EXPORT ErrorWarningFilterType {
	Notice = 1,
	Warning = 2,
	Error = 3,

	ParagrapHeader = 100,
	SectionHeader = 101
};


struct GIXCOMMON_EXPORT ErrorWarningFilterEntry {

	ErrorWarningFilterType type;

	QString filename;
	int line = 0;
	
	QString section_or_paragraph;

	QString message;
};

class GIXCOMMON_EXPORT ErrorWarningFilter
{
public:

	QList<ErrorWarningFilterEntry> filter(QString payload, spdlog::level::level_enum level);

	static bool isWarning(const QString& msg);
	static bool isError(const QString& msg);
};

