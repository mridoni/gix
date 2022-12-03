#pragma once

#include <QString>
#include <QList>

#include <spdlog/spdlog.h>

enum class ErrorWarningFilterType {
	Notice = 1,
	Warning = 2,
	Error = 3,

	ParagrapHeader = 100,
	SectionHeader = 101
};


struct ErrorWarningFilterEntry {

	ErrorWarningFilterType type;

	QString filename;
	int line = 0;
	
	QString section_or_paragraph;

	QString message;
};

class ErrorWarningFilter
{
public:

	QList<ErrorWarningFilterEntry> filter(QString payload, spdlog::level::level_enum level);
};

