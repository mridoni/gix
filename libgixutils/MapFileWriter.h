#pragma once

#include <QString>
#include <QStringList>
#include <QMap>

class MapFileWriter
{
public:
	void addSection(const QString &section_name, const QStringList &section_contents = QStringList());
	void setSectionContents(const QString &section_name, const QStringList &section_contents);
	void appendToSectionContents(const QString &section_name, const QStringList &more_contents);
	void appendToSectionContents(const QString &section_name, const QString &content);
	void appendToSectionContents(const QString &section_name, int content);

	bool writeToFile(const QString &filename);

private:
	QStringList sections;
	QMap<QString, QStringList> data;
};

