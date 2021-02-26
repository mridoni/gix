#pragma once

#include <QString>
#include <QStringList>
#include <QMap>

class MapFileReader
{
public:
	MapFileReader(const QString &filename);
	~MapFileReader();

	bool read();
	bool getSectionData(const QString &section_name, QStringList &items) const;

private:
	QString filename;
	QStringList sections;
	QMap<QString, QStringList> data;
};

