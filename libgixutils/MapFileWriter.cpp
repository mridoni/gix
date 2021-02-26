#include "MapFileWriter.h"
#include "PathUtils.h"

#include <QFile>
#include <QTextStream>

void MapFileWriter::addSection(const QString &section_name, const QStringList &section_contents)
{
    sections.append(section_name);
    data[section_name] = section_contents;
}

void MapFileWriter::setSectionContents(const QString & section_name, const QStringList & section_contents)
{
    data[section_name] = section_contents;
}

void MapFileWriter::appendToSectionContents(const QString & section_name, const QStringList & more_contents)
{
    QStringList cur_contents = data.find(section_name) == data.end() ? QStringList() : data[section_name];
    cur_contents.append(more_contents);
    data[section_name] = cur_contents;
}

void MapFileWriter::appendToSectionContents(const QString & section_name, const QString & content)
{
    QStringList cur_contents = data.find(section_name) == data.end() ? QStringList() : data[section_name];
    cur_contents.append(content);
    data[section_name] = cur_contents;
}

void MapFileWriter::appendToSectionContents(const QString &section_name, int content)
{
    QStringList cur_contents = data.find(section_name) == data.end() ? QStringList() : data[section_name];
    cur_contents.append(QString::number(content));
    data[section_name] = cur_contents;
}

bool MapFileWriter::writeToFile(const QString & filename)
{
    QFile f(filename);
    if (!f.open(QIODevice::WriteOnly | QIODevice::Text))
        return false;

    QTextStream ts(&f);

    for (QString section_name : sections) {
        ts << QString("[%1]\n").arg(section_name.trimmed());
        for (QString ln : data[section_name]) {
            ts << QString("%1\n").arg(ln.trimmed());
        }
        ts << "\n";
    }

    f.close();

    return true;
}
