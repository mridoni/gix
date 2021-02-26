#include "MapFileReader.h"
#include "SysUtils.h"
#include "linq/linq.hpp"

#include <QQueue>
#include <QRegularExpression>

static QRegularExpression rxSectionName("^\[(.*)\]$");

MapFileReader::MapFileReader(const QString &_filename)
{
    filename = _filename;
}

MapFileReader::~MapFileReader()
{}

bool MapFileReader::getSectionData(const QString &section_name, QStringList &items) const
{
    if (!sections.contains(section_name) || !data.contains(section_name))
        return false;

    items = data[section_name];
    return true;
}

bool MapFileReader::read()
{
    if (!QFile(filename).exists())
        return false;

    QStringList lines = SysUtils::FileReadAllLines(filename);
    if (!lines.size())
        return false;

    int idx = 0;
    QString cur_section = "";
    QStringList cur_section_contents;

    while(idx < lines.length()) {
        QString ln = lines[idx++];

        if (ln.startsWith("[")) {
            cur_section = ln.mid(1).chopped(1);
            continue;
        }

        if (ln.trimmed() == "") {
            if (cur_section != "") {
                sections.append(cur_section);
                data[cur_section] = cur_section_contents;
                cur_section = "";
                cur_section_contents.clear();
            }
            continue;
        }

        if (cur_section != "") {
            cur_section_contents.append(ln);
        }
    }

    return true;

}
