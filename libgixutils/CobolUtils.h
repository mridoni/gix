#pragma once

#include <QString>
#include <QSet>

class CobolUtils
{
public:
	static bool isReservedWord(QString s);
	static QString extractProgramId(const QString &filename);

private:

	static void init_reserved_words_set();
};
