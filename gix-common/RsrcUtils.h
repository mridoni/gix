#pragma once

#include <QString>

class RsrcUtils
{
public:
	static QString getAsString(QString rsrc_id);
	static QByteArray getAsByteArray(QString rsrc_id);
};

