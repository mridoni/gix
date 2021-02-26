#include "RsrcUtils.h"

#include <QFile>

QString RsrcUtils::getAsString(QString rsrc_id)
{
	QByteArray data = getAsByteArray(rsrc_id);
	return QString::fromUtf8(data);
}

QByteArray RsrcUtils::getAsByteArray(QString rsrc_id)
{
	QByteArray data;
	QString fileName(rsrc_id);

	QFile file(fileName);
	if (!file.open(QIODevice::ReadOnly)) {
		return QByteArray();
	}
	else
	{
		data = file.readAll();
	}

	file.close();
	return data;
}
