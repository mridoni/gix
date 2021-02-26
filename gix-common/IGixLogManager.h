#pragma once


#include <QString>
#include <QLogger.h>

class IGixLogManager : public QObject
{
public:
	virtual void logMessage(const QString& destination , const QString &msg, QLogger::LogLevel level) = 0;
};