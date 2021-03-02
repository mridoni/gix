#pragma once

#include <QObject>
#include <QLogger.h>

#include "IGixLogManager.h"

class IdeLogManager : public IGixLogManager
{
	Q_OBJECT

public:
	IdeLogManager();

	// Inherited via IGixLogManager
	virtual void logMessage(const QString &destination, const QString &msg, QLogger::LogLevel level) override;

private:
	QLogger::QLoggerManager *log_manager;
};

