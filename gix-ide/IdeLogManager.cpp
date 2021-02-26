#include "IdeLogManager.h"
#include "Ide.h"
#include "IdeTaskManager.h"

IdeLogManager::IdeLogManager()
{
	log_manager = QLogger::QLoggerManager::getInstance();
	log_manager->addDestination("virtual://", GIX_CONSOLE_LOG, QLogger::LogLevel::Debug);
	
	connect(log_manager, &QLogger::QLoggerManager::logMessage, Ide::TaskManager(), &IdeTaskManager::logMessage);
}

void IdeLogManager::logMessage(const QString &destination, const QString &msg, QLogger::LogLevel level)
{
	Ide::TaskManager()->logMessage(destination, msg, level);
}
