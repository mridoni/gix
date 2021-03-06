#include "Ide.h"
#include "SysUtils.h"

#include "IdeTaskManager.h"
#include "IdeSearchManager.h"
#include "CompilerManager.h"
#include "IdeDbManager.h"
#include "TargetManager.h"
#include "GixGlobals.h"
#include "UiUtils.h"

#include <QDir>
#include <QStandardPaths>

GixGlobalsCallbacks Ide::ide_callbacks;

static IdeTaskManager *ide_task_manager;
static IdeSearchManager *ide_search_manager;
static IdeDbManager *db_manager;

Ide::Ide()
{
}

Ide::~Ide()
{
}

void Ide::init()
{
	ide_task_manager = new IdeTaskManager();
	ide_search_manager = new IdeSearchManager();
	db_manager = new IdeDbManager();

	GixGlobals::initManagers();

	ide_callbacks.getCurrentConfiguration = []() { return ide_task_manager->getCurrentConfiguration(); };
	ide_callbacks.getCurrentPlatform = []() { return ide_task_manager->getCurrentPlatform(); };
	ide_callbacks.getCurrentProjectCollection = []() { return ide_task_manager->getCurrentProjectCollection(); };
	GixGlobals::registerCallbacks(&ide_callbacks);

	if (GixGlobals::getCompilerManager()->getCompilers().size() == 0) {
		QString msg = QCoreApplication::translate("gix", QString("No configured compilers found in compiler definitions directory %1").arg(GixGlobals::getCompilerDefsDir()).toUtf8().constData());
		GixGlobals::getLogManager()->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Error);
		UiUtils::ErrorDialog(msg);
	}
}

IdeTaskManager *Ide::TaskManager()
{
	return ide_task_manager;
}

IdeSearchManager *Ide::SearchManager()
{
	return ide_search_manager;
}

IdeDbManager* Ide::DbManager()
{
	return db_manager;
}

EolMode Ide::getEolModeFromSettings()
{
	QSettings settings;
	if (settings.contains("default_eol_mode")) {
		EolMode m = settings.value("default_eol_mode").value<EolMode>();
		if (m != EolMode::PlatformDefault)
			return m;
	}

#if defined(Q_OS_WIN)
	return EolMode::Windows;
#elif defined(Q_OS_LINUX)
	return EolMode::Unix;
#elif defined(Q_OS_MAC)
	return EolMode::ClassicMacOS;
#else
#error "Unknown platform"
#endif
}