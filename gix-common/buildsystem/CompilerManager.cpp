#include "CompilerManager.h"
#include "PathUtils.h"
#include "IGixLogManager.h"
#include "GixGlobals.h"

#include <QDir>
#include <QFile>


CompilerManager::CompilerManager()
{
	init();
}

CompilerManager::~CompilerManager()
{
	cleanup();
}

void CompilerManager::init()
{
	IGixLogManager *logger = GixGlobals::getLogManager();

	logger->logMessage(GIX_CONSOLE_LOG, "Initializing compiler manager", QLogger::LogLevel::Info);

	if (compiler_defs.size() > 0)
		cleanup();

	logger->logMessage(GIX_CONSOLE_LOG, "Compiler base directory is " + GixGlobals::getCompilerBaseDir(), QLogger::LogLevel::Debug);

	QString cdir = PathUtils::combine(GixGlobals::getCompilerBaseDir(), "defs");
	QDir compiler_defs_dir(QDir::fromNativeSeparators(cdir));
	compiler_defs_dir.setNameFilters(QStringList("*.def"));

	for (QFileInfo def_file : compiler_defs_dir.entryInfoList(QDir::NoDotAndDotDot | QDir::Files)) {
		CompilerDefinition* cd = CompilerDefinition::load(def_file.absoluteFilePath());
		if (cd == nullptr) {
			logger->logMessage(GIX_CONSOLE_LOG, "Cannot parse compiler definition " + def_file.absoluteFilePath(), QLogger::LogLevel::Error);
			continue;
		}


		compiler_defs[cd->getId()] = cd;
	}
}

QMap<QString, CompilerDefinition*> CompilerManager::getCompilers()
{
	return compiler_defs;
}

void CompilerManager::cleanup()
{
	for (CompilerDefinition *cd : compiler_defs) {
		delete cd;
	}

	compiler_defs.clear();
}
