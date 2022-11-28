/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#include "Ide.h"
#include "SysUtils.h"

#include "IdeTaskManager.h"
#include "IdeSearchManager.h"
#include "CompilerManager.h"
#include "IdeDbManager.h"
#include "TargetManager.h"
#include "GixGlobals.h"
#include "UiUtils.h"
#include "ide_sink.h"

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
	delete ide_task_manager;
	delete ide_search_manager;
	delete db_manager;
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
		GixGlobals::getLogManager()->error(LOG_IDE, "{}", msg);
		UiUtils::ErrorDialog(msg);
	}
    
	auto i_sink = std::make_shared<ide_sink_mt>();
	std::vector<spdlog::sink_ptr> sinks = { i_sink };
	auto logger = std::make_shared<spdlog::logger>("gix-ide", begin(sinks), end(sinks));
	spdlog::set_default_logger(logger);
	spdlog::set_level(spdlog::level::trace);	// max log level, will be limited by the sink-specific levels
	i_sink->set_level(spdlog::level::trace);
	logger->flush_on(spdlog::level::trace);    
	
#ifdef WIN32
	// We copy default settings from Global to User, if missing
	QStringList misc_settings = { "ReleaseCompilerId", "DebugCompilerId", "editor_font_name", "editor_font_size", "grid_font_name", "grid_font_size", "treeview_font_name", "treeview_font_size", "default_eol_mode" };
	for (QString k : misc_settings) {
		QString v = SysUtils::RegistryGetValue("HKEY_CURRENT_USER\\Software\\MediumGray\\gix-ide", k);
		if (v.isEmpty()) {
			v = SysUtils::RegistryGetValue("HKEY_LOCAL_MACHINE\\Software\\MediumGray\\gix-ide", k);
			if (!v.isEmpty())
				SysUtils::RegistrySetValue("HKEY_CURRENT_USER\\Software\\MediumGray\\gix-ide", k, v);
		}
	}

#endif
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
