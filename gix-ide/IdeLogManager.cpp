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

#include "IdeLogManager.h"
#include "Ide.h"
#include "IdeTaskManager.h"
#include <spdlog/sinks/msvc_sink.h>

QMap<int, std::shared_ptr<spdlog::logger>> IdeLogManager::loggers;
QMap<int, QList<QPair<spdlog::level::level_enum, QString>>> IdeLogManager::backlog;

IdeLogManager::IdeLogManager()
{
	// we start with no default logger, so the backlog entries will be stashed and used
	// when the IDE and the loggers are available
}

void IdeLogManager::registerLogSource(int source, std::shared_ptr<spdlog::logger> l)
{
	loggers[source] = l;
	handleBackLog(source);
}


std::shared_ptr<spdlog::logger> IdeLogManager::get_logger(int source)
{
	if (loggers.contains(source))
		return loggers[source];
	else
		return default_logger;
}

void IdeLogManager::add_to_backlog(int source, spdlog::level::level_enum level, std::string msg)
{
	if (!backlog.contains(source)) {
		backlog[source] = QList<QPair<spdlog::level::level_enum, QString>>();
	}
	backlog[source].append(QPair<spdlog::level::level_enum, QString>(level, QString::fromStdString(msg)));
}

void IdeLogManager::handleBackLog(int source)
{
	if (!backlog.contains(source) || !loggers.contains(source))
		return;

	std::shared_ptr<spdlog::logger> l = loggers[source];

	QList<QPair<spdlog::level::level_enum, QString>> backlog_entries = backlog.value(source);

	for (auto e : backlog_entries) {
		spdlog::level::level_enum level = e.first;
		QString msg = e.second;

		l->log(level, msg);
	}

	backlog.remove(source);
}
