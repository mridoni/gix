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

#pragma once

#include <QStringList>

#include "gixcommon_global.h"

class BuildResult {

	friend class BuildDriver;
	friend class ProjectBuilder;
	friend class SingleArtifactBuilder;

public:
	GIXCOMMON_EXPORT BuildResult();
	GIXCOMMON_EXPORT BuildResult(int st, const QString& msg);
	GIXCOMMON_EXPORT BuildResult(int st, const QStringList& bl);

	GIXCOMMON_EXPORT bool isSuccess();
	GIXCOMMON_EXPORT int getStatus();
	GIXCOMMON_EXPORT QStringList buildlog();

private:
	int status = 0;
	QStringList build_log;
};
