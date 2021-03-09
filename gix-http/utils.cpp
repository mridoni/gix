/*
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/


#include "utils.h"

#include <QFileInfo>
#include <QDir>

QLogger::LogLevel decode_log_level(QString l)
{
	if (l.isEmpty())
		return QLogger::LogLevel::Warning;

	if (l.toLower() == "debug")
		return QLogger::LogLevel::Debug;
	else
		if (l.toLower() == "error")
			return QLogger::LogLevel::Error;
		else
			if (l.toLower() == "fatal")
				return QLogger::LogLevel::Fatal;
			else
				if (l.toLower() == "info")
					return QLogger::LogLevel::Info;
				else
					if (l.toLower() == "trace")
						return QLogger::LogLevel::Trace;
					else
						if (l.toLower() == "warning")
							return QLogger::LogLevel::Warning;
						else
							return QLogger::LogLevel::Warning;
}

bool is_valid_log_file(QString f)
{
	QFileInfo fi(f);
	if (fi.exists())
		return fi.isWritable();

	QString d = fi.absoluteDir().path();
	QFileInfo di(d);
	return di.isWritable();
}
