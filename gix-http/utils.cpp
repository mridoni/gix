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
