#include "QLogger.h"
#include <QDateTime>
#include <QDir>
#include <QTextStream>

/****************************************************************************************
 ** QLogger is a library to register and print logs into a file.
 ** Copyright (C) 2018  Francesc Martinez <es.linkedin.com/in/cescmm/en>
 **
 ** This library is free software; you can redistribute it and/or
 ** modify it under the terms of the GNU Lesser General Public
 ** License as published by the Free Software Foundation; either
 ** version 2.1 of the License, or (at your option) any later version.
 **
 ** This library is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied warranty of
 ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 ** Lesser General Public License for more details.
 **
 ** You should have received a copy of the GNU Lesser General Public
 ** License along with this library; if not, write to the Free Software
 ** Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 ***************************************************************************************/

namespace QLogger
{
	void QLog_Trace(const QString& module, const QString& message)
	{
		QLog_(module, LogLevel::Trace, message);
	}

	void QLog_Debug(const QString& module, const QString& message)
	{
		QLog_(module, LogLevel::Debug, message);
	}

	void QLog_Info(const QString& module, const QString& message)
	{
		QLog_(module, LogLevel::Info, message);
	}

	void QLog_Warning(const QString& module, const QString& message)
	{
		QLog_(module, LogLevel::Warning, message);
	}

	void QLog_Error(const QString& module, const QString& message)
	{
		QLog_(module, LogLevel::Error, message);
	}

	void QLog_Fatal(const QString& module, const QString& message)
	{
		QLog_(module, LogLevel::Fatal, message);
	}

	void QLog_(const QString& module, LogLevel level, const QString& message)
	{
		const auto manager = QLoggerManager::getInstance();

		QMutexLocker(&manager->mutex);

		const auto logWriter = manager->getLogWriter(module);

		if (logWriter && !logWriter->isStop() && logWriter->getLevel() <= level) {
			manager->writeAndDequeueMessages(module);
			logWriter->write(module, message, level);
		}
		else if (!logWriter)
			manager->queueMessage(
				module,
				{ message, static_cast<int>(level), QDateTime::currentDateTime().toString("dd-MM-yyyy hh:mm:ss.zzz") });
	}

	static const int QUEUE_LIMIT = 0;

	// QLoggerManager
	QLoggerManager* QLoggerManager::INSTANCE = nullptr;
	bool QLoggerManager::mIsStop = false;
	bool QLoggerManager::mConsoleEchoEnabled = false;

	QLoggerManager::QLoggerManager()
		: QThread()
		, mutex(QMutex::Recursive)
	{
		start();
	}

	QLoggerManager* QLoggerManager::getInstance()
	{
		if (!INSTANCE)
			INSTANCE = new QLoggerManager();

		return INSTANCE;
	}

	QString QLoggerManager::levelToText(const LogLevel& level)
	{
		switch (level) {
			case LogLevel::Trace:
				return QString("Trace");
			case LogLevel::Debug:
				return QString("Debug");
			case LogLevel::Info:
				return QString("Info");
			case LogLevel::Warning:
				return QString("Warning");
			case LogLevel::Error:
				return QString("Error");
			case LogLevel::Fatal:
				return QString("Fatal");
		}

		return QString();
	}

	bool QLoggerManager::isConsoleEchoEnabled()
	{
		return mConsoleEchoEnabled;
	}

	void QLoggerManager::setConsoleEchoEnabled(bool enabled)
	{
		mConsoleEchoEnabled = enabled;
	}

	bool QLoggerManager::addDestination(const QString& fileDest, const QString& module, LogLevel level)
	{
		if (!moduleDest.contains(module)) {
			const auto log = new QLoggerWriter(fileDest, level);
			log->stop(mIsStop);

			moduleDest.insert(module, log);

			return true;
		}

		return false;
	}

	bool QLoggerManager::addDestination(const QString& fileDest, const QStringList& modules, LogLevel level)
	{
		bool allAdded = false;

		for (const auto& module : modules) {
			if (!moduleDest.contains(module)) {
				const auto log = new QLoggerWriter(fileDest, level);
				log->stop(mIsStop);

				moduleDest.insert(module, log);
				allAdded = true;
			}
		}

		return allAdded;
	}

	void QLoggerManager::queueMessage(const QString module, const QVector<QVariant>& logData)
	{
		if (mNonWriterQueue.count(module) < QUEUE_LIMIT)
			mNonWriterQueue.insert(module, logData);
	}

	void QLoggerManager::writeAndDequeueMessages(const QString& module)
	{
		auto element = mNonWriterQueue.find(module);
		const auto queueEnd = mNonWriterQueue.end();
		const auto logWriter = getLogWriter(module);

		if (element != queueEnd && logWriter && !logWriter->isStop()) {
			const auto module = element.key();

			for (; element != queueEnd; ++element) {
				const auto message = element.value().at(0).toString();
				const auto level = static_cast<LogLevel>(element.value().at(1).toInt());
				const auto dt = element.value().at(2).toString();

				if (logWriter->getLevel() <= level)
					logWriter->write(module, message, level, dt);
			}

			mNonWriterQueue.remove(module);
		}
	}

	void QLoggerManager::closeLogger()
	{
		deleteLater();
		exit(0);
	}

	void QLoggerManager::pause()
	{
		mIsStop = true;

		for (auto logWriter : moduleDest)
			logWriter->stop(mIsStop);
	}

	void QLoggerManager::resume()
	{
		mIsStop = false;

		for (auto logWriter : moduleDest)
			logWriter->stop(mIsStop);
	}

	void QLoggerManager::overwriteLogLevel(LogLevel level)
	{
		for (auto logWriter : moduleDest)
			logWriter->setLogLevel(level);
	}

	QLoggerWriter::QLoggerWriter(const QString& fileDestination, LogLevel level)
	{
		mFileDestination = fileDestination;
		mLevel = level;
	}

	QString QLoggerWriter::renameFileIfFull()
	{
		const auto MAX_SIZE = 1024 * 1024;
		const auto toRemove = mFileDestination.section('.', -1);
		const auto fileNameAux = mFileDestination.left(mFileDestination.size() - toRemove.size() - 1);
		auto renamed = false;
		auto newName = QString("%1%2").arg(fileNameAux, "_%1__%2.log");

		QFile file(mFileDestination);

		// Rename file if it's full
		if (file.size() >= MAX_SIZE) {
			const auto currentTime = QDateTime::currentDateTime();
			newName = newName.arg(currentTime.date().toString("dd_MM_yy"), currentTime.time().toString("hh_mm_ss"));
			renamed = file.rename(mFileDestination, newName);
		}

		return renamed ? newName : QString();
	}

	void QLoggerWriter::write(const QString& module, const QString& message, const LogLevel& messageLogLevel)
	{
        auto dt = QDateTime::currentDateTime();
        QString dtFormat;

        if (dt.isValid())
            dtFormat = QDateTime::currentDateTime().toString("dd-MM-yyyy hh:mm:ss.zzz");
        else
            dtFormat = "00-00-0000 00:00:00.000";

		write(module, message, messageLogLevel, dtFormat);
	}

	void QLoggerWriter::write(const QString& module, const QString& message, const LogLevel& messageLogLevel,
		const QString& dt)
	{
		const auto logLevel = QLoggerManager::levelToText(messageLogLevel);

		if (QLogger::QLoggerManager::isConsoleEchoEnabled() || mFileDestination == "console://") {
			const auto text = QString("[%1] [%2] {%3} %4").arg(dt).arg(logLevel).arg(module).arg(message);
			switch (messageLogLevel) {
				case QLogger::LogLevel::Info:
				case QLogger::LogLevel::Success:
				case QLogger::LogLevel::Debug:
				case QLogger::LogLevel::Trace:
					fprintf(stdout, "%s\n", text.toUtf8().constData());
					break;

				case QLogger::LogLevel::Error:
				case QLogger::LogLevel::Warning:
				case QLogger::LogLevel::Fatal:
					fprintf(stderr, "%s\n", text.toUtf8().constData());
					break;
			}
			if (mFileDestination == "console://")
				return;
		}

		if (mFileDestination == "virtual://") {
			emit QLoggerManager::getInstance()->logMessage(module, message, messageLogLevel);
		}
		else {

			QFile file(mFileDestination);

			const auto newName = renameFileIfFull();

			if (file.open(QIODevice::ReadWrite | QIODevice::Text | QIODevice::Append)) {
				QTextStream out(&file);

				const auto text = QString("[%1] [%2] {%3} %4\n").arg(dt).arg(logLevel).arg(module).arg(message);

				if (!newName.isEmpty())
					out << QString("%1 - Previous log %2\n").arg(dt).arg(newName);

				out << text;

				file.close();
			}
		}
	}

}
