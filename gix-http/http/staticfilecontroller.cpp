/*
This file is part of QtWebApp
Copyright (C) 2010-2021 Stefan Frings

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

/**
  @file
  @author Stefan Frings
*/

#include "staticfilecontroller.h"
#include <QFileInfo>
#include <QDir>
#include <QDateTime>
#include <QLogger.h>
#include "global.h"

using namespace stefanfrings;

StaticFileController::StaticFileController(ServerConfig* config, QObject* parent)
	:HttpRequestHandler(parent)
{
	maxAge = config->getMaxAge();
	encoding = config->getEncoding();
	docroot = config->getBasePath();
/*
	if (!(docroot.startsWith(":/") || docroot.startsWith("qrc://"))) {
		// Convert relative path to absolute, based on the directory of the config file.
#ifdef Q_OS_WIN32
		if (QDir::isRelativePath(docroot) && settings->format() != QSettings::NativeFormat)
#else
		if (QDir::isRelativePath(docroot))
#endif
		{
			QFileInfo configFile(settings->fileName());
			docroot = QFileInfo(configFile.absolutePath(), docroot).absoluteFilePath();
		}
	}
*/
	QLogger::QLog_Debug(SERVER_LOG, QString("StaticFileController: docroot=%1, encoding=%2, maxAge=%3").arg(docroot).arg(encoding).arg(maxAge));
	maxCachedFileSize = config->getMaxCachedFileSize();
	cache.setMaxCost(config->getCacheSize());
	cacheTimeout = config->getCacheTime();
	QLogger::QLog_Debug(SERVER_LOG, QString("StaticFileController: cache timeout=%1, size=%2").arg(cacheTimeout).arg(cache.maxCost()));
}


void StaticFileController::service(HttpRequest& request, HttpResponse& response)
{
	QByteArray path = request.getPath();
	// Check if we have the file in cache
	qint64 now = QDateTime::currentMSecsSinceEpoch();
	mutex.lock();
	CacheEntry* entry = cache.object(path);
	if (entry && (cacheTimeout == 0 || entry->created > now - cacheTimeout)) {
		QByteArray document = entry->document; //copy the cached document, because other threads may destroy the cached entry immediately after mutex unlock.
		QByteArray filename = entry->filename;
		mutex.unlock();
		QLogger::QLog_Debug(SERVER_LOG, QString("StaticFileController: Cache hit for %1").arg(path.data()));
		setContentType(filename, response);
		response.setHeader("Cache-Control", "max-age=" + QByteArray::number(maxAge / 1000));
		response.write(document);
	}
	else {
		mutex.unlock();
		// The file is not in cache.
		qDebug("StaticFileController: Cache miss for %s", path.data());
		// Forbid access to files outside the docroot directory
		if (path.contains("/..")) {
			QLogger::QLog_Warning(SERVER_LOG, QString("StaticFileController: detected forbidden characters in path %1").arg(path.data()));
			response.setStatus(403, "forbidden");
			response.write("403 forbidden", true);
			return;
		}
		// If the filename is a directory, append index.html.
		if (QFileInfo(docroot + path).isDir()) {
			path += "/index.html";
		}
		// Try to open the file
		QFile file(docroot + path);
		QLogger::QLog_Debug(SERVER_LOG, QString("StaticFileController: Open file %1").arg((file.fileName())));
		if (file.open(QIODevice::ReadOnly)) {
			setContentType(path, response);
			response.setHeader("Cache-Control", "max-age=" + QByteArray::number(maxAge / 1000));
			if (file.size() <= maxCachedFileSize) {
				// Return the file content and store it also in the cache
				entry = new CacheEntry();
				while (!file.atEnd() && !file.error()) {
					QByteArray buffer = file.read(65536);
					response.write(buffer);
					entry->document.append(buffer);
				}
				entry->created = now;
				entry->filename = path;
				mutex.lock();
				cache.insert(request.getPath(), entry, entry->document.size());
				mutex.unlock();
			}
			else {
				// Return the file content, do not store in cache
				while (!file.atEnd() && !file.error()) {
					response.write(file.read(65536));
				}
			}
			file.close();
		}
		else {
			if (file.exists()) {
				QLogger::QLog_Warning(SERVER_LOG, QString("StaticFileController: Cannot open existing file %1 for reading").arg(file.fileName()));
				response.setStatus(403, "forbidden");
				response.write("403 forbidden", true);
			}
			else {
				response.setStatus(404, "not found");
				response.write("404 not found", true);
			}
		}
	}
}

void StaticFileController::setContentType(const QString fileName, HttpResponse& response) const
{
	if (fileName.endsWith(".png")) {
		response.setHeader("Content-Type", "image/png");
	}
	else if (fileName.endsWith(".jpg")) {
		response.setHeader("Content-Type", "image/jpeg");
	}
	else if (fileName.endsWith(".gif")) {
		response.setHeader("Content-Type", "image/gif");
	}
	else if (fileName.endsWith(".pdf")) {
		response.setHeader("Content-Type", "application/pdf");
	}
	else if (fileName.endsWith(".txt")) {
		response.setHeader("Content-Type", qPrintable("text/plain; charset=" + encoding));
	}
	else if (fileName.endsWith(".html") || fileName.endsWith(".htm")) {
		response.setHeader("Content-Type", qPrintable("text/html; charset=" + encoding));
	}
	else if (fileName.endsWith(".css")) {
		response.setHeader("Content-Type", "text/css");
	}
	else if (fileName.endsWith(".js")) {
		response.setHeader("Content-Type", "text/javascript");
	}
	else if (fileName.endsWith(".svg")) {
		response.setHeader("Content-Type", "image/svg+xml");
	}
	else if (fileName.endsWith(".woff")) {
		response.setHeader("Content-Type", "font/woff");
	}
	else if (fileName.endsWith(".woff2")) {
		response.setHeader("Content-Type", "font/woff2");
	}
	else if (fileName.endsWith(".ttf")) {
		response.setHeader("Content-Type", "application/x-font-ttf");
	}
	else if (fileName.endsWith(".eot")) {
		response.setHeader("Content-Type", "application/vnd.ms-fontobject");
	}
	else if (fileName.endsWith(".otf")) {
		response.setHeader("Content-Type", "application/font-otf");
	}
	else if (fileName.endsWith(".json")) {
		response.setHeader("Content-Type", "application/json");
	}
	else if (fileName.endsWith(".xml")) {
		response.setHeader("Content-Type", "text/xml");
	}
	// Todo: add all of your content types
	else {
		QLogger::QLog_Debug(SERVER_LOG, QString("StaticFileController: unknown MIME type for filename '%1'").arg(fileName));
	}
}
