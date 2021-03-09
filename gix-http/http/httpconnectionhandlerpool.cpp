/*
This file is part of QtWebApp
Copyright (C) 2010 Stefan Frings

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

#ifndef QT_NO_OPENSSL
#include <QSslSocket>
#include <QSslKey>
#include <QSslCertificate>
#include <QSslConfiguration>
#endif
#include <QDir>
#include "httpconnectionhandlerpool.h"

using namespace stefanfrings;

HttpConnectionHandlerPool::HttpConnectionHandlerPool(ServerConfig* config, HttpRequestHandler* requestHandler)
	: QObject()
{
	Q_ASSERT(config != 0);
	this->config = config;
	this->requestHandler = requestHandler;
	this->sslConfiguration = NULL;
	loadSslConfig();
	cleanupTimer.start(config->getCleanupInterval());
	connect(&cleanupTimer, SIGNAL(timeout()), SLOT(cleanup()));
}


HttpConnectionHandlerPool::~HttpConnectionHandlerPool()
{
	// delete all connection handlers and wait until their threads are closed
	foreach(HttpConnectionHandler * handler, pool)
	{
		delete handler;
	}
	delete sslConfiguration;
	qDebug("HttpConnectionHandlerPool (%p): destroyed", this);
}


HttpConnectionHandler* HttpConnectionHandlerPool::getConnectionHandler()
{
	HttpConnectionHandler* freeHandler = 0;
	mutex.lock();
	// find a free handler in pool
	foreach(HttpConnectionHandler * handler, pool)
	{
		if (!handler->isBusy()) {
			freeHandler = handler;
			freeHandler->setBusy();
			break;
		}
	}
	// create a new handler, if necessary
	if (!freeHandler) {
		int maxConnectionHandlers = config->getMaxThreads();
		if (pool.count() < maxConnectionHandlers) {
			freeHandler = new HttpConnectionHandler(config, requestHandler, sslConfiguration);
			freeHandler->setBusy();
			pool.append(freeHandler);
		}
	}
	mutex.unlock();
	return freeHandler;
}


void HttpConnectionHandlerPool::cleanup()
{
	int maxIdleHandlers = config->getMinThreads();
	int idleCounter = 0;
	mutex.lock();
	foreach(HttpConnectionHandler * handler, pool)
	{
		if (!handler->isBusy()) {
			if (++idleCounter > maxIdleHandlers) {
				delete handler;
				pool.removeOne(handler);
				qDebug("HttpConnectionHandlerPool: Removed connection handler (%p), pool size is now %i", handler, pool.size());
				break; // remove only one handler in each interval
			}
		}
	}
	mutex.unlock();
}


void HttpConnectionHandlerPool::loadSslConfig()
{
	// If certificate and key files are configured, then load them
	QString sslKeyFileName = config->getSslKeyFile();
	QString sslCertFileName = config->getSslCertFile();

	if (config->getSslEnabled() && !sslKeyFileName.isEmpty() && !sslCertFileName.isEmpty()) {
#ifdef QT_NO_OPENSSL
		qWarning("HttpConnectionHandlerPool: SSL is not supported");
#else
/*
		// Convert relative fileNames to absolute, based on the directory of the config file.
		QFileInfo configFile(settings->fileName());
#ifdef Q_OS_WIN32
		if (QDir::isRelativePath(sslKeyFileName) && settings->format() != QSettings::NativeFormat)
#else
		if (QDir::isRelativePath(sslKeyFileName))
#endif
		{
			sslKeyFileName = QFileInfo(configFile.absolutePath(), sslKeyFileName).absoluteFilePath();
		}
#ifdef Q_OS_WIN32
		if (QDir::isRelativePath(sslCertFileName) && settings->format() != QSettings::NativeFormat)
#else
		if (QDir::isRelativePath(sslCertFileName))
#endif
		{
			sslCertFileName = QFileInfo(configFile.absolutePath(), sslCertFileName).absoluteFilePath();
		}
*/
		// Load the SSL certificate
		QFile certFile(sslCertFileName);
		if (!certFile.open(QIODevice::ReadOnly)) {
			qCritical("HttpConnectionHandlerPool: cannot open sslCertFile %s", qPrintable(sslCertFileName));
			return;
		}
		QSslCertificate certificate(&certFile, QSsl::Pem);
		certFile.close();

		// Load the key file
		QFile keyFile(sslKeyFileName);
		if (!keyFile.open(QIODevice::ReadOnly)) {
			qCritical("HttpConnectionHandlerPool: cannot open sslKeyFile %s", qPrintable(sslKeyFileName));
			return;
		}
		QSslKey sslKey(&keyFile, QSsl::Rsa, QSsl::Pem);
		keyFile.close();

		// Create the SSL configuration
		sslConfiguration = new QSslConfiguration();
		sslConfiguration->setLocalCertificate(certificate);
		sslConfiguration->setPrivateKey(sslKey);
		sslConfiguration->setPeerVerifyMode(QSslSocket::VerifyNone);
		sslConfiguration->setProtocol(QSsl::TlsV1SslV3);

		qDebug("HttpConnectionHandlerPool: SSL settings loaded");
#endif
	}
}
