/**
  @file
  @author Stefan Frings
*/

#include "httplistener.h"
#include "httpconnectionhandler.h"
#include "httpconnectionhandlerpool.h"
#include <QCoreApplication>
#include <QLogger.h>

using namespace stefanfrings;

HttpListener::HttpListener(ServerConfig* config, HttpRequestHandler* requestHandler, QObject* parent)
	: QTcpServer(parent)
{
	Q_ASSERT(config != nullptr);
	Q_ASSERT(requestHandler != nullptr);
	pool = nullptr;
	this->config = config;
	this->requestHandler = requestHandler;
	// Reqister type of socketDescriptor for signal/slot handling
	qRegisterMetaType<tSocketDescriptor>("tSocketDescriptor");
	
}


HttpListener::~HttpListener()
{
	close();
	QLogger::QLog_Debug(SERVER_LOG, "HttpListener: destroyed");
}


bool HttpListener::listen()
{
	// Start listening
	if (!pool) {
		pool = new HttpConnectionHandlerPool(config, requestHandler);
	}
	QString host = config->getAddressString();
	quint16 port = config->getPort() & 0xFFFF;
	QTcpServer::listen(host.isEmpty() ? QHostAddress::Any : QHostAddress(host), port);
	if (!isListening()) {
		QLogger::QLog_Error(SERVER_LOG, QString("HttpListener: Cannot bind on port %1: %2").arg(port).arg(errorString()).toUtf8().data());
		return false;
	}
	else {
		QLogger::QLog_Info(SERVER_LOG, QString("HttpListener: Listening on port %1").arg(port).toUtf8().data());
		return true;
	}
}


void HttpListener::close()
{
	QTcpServer::close();
	QLogger::QLog_Debug(SERVER_LOG, "HttpListener: closed");
	if (pool) {
		delete pool;
		pool = nullptr;
	}
}

void HttpListener::incomingConnection(tSocketDescriptor socketDescriptor)
{
#ifdef SUPERVERBOSE
	qDebug("HttpListener: New connection");
#endif

	HttpConnectionHandler* freeHandler = nullptr;
	if (pool) {
		freeHandler = pool->getConnectionHandler();
	}

	// Let the handler process the new connection.
	if (freeHandler) {
		// The descriptor is passed via event queue because the handler lives in another thread
		QMetaObject::invokeMethod(freeHandler, "handleConnection", Qt::QueuedConnection, Q_ARG(tSocketDescriptor, socketDescriptor));
	}
	else {
		// Reject the connection
		QLogger::QLog_Debug(SERVER_LOG, "HttpListener: Too many incoming connections");
		QTcpSocket* socket = new QTcpSocket(this);
		socket->setSocketDescriptor(socketDescriptor);
		connect(socket, SIGNAL(disconnected()), socket, SLOT(deleteLater()));
		socket->write("HTTP/1.1 503 too many connections\r\nConnection: close\r\n\r\nToo many connections\r\n");
		socket->disconnectFromHost();
	}
}
