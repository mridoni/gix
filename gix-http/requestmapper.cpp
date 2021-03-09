/*
Copyright (C) 2010 Stefan Frings
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
/**
  @file
  @author Stefan Frings
*/

#include <QCoreApplication>
#include "global.h"
#include "requestmapper.h"
#include "filelogger.h"
#include "staticfilecontroller.h"
#include "controller/dumpcontroller.h"
#include "controller/templatecontroller.h"
#include "controller/formcontroller.h"
#include "controller/fileuploadcontroller.h"
#include "controller/sessioncontroller.h"
#include "controller/schemacontroller.h"
#include "controller/cobolcontroller.h"
#include "QLogger.h"

RequestMapper::RequestMapper(ServerConfig* svr_config, QObject* parent)
    :HttpRequestHandler(parent)
{
	config = svr_config;
	QMap<QString, ServiceConfig *> svcs = config->getServices();
	QMap<QString, ServiceConfig *>::iterator it;

	for (it = svcs.begin(); it != svcs.end(); ++it) {
		ServiceConfig *svc = (*it);
		service_paths[svc->getUrl()] = svc;
	}
    QLogger::QLog_Debug(SERVER_LOG, "RequestMapper: created");
}


RequestMapper::~RequestMapper()
{
    QLogger::QLog_Debug(SERVER_LOG, "RequestMapper: deleted");
}


void RequestMapper::service(HttpRequest& request, HttpResponse& response)
{
    QByteArray path=request.getPath();
    qDebug("RequestMapper: path=%s", path.data());

	ServiceConfig* svc = findService(path.data());
	if (!svc) {
		QLogger::QLog_Error(SERVER_LOG, QString("Invalid request for path ") + path.data());
		return;
	}

	QLogger::QLog_Debug(SERVER_LOG, QString("Request for service %1 from %2").arg(svc->getName()).arg(request.getPeerAddress().toString()));
	QLogger::QLog_Debug(svc->getName(), QString("Request from %1").arg(request.getPeerAddress().toString()));

    // For the following pathes, each request gets its own new instance of the related controller.

    if (path.startsWith(svc->getSchemaUrl().toUtf8())) {
        SchemaController().service(svc, request, response);
    }

    else

    if (path.startsWith(svc->getUrl().toUtf8())) {
        CobolController().service(svc, request, response);
    }

    else

    if (path.startsWith("/dump"))
    {        
        DumpController().service(request, response);
    }

    else if (path.startsWith("/template"))
    {
        TemplateController().service(request, response);
    }

    else if (path.startsWith("/form"))
    {
        FormController().service(request, response);
    }

    else if (path.startsWith("/file"))
    {
        FileUploadController().service(request, response);
    }

    else if (path.startsWith("/session"))
    {
        SessionController().service(request, response);
    }

    // All other pathes are mapped to the static file controller.
    // In this case, a single instance is used for multiple requests.
    else
    {
        staticFileController->service(request, response);
    }

    qDebug("RequestMapper: finished request");

    // Clear the log buffer
    //if (logger)
    //{
    //   logger->clear();
    //}
}

ServiceConfig* RequestMapper::findService(const char* p)
{
	QMap<QString, ServiceConfig*> svcs = config->getServices();
	QMap<QString, ServiceConfig*>::iterator it;

	QString rp(p);

	for (it = svcs.begin(); it != svcs.end(); ++it) {
		ServiceConfig* svc = (*it);
		if (!svc->isEnabled())
			continue;

		if (rp.startsWith(svc->getUrl())) {
			return svc;
		}
	}
	return nullptr;
}
