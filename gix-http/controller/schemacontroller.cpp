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

#include "schemacontroller.h"
#include "ServerConfig.h"
#include "JsonManager.h"
#include "ExternalInterfaceData.h"

#include <QVariant>
#include <QDateTime>
#include <QThread>

SchemaController::SchemaController()
{}

void SchemaController::service(ServiceConfig* svc, HttpRequest& request, HttpResponse& response)
{

    response.setHeader("Content-Type", "text/html; charset=ISO-8859-1");
    response.setCookie(HttpCookie("firstCookie", "hello", 600, QByteArray(), QByteArray(), QByteArray(), false, true));
    response.setCookie(HttpCookie("secondCookie", "world", 600));

    //ExternalInterfaceData * dcp = svc->getInterfaceData();
	JsonManager* jmgr = new JsonManager(svc);
	
    response.write(jmgr->getSchema(SchemaType::In).toUtf8(), true);
}
