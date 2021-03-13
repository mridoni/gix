/*
This file is part of QtWebApp
Copyright (C) 2010-2019 Stefan Frings

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

#include <QDateTime>
#include "../global.h"
#include "sessioncontroller.h"
#include "httpsessionstore.h"

SessionController::SessionController()
{}

void SessionController::service(HttpRequest& request, HttpResponse& response)
{

    response.setHeader("Content-Type", "text/html; charset=ISO-8859-1");

    // Get current session, or create a new one
    HttpSession session=sessionStore->getSession(request,response);
    if (!session.contains("startTime"))
    {
        response.write("<html><body>New session started. Reload this page now.</body></html>");
        session.set("startTime",QDateTime::currentDateTime());
    }
    else
    {
        QDateTime startTime=session.get("startTime").toDateTime();
        response.write("<html><body>Your session started ");
        response.write(startTime.toString().toLatin1());
        response.write("</body></html>");
    }

}
