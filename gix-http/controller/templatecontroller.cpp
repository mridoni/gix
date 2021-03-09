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

/**
  @file
  @author Stefan Frings
*/

#include "../global.h"
#include "templatecontroller.h"
#include "templatecache.h"
#include "template.h"

TemplateController::TemplateController()
{}

void TemplateController::service(HttpRequest& request, HttpResponse& response)
{
    response.setHeader("Content-Type", "text/html; charset=ISO-8859-1");

    Template t=templateCache->getTemplate("demo",request.getHeader("Accept-Language"));
    t.enableWarnings();
    t.setVariable("path",request.getPath());

    QMap<QByteArray,QByteArray> headers=request.getHeaderMap();
    QMapIterator<QByteArray,QByteArray> iterator(headers);
    t.loop("header",headers.size());
    int i=0;
    while (iterator.hasNext())
    {
        iterator.next();
        t.setVariable(QString("header%1.name").arg(i),QString(iterator.key()));
        t.setVariable(QString("header%1.value").arg(i),QString(iterator.value()));
        ++i;
    }

    response.write(t.toLatin1(),true);
}
