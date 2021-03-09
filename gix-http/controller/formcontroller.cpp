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

#include "formcontroller.h"

FormController::FormController()
{}

void FormController::service(HttpRequest& request, HttpResponse& response)
{

    response.setHeader("Content-Type", "text/html; charset=ISO-8859-1");

    if (request.getParameter("action")=="show")
    {
        response.write("<html><body>");
        response.write("Name = ");
        response.write(request.getParameter("name"));
        response.write("<br>City = ");
        response.write(request.getParameter("city"));
        response.write("</body></html>",true);
    }
    else
    {
        response.write("<html><body>");
        response.write("<form method=\"post\">");
        response.write("  <input type=\"hidden\" name=\"action\" value=\"show\">");
        response.write("  Name: <input type=\"text\" name=\"name\"><br>");
        response.write("  City: <input type=\"text\" name=\"city\"><br>");
        response.write("  <input type=\"submit\">");
        response.write("</form>");
        response.write("</body></html>",true);
    }
}

