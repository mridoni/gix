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

#include "fileuploadcontroller.h"

FileUploadController::FileUploadController()
{}

void FileUploadController::service(HttpRequest& request, HttpResponse& response)
{

    if (request.getParameter("action")=="show")
    {
        response.setHeader("Content-Type", "image/jpeg");
        QTemporaryFile* file=request.getUploadedFile("file1");
        if (file)
        {
            while (!file->atEnd() && !file->error())
            {
                QByteArray buffer=file->read(65536);
                response.write(buffer);
            }
        }
        else
        {
            response.write("upload failed");
        }
    }

    else
    {
        response.setHeader("Content-Type", "text/html; charset=ISO-8859-1");
        response.write("<html><body>");
        response.write("Upload a JPEG image file<p>");
        response.write("<form method=\"post\" enctype=\"multipart/form-data\">");
        response.write("  <input type=\"hidden\" name=\"action\" value=\"show\">");
        response.write("  File: <input type=\"file\" name=\"file1\"><br>");
        response.write("  <input type=\"submit\">");
        response.write("</form>");
        response.write("</body></html>",true);
    }
}

