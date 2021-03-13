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

#include "dualfilelogger.h"

using namespace stefanfrings;

DualFileLogger::DualFileLogger(QSettings *firstSettings, QSettings* secondSettings, const int refreshInterval, QObject* parent)
    :Logger(parent)
{
     firstLogger=new FileLogger(firstSettings, refreshInterval, this);
     secondLogger=new FileLogger(secondSettings, refreshInterval, this);
}

void DualFileLogger::log(const QtMsgType type, const QString& message, const QString &file, const QString &function, const int line)
{
    firstLogger->log(type,message,file,function,line);
    secondLogger->log(type,message,file,function,line);
}

void DualFileLogger::clear(const bool buffer, const bool variables)
{
    firstLogger->clear(buffer,variables);
    secondLogger->clear(buffer,variables);
}
