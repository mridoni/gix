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

#include "logmessage.h"
#include <QThread>

using namespace stefanfrings;

LogMessage::LogMessage(const QtMsgType type, const QString& message, const QHash<QString, QString> *logVars, const QString &file, const QString &function, const int line)
{
    this->type=type;
    this->message=message;
    this->file=file;
    this->function=function;
    this->line=line;
    timestamp=QDateTime::currentDateTime();
    threadId=QThread::currentThreadId();

    // Copy the logVars if not null,
    // so that later changes in the original do not affect the copy
    if (logVars)
    {
        this->logVars=*logVars;
    }
}

QString LogMessage::toString(const QString& msgFormat, const QString& timestampFormat) const
{
    QString decorated=msgFormat+"\n";
    decorated.replace("{msg}",message);

    if (decorated.contains("{timestamp}"))
    {
        decorated.replace("{timestamp}",timestamp.toString(timestampFormat));
    }

    QString typeNr;
    typeNr.setNum(type);
    decorated.replace("{typeNr}",typeNr);

    switch (type)
    {
        case QtDebugMsg:
            decorated.replace("{type}","DEBUG   ");
            break;
        case QtWarningMsg:
            decorated.replace("{type}","WARNING ");
            break;
        case QtCriticalMsg:
            decorated.replace("{type}","CRITICAL");
            break;
        case QtFatalMsg:
            decorated.replace("{type}","FATAL   ");
            break;
    #if (QT_VERSION >= QT_VERSION_CHECK(5, 5, 0))
        case QtInfoMsg:
            decorated.replace("{type}","INFO    ");
            break;
    #endif
    }

    decorated.replace("{file}",file);
    decorated.replace("{function}",function);
    decorated.replace("{line}",QString::number(line));

    QString threadId;
    threadId.sprintf("%p", QThread::currentThreadId());
    //threadId.setNum((uintptr_t)QThread::currentThreadId());
    decorated.replace("{thread}",threadId);

    // Fill in variables
    if (decorated.contains("{") && !logVars.isEmpty())
    {
        QList<QString> keys=logVars.keys();
        foreach (QString key, keys)
        {
            decorated.replace("{"+key+"}",logVars.value(key));
        }
    }

    return decorated;
}

QtMsgType LogMessage::getType() const
{
    return type;
}
