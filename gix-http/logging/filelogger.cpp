/*
This file is part of QtWebApp
Copyright (C) 2010-2021 Stefan Frings

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

#include "filelogger.h"
#include <QTime>
#include <QStringList>
#include <QThread>
#include <QtGlobal>
#include <QFile>
#include <QTimerEvent>
#include <QDir>
#include <QFileInfo>
#include <stdio.h>

using namespace stefanfrings;

void FileLogger::refreshSettings()
{
    mutex.lock();
    // Save old file name for later comparision with new settings
    QString oldFileName=fileName;

    // Load new config settings
    settings->sync();
    fileName=settings->value("fileName").toString();
    // Convert relative fileName to absolute, based on the directory of the config file.
#ifdef Q_OS_WIN32
    if (QDir::isRelativePath(fileName) && settings->format()!=QSettings::NativeFormat)
#else
    if (QDir::isRelativePath(fileName))
#endif
    {
        QFileInfo configFile(settings->fileName());
        fileName=QFileInfo(configFile.absolutePath(),fileName).absoluteFilePath();
    }
    maxSize=settings->value("maxSize",0).toLongLong();
    maxBackups=settings->value("maxBackups",0).toInt();
    msgFormat=settings->value("msgFormat","{timestamp} {type} {msg}").toString();
    timestampFormat=settings->value("timestampFormat","yyyy-MM-dd hh:mm:ss.zzz").toString();
    minLevel=static_cast<QtMsgType>(settings->value("minLevel",0).toInt());
    bufferSize=settings->value("bufferSize",0).toInt();

    // Create new file if the filename has been changed
    if (oldFileName!=fileName)
    {
        fprintf(stderr,"Logging to %s\n",qPrintable(fileName));
        close();
        open();
    }
    mutex.unlock();
}


FileLogger::FileLogger(QSettings *settings, const int refreshInterval, QObject* parent)
    : Logger(parent)
{
    Q_ASSERT(settings!=nullptr);
    Q_ASSERT(refreshInterval>=0);
    this->settings=settings;
    file=nullptr;
    if (refreshInterval>0)
    {
        refreshTimer.start(refreshInterval,this);
    }
    flushTimer.start(1000,this);
    refreshSettings();
}


FileLogger::~FileLogger()
{
    close();
}


void FileLogger::write(const LogMessage* logMessage)
{
    // Try to write to the file
    if (file)
    {

        // Write the message
        file->write(qPrintable(logMessage->toString(msgFormat,timestampFormat)));

        // Flush error messages immediately, to ensure that no important message
        // gets lost when the program terinates abnormally.
        if (logMessage->getType()>=QtCriticalMsg)
        {
            file->flush();
        }

        // Check for success
        if (file->error())
        {
            qWarning("Cannot write to log file %s: %s",qPrintable(fileName),qPrintable(file->errorString()));
            close();
        }

    }

    // Fall-back to the super class method, if writing failed
    if (!file)
    {
        Logger::write(logMessage);
    }

}

void FileLogger::open()
{
    if (fileName.isEmpty())
    {
        qWarning("Name of logFile is empty");
    }
    else {
        file=new QFile(fileName);
        if (!file->open(QIODevice::WriteOnly | QIODevice::Append | QIODevice::Text))
        {
            qWarning("Cannot open log file %s: %s",qPrintable(fileName),qPrintable(file->errorString()));
            file=nullptr;
        }
    }
}


void FileLogger::close()
{
    if (file)
    {
        file->close();
        delete file;
        file=nullptr;
    }
}

void FileLogger::rotate() {
    // count current number of existing backup files
    int count=0;
    forever
    {
        QFile bakFile(QString("%1.%2").arg(fileName).arg(count+1));
        if (bakFile.exists())
        {
            ++count;
        }
        else
        {
            break;
        }
    }

    // Remove all old backup files that exceed the maximum number
    while (maxBackups>0 && count>=maxBackups)
    {
        QFile::remove(QString("%1.%2").arg(fileName).arg(count));
        --count;
    }

    // Rotate backup files
    for (int i=count; i>0; --i) {
        QFile::rename(QString("%1.%2").arg(fileName).arg(i),QString("%1.%2").arg(fileName).arg(i+1));
    }

    // Backup the current logfile
    QFile::rename(fileName,fileName+".1");
}


void FileLogger::timerEvent(QTimerEvent* event)
{
    if (!event)
    {
        return;
    }
    else if (event->timerId()==refreshTimer.timerId())
    {
        refreshSettings();
    }
    else if (event->timerId()==flushTimer.timerId() && file)
    {
        mutex.lock();

        // Flush the I/O buffer
        file->flush();

        // Rotate the file if it is too large
        if (maxSize>0 && file->size()>=maxSize)
        {
            close();
            rotate();
            open();
        }

        mutex.unlock();
    }
}
