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

#ifndef DUALFILELOGGER_H
#define DUALFILELOGGER_H

#include <QString>
#include <QSettings>
#include <QtGlobal>
#include "logglobal.h"
#include "logger.h"
#include "filelogger.h"

namespace stefanfrings {

/**
  Writes log messages into two log files simultaneously.
  I recommend to configure:
  - One "main" logfile with minLevel=1 or 2 and bufferSize=0. This file is for the operator to see when a problem occured.
  - A second "debug" logfile with minLevel=1 or 2 and bufferSize=100. This file is for the developer who may need more details (the debug messages) about the
  situation that leaded to the error.

  @see FileLogger for a description of the two underlying loggers.
*/

class DECLSPEC DualFileLogger : public Logger {
    Q_OBJECT
    Q_DISABLE_COPY(DualFileLogger)
public:

    /**
      Constructor.
      @param firstSettings Configuration settings for the first logfile, usually stored in an INI file.
      Must not be 0.
      Settings are read from the current group, so the caller must have called settings->beginGroup().
      Because the group must not change during runtime, it is recommended to provide a
      separate QSettings instance that is not used by other parts of the program.
      The FileLogger does not take over ownership of the QSettings instance, so the caller
      should destroy it during shutdown.
      @param secondSettings Same as firstSettings, but for the second log file.
      @param refreshInterval Interval of checking for changed config settings in msec, or 0=disabled
      @param parent Parent object.
    */
    DualFileLogger(QSettings* firstSettings, QSettings* secondSettings,
                   const int refreshInterval=10000, QObject *parent = nullptr);

    /**
      Decorate and log the message, if type>=minLevel.
      This method is thread safe.
      @param type Message type (level)
      @param message Message text
      @param file Name of the source file where the message was generated (usually filled with the macro __FILE__)
      @param function Name of the function where the message was generated (usually filled with the macro __LINE__)
      @param line Line Number of the source file, where the message was generated (usually filles with the macro __func__ or __FUNCTION__)
      @see LogMessage for a description of the message decoration.
    */
    virtual void log(const QtMsgType type, const QString& message, const QString &file="",
                     const QString &function="", const int line=0);

    /**
      Clear the thread-local data of the current thread.
      This method is thread safe.
      @param buffer Whether to clear the backtrace buffer
      @param variables Whether to clear the log variables
    */
    virtual void clear(const bool buffer=true, const bool variables=true);

private:

    /** First logger */
    FileLogger* firstLogger;

    /** Second logger */
    FileLogger* secondLogger;

};

} // end of namespace

#endif // DUALFILELOGGER_H
