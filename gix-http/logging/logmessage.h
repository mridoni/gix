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

#ifndef LOGMESSAGE_H
#define LOGMESSAGE_H

#include <QtGlobal>
#include <QDateTime>
#include <QHash>
#include "logglobal.h"

namespace stefanfrings {

/**
  Represents a single log message together with some data
  that are used to decorate the log message.

  The following variables may be used in the message and in msgFormat:

  - {timestamp} Date and time of creation
  - {typeNr}    Type of the message in numeric format (0-3)
  - {type}      Type of the message in string format (DEBUG, WARNING, CRITICAL, FATAL)
  - {thread}    ID number of the thread
  - {msg}       Message text
  - {xxx}       For any user-defined logger variable

  Plus some new variables since QT 5.0, only filled when compiled in debug mode:

  - {file}      Filename where the message was generated
  - {function}  Function where the message was generated
  - {line}      Line number where the message was generated
*/

class DECLSPEC LogMessage
{
    Q_DISABLE_COPY(LogMessage)
public:

    /**
      Constructor. All parameters are copied, so that later changes to them do not
      affect this object.
      @param type Type of the message
      @param message Message text
      @param logVars Logger variables, 0 is allowed
      @param file Name of the source file where the message was generated
      @param function Name of the function where the message was generated
      @param line Line Number of the source file, where the message was generated
    */
    LogMessage(const QtMsgType type, const QString& message, const QHash<QString,QString>* logVars,
               const QString &file, const QString &function, const int line);

    /**
      Returns the log message as decorated string.
      @param msgFormat Format of the decoration. May contain variables and static text,
          e.g. "{timestamp} {type} thread={thread}: {msg}".
      @param timestampFormat Format of timestamp, e.g. "dd.MM.yyyy hh:mm:ss.zzz", see QDateTime::toString().
      @see QDatetime for a description of the timestamp format pattern
    */
    QString toString(const QString& msgFormat, const QString& timestampFormat) const;

    /**
      Get the message type.
    */
    QtMsgType getType() const;

private:

    /** Logger variables */
    QHash<QString,QString> logVars;

    /** Date and time of creation */
    QDateTime timestamp;

    /** Type of the message */
    QtMsgType type;

    /** ID number of the thread  */
    Qt::HANDLE threadId;

    /** Message text */
    QString message;

    /** Filename where the message was generated */
    QString file;

    /** Function name where the message was generated */
    QString function;

    /** Line number where the message was generated */
    int line;

};

} // end of namespace

#endif // LOGMESSAGE_H
