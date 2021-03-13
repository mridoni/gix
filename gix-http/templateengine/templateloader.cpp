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

#include "templateloader.h"
#include <QFile>
#include <QFileInfo>
#include <QStringList>
#include <QDir>
#include <QSet>

using namespace stefanfrings;

TemplateLoader::TemplateLoader(const QSettings *settings, QObject *parent)
    : QObject(parent)
{
    templatePath=settings->value("path",".").toString();
    // Convert relative path to absolute, based on the directory of the config file.
#ifdef Q_OS_WIN32
    if (QDir::isRelativePath(templatePath) && settings->format()!=QSettings::NativeFormat)
#else
    if (QDir::isRelativePath(templatePath))
#endif
    {
        QFileInfo configFile(settings->fileName());
        templatePath=QFileInfo(configFile.absolutePath(),templatePath).absoluteFilePath();
    }
    fileNameSuffix=settings->value("suffix",".tpl").toString();
    QString encoding=settings->value("encoding").toString();
    if (encoding.isEmpty())
    {
        textCodec=QTextCodec::codecForLocale();
    }
    else
    {
       textCodec=QTextCodec::codecForName(encoding.toLocal8Bit());
   }
   qDebug("TemplateLoader: path=%s, codec=%s",qPrintable(templatePath),textCodec->name().data());
}

TemplateLoader::~TemplateLoader()
{}

QString TemplateLoader::tryFile(QString localizedName)
{
    QString fileName=templatePath+"/"+localizedName+fileNameSuffix;
    qDebug("TemplateCache: trying file %s",qPrintable(fileName));
    QFile file(fileName);
    if (file.exists()) {
        file.open(QIODevice::ReadOnly);
        QString document=textCodec->toUnicode(file.readAll());
        file.close();
        if (file.error())
        {
            qCritical("TemplateLoader: cannot load file %s, %s",qPrintable(fileName),qPrintable(file.errorString()));
            return "";
        }
        else
        {
            return document;
        }
    }
    return "";
}

Template TemplateLoader::getTemplate(QString templateName, QString locales)
{
    QSet<QString> tried; // used to suppress duplicate attempts
    QStringList locs=locales.split(',',QString::SkipEmptyParts);

    // Search for exact match
    foreach (QString loc,locs)
    {
        loc.replace(QRegExp(";.*"),"");
        loc.replace('-','_');
        QString localizedName=templateName+"-"+loc.trimmed();
        if (!tried.contains(localizedName))
        {
            QString document=tryFile(localizedName);
            if (!document.isEmpty()) {
                return Template(document,localizedName);
            }
            tried.insert(localizedName);
        }
    }

    // Search for correct language but any country
    foreach (QString loc,locs)
    {
        loc.replace(QRegExp("[;_-].*"),"");
        QString localizedName=templateName+"-"+loc.trimmed();
        if (!tried.contains(localizedName))
        {
            QString document=tryFile(localizedName);
            if (!document.isEmpty())
            {
                return Template(document,localizedName);
            }
            tried.insert(localizedName);
        }
    }

    // Search for default file
    QString document=tryFile(templateName);
    if (!document.isEmpty())
    {
        return Template(document,templateName);
    }

    qCritical("TemplateCache: cannot find template %s",qPrintable(templateName));
    return Template("",templateName);
}
