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
#include "templatecache.h"
#include <QDateTime>
#include <QStringList>
#include <QSet>

using namespace stefanfrings;

TemplateCache::TemplateCache(const QSettings* settings, QObject* parent)
    :TemplateLoader(settings,parent)
{
    cache.setMaxCost(settings->value("cacheSize","1000000").toInt());
    cacheTimeout=settings->value("cacheTime","60000").toInt();
    qDebug("TemplateCache: timeout=%i, size=%i",cacheTimeout,cache.maxCost());
}

QString TemplateCache::tryFile(const QString localizedName)
{
    qint64 now=QDateTime::currentMSecsSinceEpoch();
    mutex.lock();
    // search in cache
    qDebug("TemplateCache: trying cached %s",qPrintable(localizedName));
    CacheEntry* entry=cache.object(localizedName);
    if (entry && (cacheTimeout==0 || entry->created>now-cacheTimeout))
    {
        mutex.unlock();
        return entry->document;
    }
    // search on filesystem
    entry=new CacheEntry();
    entry->created=now;
    entry->document=TemplateLoader::tryFile(localizedName);
    // Store in cache even when the file did not exist, to remember that there is no such file
    cache.insert(localizedName,entry,entry->document.size());
    mutex.unlock();
    return entry->document;
}

