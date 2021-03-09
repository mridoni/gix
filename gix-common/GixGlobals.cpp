/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#include "GixGlobals.h"
#include "PathUtils.h"

#include <QSettings>

GixGlobals GixGlobals::instance;
#if defined(Q_OS_WIN)
QString GixGlobals::_gix_data_dir = QString();
#endif

bool GixGlobals::initManagers()
{
    QString dataDir = getGixDataDir();
    if (!dataDir.isEmpty()) {
        QDir(".").mkpath(dataDir);
        QDir(".").mkpath(PathUtils::combine(dataDir, "compiler-defs"));
    }

    instance.target_manager = new TargetManager();
    instance.compiler_manager = new CompilerManager();
    instance.metadata_manager = new MetadataManager();

    return true;
}

bool GixGlobals::registerLogManager(IGixLogManager *logger)
{
    if (!logger)
        return false;

    instance.logger = logger;
    return true;
}

bool GixGlobals::registerCallbacks(GixGlobalsCallbacks *callbacks)
{
    if (!callbacks || !callbacks->getCurrentConfiguration || !callbacks->getCurrentPlatform || !callbacks->getCurrentProjectCollection)
        return false;

    instance.cb_getCurrentConfiguration = callbacks->getCurrentConfiguration;
    instance.cb_getCurrentPlatform = callbacks->getCurrentPlatform;
    instance.cb_getCurrentProjectCollection = callbacks->getCurrentProjectCollection;

    return true;
}


QString GixGlobals::getCurrentConfiguration()
{
    return instance.cb_getCurrentConfiguration();
}

QString GixGlobals::getCurrentPlatform()
{
    return instance.cb_getCurrentPlatform();
}

ProjectCollection *GixGlobals::getCurrentProjectCollection()
{
    return instance.cb_getCurrentProjectCollection();
}

TargetManager *GixGlobals::getTargetManager()
{
    return instance.target_manager;
}

IGixLogManager *GixGlobals::getLogManager()
{
    return instance.logger;
}

QString GixGlobals::getGixHomeDir()
{
    auto qba = qgetenv("GIX_HOME");
    if (!qba.isEmpty())
        return QString::fromUtf8(qba);

    QString bindir = getGixBinDir();
    if (!bindir.isEmpty()) {
        QDir theDir(bindir);
        theDir.cdUp();
        return theDir.absolutePath();
    }

    return QString();
}

QString GixGlobals::getGixBinDir()
{
    QDir theDir(QCoreApplication::applicationDirPath());
    return theDir.absolutePath();
}

QString GixGlobals::getGixLibDir(QString target_platform)
{
    auto qba = qgetenv("GIX_LIB_DIR");
    if (!qba.isEmpty())
        return QString::fromUtf8(qba);

    QString libdir = getGixHomeDir();
    if (!libdir.isEmpty()) {
        QDir theDir(libdir);
        if (theDir.cd(target_platform))
            return theDir.absolutePath();
    }
    return QString();
}


QString GixGlobals::getGixDataDir()
{
    /* This is:
        Windows: %PROGRAMDATA%/gix
        Linux: $INSTALLDIR/share/gix (e.g. /opt/gix-ide/share/gix or /usr/share/gix)
        Mac: $HOME/Library/gix
    */

    // Override from environment
    auto qba = qgetenv("GIX_DATA_DIR");
    if (!qba.isEmpty())
        return QString::fromUtf8(qba);

#if defined(Q_OS_WIN)
    if (_gix_data_dir.isEmpty()) {

        QSettings m("HKEY_LOCAL_MACHINE\\SOFTWARE\\MediumGray\\gix-ide", QSettings::Registry64Format);
        QVariant v = m.value("DataDir");
        if (!v.isValid()) {
            QSettings u("HKEY_CURRENT_USER\\SOFTWARE\\MediumGray\\gix-ide", QSettings::Registry64Format);
            v = u.value("DataDir");
            if (!v.isValid()) {
                _gix_data_dir = PathUtils::combine(qgetenv("LOCALAPPDATA"), "Gix");
            }
            else
                _gix_data_dir = v.toString();
        }
    }
    return _gix_data_dir;

#else
    return PathUtils::combine({ QDir::homePath(), ".gix" });
#endif
}

QString GixGlobals::getCompilerDefsDir()
{
    /* This is:
        $GIX_DATA_DIR/compiler-pkgs
        or
        %GIX_DATA_DIR%/compiler-pkgs
    */

    // Override from environment
    auto qba = getGixDataDir();

    QString compiler_base_dir = PathUtils::combine(getGixDataDir(), "compiler-defs");
    return QDir(compiler_base_dir).exists() ? compiler_base_dir : QString();
}


QString GixGlobals::getGixToolPath(QString tool)
{
    QString path = getGixBinDir();
#if defined(Q_OS_WIN)
    return PathUtils::combine(path, PathUtils::changeExtension(tool, ".exe"));
#else
    return PathUtils::combine(path, tool);
#endif
}

CompilerManager *GixGlobals::getCompilerManager()
{
    return instance.compiler_manager;
}

MetadataManager *GixGlobals::getMetadataManager()
{
    return instance.metadata_manager;
}

GixGlobals::GixGlobals()
{

}
