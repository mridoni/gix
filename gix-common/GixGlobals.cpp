#include "GixGlobals.h"
#include "PathUtils.h"

GixGlobals GixGlobals::instance;

bool GixGlobals::initManagers()
{
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
    QString programdata_dir = qgetenv("PROGRAMDATA");
    QString gix_data_dir = PathUtils::combine(programdata_dir, "gix");
    return QDir(gix_data_dir).exists() ? gix_data_dir : QString();
#elif defined(Q_OS_LINUX)
    QDir binDir = QDir(getGixBinDir());
    if (binDir.isEmpty() || !binDir.cdUp())
        return QString();

    return binDir.absolutePath();
#elif defined(Q_OS_MAC)
    QString gix_data_dir = PathUtils::combine({ QDir::homePath(), "gix", "share" });
    return QDir(gix_data_dir).exists() ? gix_data_dir : QString();
#else
#error "Unknown platform"
#endif
}

QString GixGlobals::getCompilerBaseDir()
{
    /* This is:
        $GIX_DATA_DIR/compiler-pkgs
        or
        %GIX_DATA_DIR%/compiler-pkgs
    */

    // Override from environment
    auto qba = qgetenv("GIX_COMPILER_BASE_DIR");
    if (!qba.isEmpty())
        return QString::fromUtf8(qba);

    QString compiler_base_dir = PathUtils::combine(getGixDataDir(), "compiler-pkgs");
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
