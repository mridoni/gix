#pragma once

#include <QString>
#include <QtGlobal>

#include "gixcommon_global.h"
#include "TargetManager.h"
#include "CompilerManager.h"
#include "MetadataManager.h"
#include "ProjectCollection.h"
#include "IGixLogManager.h"

typedef std::function<QString()> f_ret_qstring_callback;
typedef std::function<ProjectCollection *()> f_ret_prjcoll_callback;

class GIXCOMMON_EXPORT GixGlobalsCallbacks
{
public:
	f_ret_qstring_callback getCurrentConfiguration = nullptr;
	f_ret_qstring_callback getCurrentPlatform = nullptr;
	f_ret_prjcoll_callback getCurrentProjectCollection = nullptr;
};

class GIXCOMMON_EXPORT GixGlobals
{
public:

	static bool registerLogManager(IGixLogManager *logger);
	static bool registerCallbacks(GixGlobalsCallbacks *callbacks);


	static bool initManagers();

	static QString getCurrentConfiguration();
	static QString getCurrentPlatform();
	static TargetManager *getTargetManager();
	static IGixLogManager *getLogManager();
	static QString getGixHomeDir();
	static QString getGixBinDir();
	static QString getGixLibDir(QString target_platform);
	static QString getGixDataDir();
	static QString getCompilerDefsDir();
	static QString getGixToolPath(QString tool);
	static ProjectCollection *getCurrentProjectCollection();
	static CompilerManager *getCompilerManager();
	static MetadataManager *getMetadataManager();

private:
	GixGlobals();

	TargetManager *target_manager = nullptr;
	CompilerManager *compiler_manager = nullptr;
	MetadataManager *metadata_manager = nullptr;
	IGixLogManager *logger = nullptr;

	f_ret_qstring_callback cb_getCurrentConfiguration;
	f_ret_qstring_callback cb_getCurrentPlatform;
	f_ret_prjcoll_callback cb_getCurrentProjectCollection;

	static GixGlobals instance;

	// Only used on Windows to avoid a registry lookup
#if defined(Q_OS_WIN)
	static QString _gix_data_dir;
#endif
};

