#pragma once

#include "BuildActionHandler.h"

class BuildActionLinkHandler : public BuildActionHandler
{
public:
	BuildActionLinkHandler();
	~BuildActionLinkHandler();

	// Inherited via BuildActionHandler
	virtual bool startBuild() override;

private:
	QStringList retrieve_link_dirs();
	QStringList retrieve_link_libs();

		
	bool generateDebugHelperObj(QStringList srclist, QString target_path, QString build_dir, QString &dbg_helper_obj);
	bool rebuildMetadata(const QStringList& mod_src_list);
	bool compileDebugHelperObj(QString build_dir, QString c_filename, QString obj_filename);
};

