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

#include "MultiBinaryProject.h"
#include "SingleBinaryProject.h"
#include "PropertyConsts.h"
#include "MacroManager.h"
#include "PathUtils.h"
#include "TargetManager.h"
#include "BuildConsts.h"
#include "SysUtils.h"
#include "GixGlobals.h"
#include "CobolUtils.h"

MultiBinaryProject::MultiBinaryProject(const QMap<QString, QVariant> &opts)
{
	this->type = ProjectType::MultipleBinaries;
}

MultiBinaryProject::MultiBinaryProject(const Project &prj)
{}

BuildTarget *MultiBinaryProject::getBuildTarget(QMap<QString, QVariant> env, BuildTarget *parent)
{
	QString target_type = "multiartifact/" + PropertyGetValue("build_type").toString();
	return GixGlobals::getTargetManager()->composeBuildTarget(parent, target_type, &env, ((IBuildableItem *)this));
}

QList<IBuildableItem *> MultiBinaryProject::getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership)
{
	QList<IBuildableItem *> res;

	if (use == "singleartifact/dll" || use == "singleartifact/exe") {
		res = splitProject();
		*yield_ownership = true;
	}

	return res;
}

QList<IBuildableItem *> MultiBinaryProject::splitProject()
{

	QList<IBuildableItem *> res;
	QList<ProjectFile *> prj_files = this->getAllCompilableFiles();

	for (int i = 0; i < prj_files.size(); i++) {
		//ProjectFile *pf = prj_files.at(i);

		ProjectFile *porig = prj_files.at(i);

		ProjectFile *pf = porig->clone();

		QString filename = pf->GetFileFullPath();

		SingleBinaryProject *pm = SingleBinaryProject::fromProject(this);
		pm->setDirty(false);
		pm->setVirtual(true);

		pm->GetChildren()->append(pf);
		pf->SetParent(pm);

		QVariant file_build_type = pf->PropertyGetValue(PropertyConsts::CustomBuildType);
		if (file_build_type.isNull() || file_build_type.type() != QVariant::Type::String || file_build_type.toString().isEmpty()) {	// Default
			file_build_type = this->PropertyGetValue("build_type");
			pm->PropertySetValue(PropertyConsts::CustomBuildType, file_build_type);
			pm->PropertySetValue(PropertyConsts::IsStartupItem, this->PropertyGetValue("build_type").toString() == BuildConsts::MODULE_EXECUTABLE);
		}
		else {	// User-set
			QString custom_build_type = file_build_type.toString();
			pm->PropertySetValue("build_type", custom_build_type);
			pm->PropertySetValue(PropertyConsts::IsStartupItem, custom_build_type == BuildConsts::MODULE_EXECUTABLE);
		}

		QString module_name = CobolUtils::extractProgramId(pf->GetFileFullPath());
		QString ext;
		if (this->PropertyGetValue("build_type").toString() == BuildConsts::MODULE_EXECUTABLE) {
			ext = SysUtils::isWindows() ? ".exe" : "";
		}
		else {
			ext = SysUtils::isWindows() ? ".dll" : ".so";
		}

		pm->PropertySetValue("target_name", module_name + "." + ext);

		res.append(pm);
	}
	return res;
}

