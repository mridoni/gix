/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
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

#include "WebProject.h"
#include "GixGlobals.h"
#include "SingleBinaryProject.h"
#include "BuildConsts.h"
#include "PathUtils.h"

WebProject::WebProject(const QMap<QString, QVariant>& opts)
{
	this->type = ProjectType::Web;
}

WebProject::WebProject(const Project & prj)
{}

BuildTarget *WebProject::getBuildTarget(QMap<QString, QVariant> env, BuildTarget *parent)
{
	QString target_type = "webartifact/dll";
	return GixGlobals::getTargetManager()->composeBuildTarget(parent, target_type, &env, ((IBuildableItem *)this));
}

QList<IBuildableItem *> WebProject::getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership)
{
	QList<IBuildableItem *> res;

	//if (use == "singleartifact/dll" || use == "singleartifact/exe") {
	res = splitProject();
	*yield_ownership = true;
	//}

	return res;
}


QList<IBuildableItem *> WebProject::splitProject()
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

		pm->PropertySetValue("build_type", BuildConsts::MODULE_DYNLOAD);

		QString module_name = PathUtils::toModuleName(pf->GetFileFullPath());
		pm->PropertySetValue("target_name", module_name);

		res.append(pm);
	}
	return res;
}
