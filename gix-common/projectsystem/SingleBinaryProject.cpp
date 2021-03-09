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

#include "SingleBinaryProject.h"

#include <QFileInfo>

#include "MacroManager.h"
#include "TargetManager.h"
#include "BuildConsts.h"
#include "SysUtils.h"
#include "GixGlobals.h"

SingleBinaryProject::SingleBinaryProject()
{
	this->type = ProjectType::SingleBinary;
}

SingleBinaryProject::SingleBinaryProject(const QMap<QString, QVariant> &opts) : SingleBinaryProject()
{
	this->properties = &this->properties->unite(opts);
}

SingleBinaryProject *SingleBinaryProject::fromProject(const Project *prj, bool copy_children)
{
	SingleBinaryProject *sp = new SingleBinaryProject();
	// from project
	sp->description = prj->description;

	// from projectitem
	sp->parent_item = prj->parent_item;
	sp->filepath = prj->filepath;

	// from propertysource
	sp->properties = new QVariantMap(*prj->properties);
	sp->runtime_properties = QVariantMap(prj->runtime_properties);

	sp->is_virtual = prj->is_virtual;

	if (copy_children) {

		sp->children = prj->children;
	}
	return sp;
}

BuildTarget *SingleBinaryProject::getBuildTarget(QMap<QString, QVariant> env, BuildTarget *parent)
{
	QString target_type = "singleartifact/" + PropertyGetValue("build_type").toString();
	return GixGlobals::getTargetManager()->composeBuildTarget(parent, target_type, &env, ((IBuildableItem *)this));
}

QList<IBuildableItem *> SingleBinaryProject::getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership)
{
	QList<IBuildableItem *> res;

	if (use == "obj" || use == "cbl") {
		QList<ProjectFile *> files = this->getAllCompilableFiles();
		for (auto file : files) {
			res.append((IBuildableItem *)file);
		}
	}

	return res;
}
