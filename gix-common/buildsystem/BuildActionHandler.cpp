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

#include "BuildActionHandler.h"
#include "BuildActionCompileHandler.h"
#include "BuildActionLinkHandler.h"
#include "BuildActionPreprocessESQLHandler.h"
#include "BuildActionMakeListingHandler.h"
#include "BuildActionNoOpHandler.h"
#include "PathUtils.h"
#include "SysUtils.h"
#include "BuildConsts.h"
#include "linq/linq.hpp"
#include "MacroManager.h"
#include "PropertySource.h"
#include "IBuildableItem.h"
#include "ProjectItem.h"


BuildActionHandler::~BuildActionHandler()
{
}

BuildActionHandler * BuildActionHandler::get(BuildTarget *bt)
{
	if (bt->isVirtual())
		return new BuildActionNoOpHandler();

	if (bt->buildAction() == BuildConsts::BUILD_ACTION_NONE)
		return new BuildActionNoOpHandler();

	BuildActionHandler *res = nullptr;

	if (bt->buildAction() == BuildConsts::BUILD_ACTION_COMPILE && 
		bt->providesOneOf({ BuildConsts::TYPE_OBJ, BuildConsts::TYPE_OBJ_MAIN }) &&
		bt->usesOneOf({ BuildConsts::TYPE_CBSQL, BuildConsts::TYPE_COBOL }))
	{
		res = new BuildActionCompileHandler();
	}

	if (bt->buildAction() == BuildConsts::BUILD_ACTION_LINK &&
		bt->usesOneOf({ BuildConsts::TYPE_LIB, BuildConsts::TYPE_OBJ, BuildConsts::TYPE_OBJ_MAIN }) &&
		bt->providesOneOf({ BuildConsts::MODULE_DYNLOAD, BuildConsts::MODULE_EXECUTABLE }))
	{
		res = new BuildActionLinkHandler();
	}

	if (bt->buildAction() == BuildConsts::BUILD_ACTION_PREPROC_ESQL && bt->provides(BuildConsts::TYPE_CBSQL)  && 
		(bt->usesOneOf({ BuildConsts::TYPE_COBOL, BuildConsts::TYPE_CBLPP }))) {
		res = new BuildActionPreprocessESQLHandler();
	}

	if (bt->buildAction() == BuildConsts::BUILD_ACTION_MK_LISTING && bt->provides(BuildConsts::TYPE_LISTING)) {
		res = new BuildActionMakeListingHandler();
	}


	if (res != nullptr)
		res->target = bt;

	return res;
}


void BuildActionHandler::addEnvironment(QMap<QString, QVariant>&e)
{
	SysUtils::mergeMaps(environment, e);
}

void BuildActionHandler::addEnvironment(QString s, QVariant v)
{
	environment.insert(s, v);
}

void BuildActionHandler::setMainBuilder(BuildDriver *ib)
{
	build_driver = ib;
}

void BuildActionHandler::readStdOut(QProcess *p)
{
	p->setReadChannel(QProcess::ProcessChannel::StandardOutput);
	QByteArray qba = p->readAll();
	QString s(qba);
	if (!s.isEmpty())
		build_driver->log_build_message(s, QLogger::LogLevel::Info);
}

void BuildActionHandler::readStdErr(QProcess *p)
{
	p->setReadChannel(QProcess::ProcessChannel::StandardError);
	QByteArray qba = p->readAll();
	QString s(qba);
	if (!s.isEmpty())
		build_driver->log_build_message(s, QLogger::LogLevel::Error);
}

QString BuildActionHandler::getBuildDirectory()
{
	MacroManager mm(environment);
	QString build_dir = mm.translate(environment["prj.build_dir"].toString());
	return build_dir;
}

void BuildActionHandler::importProjectEnvironment()
{
	if (!target || !target->getItem())
		return;

	ProjectItem *pi = dynamic_cast<ProjectItem *>(target->getItem());
	while (pi != nullptr && pi->GetItemType() != ProjectItemType::TProject)
		pi = pi->GetParent();

	if (!pi)
		return;

	PropertySource *ps = dynamic_cast<PropertySource *>(pi);
	if (!ps)
		return;

	addEnvironment(*ps->PropertyGetCurrentValues());
	addEnvironment(ps->getRuntimeProperties());
}

void BuildActionHandler::importFileEnvironment()
{
	if (!target || !target->getItem())
		return;

	ProjectItem *pi = dynamic_cast<ProjectItem *>(target->getItem());
	if (!pi || pi->GetItemType() != ProjectItemType::TFile)
		return;

	PropertySource *ps = dynamic_cast<PropertySource *>(pi);
	if (!ps)
		return;

	addEnvironment(*ps->PropertyGetCurrentValues());
	addEnvironment(ps->getRuntimeProperties());
}
