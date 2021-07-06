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

#include "TargetManager.h"
#include "TargetDefinition.h"
#include "BuildTarget.h"
#include "MacroManager.h"
//#include "Ide.h"
//#include "IdeTaskManager.h"
#include "PathUtils.h"

#include <QFile>

TargetManager::TargetManager()
{
	init();
}

TargetManager::~TargetManager()
{
	QMap<QString, TargetDefinition *>::iterator it;

	for (it = target_defs.begin(); it != target_defs.end(); ++it) {
		if (it.value())
			delete it.value();
	}
}

void TargetManager::init()
{

	QFile target_def_file(":/gix-targets.xml");

	QDomDocument doc;
	if (!target_def_file.open(QIODevice::ReadOnly) || !doc.setContent(&target_def_file))
		return;

	target_def_file.close();

	auto xtarget_defs = doc.firstChildElement().elementsByTagName("target");
	for (int i = 0; i < xtarget_defs.count(); i++) {
		QDomElement xtdef = xtarget_defs.at(i).toElement();

		TargetDefinition *tdef = parseTargetDef(xtdef);
		if (!tdef)
			continue;

		target_defs[tdef->fulltype] = tdef;
	}

}

/*
	<target type="obj" esql="false" provider="file" expected="file">
		<filename>${inputfile.noext}.${sys.objext}</filename>
		<provides>obj,sym</provides>
		<uses>cbl</uses>
		<location></location>
		<action>
			<action_id>compile</action_id>
			<action_args>*</action_args>
		</action>
	</target>
*/
BuildTarget *TargetManager::composeBuildTarget(BuildTarget *parent, const QString &target_type, QVariantMap *props, IBuildableItem *bi)
{
	auto bi_props = dynamic_cast<PropertySource *>(bi)->PropertyGetCurrentValues();
	for (QVariantMap::iterator it = bi_props->begin(); it != bi_props->end(); ++it) {
		props->insert(it.key(), it.value());
	}

	auto rt_props = dynamic_cast<PropertySource *>(bi)->getRuntimeProperties();
	for (QVariantMap::iterator it = rt_props.begin(); it != rt_props.end(); ++it) {
		props->insert(it.key(), it.value());
	}

	auto pi = dynamic_cast<ProjectItem *>(bi);

	TargetDefinition *tdef = lookupTarget(target_type, props);
	if (!tdef)
		return nullptr;

	if (!tdef->handler.isEmpty())
		return composeTargetUsingHandler(tdef, parent, target_type, props, bi);

	MacroManager mm(*props);

	BuildTarget *bt = new BuildTarget(parent);
	bt->setItem(bi);
	bt->setProvides(tdef->provides);
	bt->setUses(tdef->uses);
	bt->setBuildAction(tdef->action_id);
	bt->setOptional(tdef->is_optional);
	bt->setVirtual(tdef->is_virtual);

	QString t_filename = mm.translate(tdef->filename);
	if (QDir::isAbsolutePath(t_filename))
		bt->setFilename(t_filename);
	else
		bt->setFilename(mm.translate(tdef->location) + "/" + t_filename);

	//bt->setLocation(mm.translate(tdef->location), pi->GetBaseDir());

	for (auto use : tdef->uses) {
		bool assume_ownership = false;
		QList<IBuildableItem *> bi_deps = bi->getDependencies(use, props, &assume_ownership);
		for (IBuildableItem *sub_bi : bi_deps) {
			BuildTarget *sub_target = composeBuildTarget(bt, use, props, sub_bi);
			if (!sub_target)
				continue;

			sub_target->setItemOwnership(assume_ownership);
			bt->dependencies()->append(sub_target);

		}
	}

	return bt;
}

TargetDefinition *TargetManager::parseTargetDef(const QDomElement &xe)
{
	TargetDefinition *td = new TargetDefinition();
	td->type = xe.attribute("type");
	td->filename = xe.firstChildElement("filename").text();
	td->is_optional = xe.hasAttribute("optional") && xe.attribute("optional") == "true";
	td->is_virtual = xe.hasAttribute("virtual") && xe.attribute("virtual") == "true";
	td->provides = xe.firstChildElement("provides").text().split(',');
	td->uses = xe.firstChildElement("uses").text().split(',');
	td->location = xe.firstChildElement("location").text();
	td->handler = xe.firstChildElement("handler").text();

	td->action_id = xe.firstChildElement("action").firstChildElement("action_id").text();
	td->action_args = xe.firstChildElement("action").firstChildElement("action_args").text();
	

	QString full_id = td->type;

	if (xe.hasAttribute("esql")) {
		full_id += QString("/esql=%1").arg(xe.attribute("esql"));
		td->resolve_checks.insert("preprocess_esql", xe.attribute("esql") == "true");
	}

	if (xe.hasAttribute("consolidate")) {
		full_id += QString("/consolidate=%1").arg(xe.attribute("consolidate"));
		td->resolve_checks.insert("consolidate_source", xe.attribute("consolidate") == "true");
	}

	td->fulltype = full_id;

	return td;
}

TargetDefinition *TargetManager::lookupTarget(const QString &tt, QVariantMap *props)
{
	
	QMap<QString, bool>::iterator it;

	for (TargetDefinition *tdef : target_defs.values()) {
		if (tt != tdef->type)
			continue;

		bool res_checks = true;
		for (it = tdef->resolve_checks.begin(); it != tdef->resolve_checks.end(); ++it) {
			QVariant v = props->value(it.key());
			if (v.isNull() || v != it.value()) {
				res_checks = false;
				break;
			}
		}
		if (res_checks) {
			//Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("Looking up target type %1: found").arg(tt), QLogger::LogLevel::Trace);
			return tdef;
		}
	}

	//Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("Looking up target type %1: NOT found").arg(tt), QLogger::LogLevel::Trace);
	return nullptr;
}

BuildTarget *TargetManager::composeTargetUsingHandler(TargetDefinition *tdef, BuildTarget *parent, const QString &target_type, QVariantMap *props, IBuildableItem *bi)
{
	return nullptr;
}
