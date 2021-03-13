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

#include "BuildTarget.h"
#include "PathUtils.h"
#include "IBuildableItem.h"
#include "GixGlobals.h"
#include "IGixLogManager.h"
#include "SingleBinaryProject.h"
#include "WebProject.h"

#include <QFileInfo>
#include <QDateTime>
#include <QDir>

QList<BuildTarget*> *BuildTarget::dependencies()
{
	return target_dependencies;
}

bool BuildTarget::isUpToDate()
{
	IGixLogManager *logger = GixGlobals::getLogManager();

	QString cd = QDir::currentPath();
	if (!QFile(filename()).exists()) {
		logger->logMessage(GIX_CONSOLE_LOG, filename() + " IS NOT UP TO DATE (0)", QLogger::LogLevel::Trace);
		return false;
	}

	QDateTime target_mtime = QFileInfo(filename()).lastModified();
	for (BuildTarget *dep : *target_dependencies) {
		if (!dep->isUpToDate()) {
			logger->logMessage(GIX_CONSOLE_LOG, dep->filename() + " IS NOT UP TO DATE (1)", QLogger::LogLevel::Trace);
			return false;
		}
	}

	for (BuildTarget *dep : *target_dependencies) {
		if (!QFile(dep->filename()).exists()) {
			logger->logMessage(GIX_CONSOLE_LOG, dep->filename() + " IS NOT UP TO DATE (2)", QLogger::LogLevel::Trace);
			return false;
		}

		QDateTime dep_mtime = QFileInfo(dep->filename()).lastModified();
		if (dep_mtime > target_mtime) {
			logger->logMessage(GIX_CONSOLE_LOG, QString("IS NOT UP TO DATE (3): T(%1): %2, D(%3): %4").arg(QFileInfo(this->filename()).fileName()).arg(target_mtime.toString()).arg(QFileInfo(dep->filename()).fileName()).arg(dep_mtime.toString()), QLogger::LogLevel::Error);
			return false;
		}
	}

	logger->logMessage(GIX_CONSOLE_LOG, filename() + " IS UP TO DATE", QLogger::LogLevel::Trace);
	return true;
}

QString BuildTarget::toString()
{
	int level = 0;
	QString res;
	res.append(QString(0, QChar(' ')) + "[\n");
	dump_target(this, res, &level);
	res.append(QString(0, QChar(' ')) + "]\n");
	return res;
}

bool BuildTarget::isOptional()
{
	return is_optional;
}

void BuildTarget::setOptional(bool b)
{
	is_optional = b;
}

bool BuildTarget::isVirtual()
{
	return is_virtual;
}

void BuildTarget::setVirtual(bool b)
{
	is_virtual = b;
}

IBuildableItem *BuildTarget::getItem()
{
	return item;
}

void BuildTarget::setItem(IBuildableItem *pi)
{
	item = pi;
}

BuildTarget *BuildTarget::getDependency(const QString &p)
{
	for (BuildTarget *bd : *target_dependencies) {

		if (bd->provides(p))
			return bd;
	}

	return nullptr;
}

void BuildTarget::setItemOwnership(bool b)
{
	has_item_ownership = b;
}

void BuildTarget::dump_target(BuildTarget *bt, QString &res, int *level)
{
	QChar SPACE(' ');

	res.append(QString(*level * 4, SPACE) + "    target_filename : " + bt->filename() + "\n");
	res.append(QString(*level * 4, SPACE) + "    build_action    : " + bt->buildAction() + "\n");
	res.append(QString(*level * 4, SPACE) + "    optional        : " + (bt->isOptional() ? "true" : "false") + "\n");
	res.append(QString(*level * 4, SPACE) + "    virtual         : " + (bt->isVirtual() ? "true" : "false") + "\n");
	res.append(QString(*level * 4, SPACE) + "    target_provides : " + bt->provides().join(QChar(',')) + "\n");
	res.append(QString(*level * 4, SPACE) + "    target_uses     : " + bt->uses().join(QChar(',')) + "\n");
	res.append(QString(*level * 4, SPACE) + "    has_ownership   : " + (bt->has_item_ownership ? "true" : "false") + "\n");
	res.append(QString(*level * 4, SPACE) + "    item            : " + (bt->item ? ((ProjectItem *) bt->item)->getGuid() : "N/A") + "\n");
	res.append(QString(*level * 4, SPACE) + "    dependencies    : [\n");
	(*level)++;
	for (int i = 0; i < bt->dependencies()->size(); i ++) {
		dump_target(bt->dependencies()->at(i), res, level);
	}
	(*level)--;
	res.append(QString(*level * 4, SPACE) + "    ]\n");

}

BuildTarget::BuildTarget(BuildTarget *p)
{
	is_optional = false;
	is_virtual = false;
	this->target_parent = p;
	target_dependencies = new QList<BuildTarget *>();
	item = nullptr;
	has_item_ownership = false;
}

BuildTarget::~BuildTarget()
{
	if (has_item_ownership && item) {
		Project::deleteItem(item);
	}

	for (BuildTarget *bt : *target_dependencies)
		delete bt;

	delete target_dependencies;
}

bool BuildTarget::providesOneOf(QStringList clist)
{
	for (QString s : clist)
		if (target_provides.contains(s))
			return true;
	
	return false;
}

QStringList BuildTarget::provides()
{
	return target_provides;
}

bool BuildTarget::provides(QString c)
{
	return target_provides.contains(c);
}

QStringList BuildTarget::uses()
{
	return target_uses;
}

bool BuildTarget::uses(QString c)
{
	return target_uses.contains(c);
}

bool BuildTarget::usesOneOf(QStringList clist)
{
	for (QString s : clist)
		if (target_uses.contains(s))
			return true;

	return false;
}

QString BuildTarget::filename()
{
	return target_filename;
}

QString BuildTarget::buildAction()
{
	return target_buildAction;
}

QString BuildTarget::buildSubAction()
{
	return target_buildSubAction;
}

void BuildTarget::setProvides(QStringList s)
{
	target_provides = s;
}

void BuildTarget::setProvides(QString s)
{
	target_provides.clear();
	target_provides << s;
}

void BuildTarget::setUses(QStringList s)
{
	target_uses = s;
}

void BuildTarget::setUses(QString s)
{
	target_uses.clear();
	target_uses << s;
}

void BuildTarget::setFilename(QString s)
{
	target_filename = s;
}

void BuildTarget::setBuildAction(QString s)
{
	target_buildAction = s;
}

void BuildTarget::setBuildSubAction(QString s)
{
	target_buildSubAction = s;
}

//void BuildTarget::setLocation(QString s, QString stem_dir)
//{
//	if (s.isEmpty() || QDir::isAbsolutePath(filename()))
//		return;
//
//	if (QDir::isRelativePath(s))
//		s = PathUtils::combine(stem_dir, s);
//
//	target_filename = PathUtils::combine(s, filename());
//}

void BuildTarget::setParent(BuildTarget *p)
{
	this->target_parent = p;
}

BuildTarget * BuildTarget::parent()
{
	return this->target_parent;
}
