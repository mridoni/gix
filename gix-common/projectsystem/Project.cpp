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

#include "Project.h"

#include <QDomDocument>
#include <QFile>
#include <QDir>
#include <QTextStream>
#include <QDebug>
#include <QRegularExpression>

#include "SysUtils.h"
#include "BuildConsts.h"
#include "PropertyConsts.h"
#include "MacroManager.h"
#include "PathUtils.h"
#include "GixGlobals.h"
#include "linq/linq.hpp"

#include "SingleBinaryProject.h"
#include "MultiBinaryProject.h"
#include "WebProject.h"

using namespace cpplinq;

static ProjectPropertyDefinitionCollection property_defs;

Project* Project::newProject(ProjectType type, ProjectFileType add_filetype, QString prj_filepath, const QMap<QString, QVariant>& opts)
{
	Project* prj = nullptr;
	switch (type) {
		case ProjectType::SingleBinary:
			prj = new SingleBinaryProject(opts);
			break;

		case ProjectType::MultipleBinaries:
			prj = new MultiBinaryProject(opts);
			break;

		case ProjectType::Web:
			prj = new WebProject(opts);
			break;
	}

	if (prj) {
		prj->filepath = PathUtils::changeExtension(prj_filepath, "") + "/" + prj_filepath + ".gixprj";

		for (auto pdef : property_defs.getAll()) {
			prj->properties->insert(pdef->Name, pdef->DefaultValue);
		}

		QString basename = QFileInfo(prj->filepath).fileName();
		QString target_name = PathUtils::changeExtension(basename, "").toUpper();

		prj->PropertySetValue("target_name", target_name);

		QStringList copy_path({ "${prj.basedir}" });
		prj->PropertySetValue("copy_include_path", copy_path);

		if (add_filetype != ProjectFileType::NoFile) {
			QString fdir = QFileInfo(prj->filepath).absolutePath();
			QString filepath = PathUtils::changeExtension(basename, "").toUpper() + ".cbl";
			ProjectFile* pf = new ProjectFile();
			pf->filepath = filepath;
			pf->PropertySetValue("build_action", "compile");
			pf->PropertySetValue(PropertyConsts::IsStartupItem, (opts["build_type"].toString() == "exe"));
			prj->addFile(pf);
		}

		prj->runtime_properties["prj.path"] = prj->GetFileFullPath();
		prj->runtime_properties["prj.basedir"] = prj->GetBaseDir();
	}



	return prj;
}

Project* Project::loadProject(ProjectItem* owner, QString filepath)
{
	QDomDocument doc;
	QFile prj_file(PathUtils::combine(owner->GetBaseDir(), filepath));
	if (!prj_file.open(QIODevice::ReadOnly) || !doc.setContent(&prj_file))
		return nullptr;

	prj_file.close();

	auto a = doc.firstChildElement().tagName();
	if (doc.elementsByTagName("type").isEmpty())
		return nullptr;

	QDomElement xprj_type = doc.elementsByTagName("type").item(0).toElement();
	if (xprj_type.isNull())
		return nullptr;

	int prj_type = xprj_type.text().toInt();
	if (!prj_type)
		return nullptr;

	Project* prj = newProject((ProjectType)prj_type, ProjectFileType::NoFile, "", QVariantMap());
	if (!prj)
		return nullptr;

	if (!prj->load(owner, filepath))
		return nullptr;

	QString pdesc;
	switch (prj->getType()) {
		case ProjectType::SingleBinary:
			pdesc = QCoreApplication::translate("gix", "Single binary");
			break;

		case ProjectType::MultipleBinaries:
			pdesc = QCoreApplication::translate("gix", "Multiple binaries");
			break;

		case ProjectType::Web:
			pdesc = QCoreApplication::translate("gix", "Web project");
			break;

	}
	prj->PropertySetValue("__project_type", pdesc);
	prj->PropertySetValue("__project_type_id", (int)prj->getType());

	prj->runtime_properties["prj.path"] = prj->GetFileFullPath();
	prj->runtime_properties["prj.basedir"] = prj->GetBaseDir();

	return prj;
}

Project::Project() : ProjectItem()
{
	is_dirty = true;
	type = ProjectType::NoProject;
}


Project::~Project()
{

}

Project::Project(const Project& o) : ProjectItem()
{
	for (int i = 0; i < o.children->size(); i++) {
		this->children->append(o.children->at(i));
	}

	for (int i = 0; i < o.properties->keys().size(); i++) {
		QString key = o.properties->keys().at(i);
		this->properties->insert(key, o.properties->value(key));
	}

	this->description = o.description;
	this->filepath = o.filepath;
	this->parent_item = o.parent_item;
}

void Project::addFile(ProjectFile* pf)
{
	if (!pf)
		return;

	pf->parent_item = this;
	this->children->append(pf);
}

void Project::addToCopyPathList(QString)
{}

int Project::getLoadableItemCount(QString filepath)
{
	int res = 0;

	QDomDocument doc;
	QFile prj_file(filepath);
	if (!prj_file.open(QIODevice::ReadOnly) || !doc.setContent(&prj_file))
		return false;

	prj_file.close();

	QDomNode items_container = doc.elementsByTagName("items").item(0);
	QDomNodeList items = items_container.childNodes();
	for (int i = 0; i < items.size(); i++) {

		QDomElement xitem = items.item(i).toElement();
		if (QString::compare(xitem.nodeName(), "file") == 0) {
			res++;
		}
	}

	return res;
}

int Project::getLoadableItemCount()
{
	QList<ProjectFile*> prj_files;
	add_compilable_children(prj_files, (ProjectItem*)this);
	return prj_files.size();
}

QList<ProjectFile*> Project::getAllCompilableFiles()
{
	QList<ProjectFile*> files;
	add_compilable_children(files, (ProjectItem*)this);
	return files;
}

QList<ProjectFile*> Project::getAllCopyFiles()
{
	QList<ProjectFile*> files;
	add_copy_children(files, (ProjectItem*)this);
	return files;
}

QString Project::getStartupItemName()
{
	ProjectFile* pf = getStartupItem();
	if (pf)
		return PathUtils::toModuleName(pf->GetFilename());

	return QString();
}

ProjectFile* Project::getStartupItem()
{
	QList<ProjectFile*> prj_files = this->getAllCompilableFiles();
	QList<ProjectFile*>::iterator it;

	for (it = prj_files.begin(); it != prj_files.end(); ++it) {
		ProjectFile* f = (*it);
		if (f->PropertyGetValue(PropertyConsts::IsStartupItem, false).toBool()) {
			return f;
		}
	}
	return nullptr;
}

ProjectType Project::getType()
{
	return type;
}

bool Project::load(ProjectItem* owner, QString filepath)
{
	this->filepath = filepath;
	this->parent_item = owner;

	QDomDocument doc;
	QFile prj_file(PathUtils::combine(owner->GetBaseDir(), filepath));
	if (!prj_file.open(QIODevice::ReadOnly) || !doc.setContent(&prj_file))
		return false;

	prj_file.close();

	QDomNode items_container = doc.elementsByTagName("items").item(0);
	append_children(items_container, this);

	QDomNode settings_container = doc.elementsByTagName("settings").item(0);
	QDomNodeList prj_settings = settings_container.childNodes();
	for (int i = 0; i < prj_settings.size(); i++) {

		QDomElement xsetting = prj_settings.item(i).toElement();
		QString key = xsetting.nodeName();
		PropertyDefinition* pd = property_defs.getPropertyDefinition(key);
		if (pd == nullptr)
			continue;

		properties->insert(key, pd->parse(xsetting.text()));
	}
	return true;
}

void Project::append_children(QDomNode& parent, ProjectItem* pi)
{
	QDomNodeList items = parent.childNodes();
	for (int i = 0; i < items.size(); i++) {

		QDomElement xitem = items.item(i).toElement();
		if (QString::compare(xitem.nodeName(), "file") == 0) {
			QString file_path = xitem.attribute("path", nullptr);
			if (file_path == nullptr)
				continue;

			ProjectFile* file = new ProjectFile();
			if (!((IPersistableProjectItem*)file)->load(pi, file_path))
				continue;

			pi->GetChildren()->append(file);
			for (int n = 0; n < xitem.childNodes().size(); n++) {
				QDomElement xprop = xitem.childNodes().at(n).toElement();
				QString key = xprop.tagName();
				if (key.startsWith("__"))
					continue;
				QString val = xprop.text();
				PropertyDefinition* pd = file->PropertyGetDefinitions().getPropertyDefinition(key);
				if (pd != nullptr) {
					file->PropertySetValue(key, val);
				}
			}
		}

		if (QString::compare(xitem.nodeName(), "folder") == 0) {
			QString folder_path = xitem.attribute("path", nullptr);
			if (folder_path == nullptr)
				continue;

			ProjectFolder* folder = new ProjectFolder(folder_path, false);
			folder->SetParent(pi);
			pi->GetChildren()->append(folder);

			append_children(xitem, folder);
		}
	}
}

bool Project::save(ProjectItem* owner, QString fp)
{
	if (!owner) {
		if (!parent_item)
			return false;
		else
			owner = parent_item;
	}

	if (fp.isEmpty()) {
		if (this->filepath.isEmpty())
			return false;
		else
			fp = this->filepath;
	}

	this->filepath = fp;

	if (is_virtual)
		return true;

	QDomDocument doc;
	QFile prj_file(PathUtils::combine(owner->GetBaseDir(), filepath));
	QString pf = PathUtils::combine(owner->GetBaseDir(), filepath);

	QFileInfo prj_file_info(pf);
	QString prj_dir = prj_file_info.dir().path();
	QFileInfo prj_dir_info(prj_dir);

	if (prj_dir_info.exists() && !prj_dir_info.isDir())
		return false;

	if (!prj_dir_info.exists()) {
		if (!QDir(".").mkpath(prj_dir))
			return false;
	}

	QDomElement xprj = doc.createElement("gix-project");
	doc.appendChild(xprj);
	xprj.setAttribute("version", "1.0");

	QDomElement xprj_type = doc.createElement("type");
	xprj_type.appendChild(doc.createTextNode(QString::number((int)this->type)));
	xprj.appendChild(xprj_type);

	QDomElement items_container = doc.createElement("items");
	xprj.appendChild(items_container);

	this->dump_children(items_container, this);

	QDomElement settings_container = doc.createElement("settings");
	xprj.appendChild(settings_container);
	QMap<QString, QVariant>::iterator si;

	for (si = properties->begin(); si != properties->end(); ++si) {
		QString pkey = si.key();
		if (pkey.startsWith("__"))
			continue;

		PropertyDefinition* pd = property_defs.getPropertyDefinition(pkey);
		QDomElement xs = doc.createElement(si.key());
		xs.appendChild(doc.createTextNode(pd->serialize(si.value())));
		settings_container.appendChild(xs);
	}

	QFile file(GetFileFullPath());

	if (file.exists())
		file.copy(GetFileFullPath() + ".bak");

	if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
		//qDebug("Failed to open file for writing: " + filepath);
		return false;
	}

	QTextStream stream(&file);
	stream << doc.toString();
	file.close();

	return true;
}

bool Project::revert()
{
	return false;
}

bool Project::isDirty()
{
	return is_dirty;
}

void Project::setDirty(bool b)
{
	is_dirty = b;
}

BuildTarget *Project::getPostBuildTarget(QMap<QString, QVariant> build_environment, BuildTarget *parent)
{
	return nullptr;
}

void Project::deleteItem(IBuildableItem *item)
{
	if (!item)
		return;

	SingleBinaryProject *sprj = dynamic_cast<SingleBinaryProject *>(item);
	if (sprj)
		delete sprj;
	else {
		WebProject *wprj = dynamic_cast<WebProject *>(item);
		if (wprj)
			delete wprj;
	}
}


ProjectFile* Project::locateProjectFileByPath(QString filepath, bool include_copy_files)
{

	QList<ProjectFile*> files;
	add_compilable_children(files, (ProjectItem*)this, include_copy_files);

	auto res = from(files).where([filepath](ProjectFile* pf) { return QDir::cleanPath(pf->GetFileFullPath()) == QDir::cleanPath(filepath);  }).to_vector();
	return (res.size() == 1) ? res.at(0) : nullptr;
}


QString Project::locateSourceFileByModuleName(QString module_name, bool in_debugging_session)
{
	QRegularExpression rxProgramId("PROGRAM\\-ID\\.\\ [\"']?+([A-Za-z0-9_\\-]+)[\"']?\\ *\\.");
	QList<ProjectFile*> files;
	add_compilable_children(files, (ProjectItem*)this);
	int nfiles = files.size();
	for (ProjectFile* pf : files) {
		QFile f(pf->GetFileFullPath());
		if (!f.exists())
			continue;

		if (!f.open(QIODevice::OpenModeFlag::ReadOnly))
			continue;

		QString content = QString::fromUtf8(f.readAll());
		f.close();

		QRegularExpressionMatch m = rxProgramId.match(content);
		if (m.hasMatch()) {
			QString c = m.captured(1);
			if (m.captured(1).toUpper().trimmed() == module_name.toUpper().trimmed()) {
				if (!in_debugging_session || !isEsql()) {
					return pf->GetFileFullPath();
				}

				QString src = pf->GetFileFullPath();
				src = PathUtils::getFilename(src);
				src = PathUtils::changeExtension(src, ".cbsql");
				src = PathUtils::combine(this->getBuildDirectory(GixGlobals::getCurrentConfiguration(), GixGlobals::getCurrentPlatform()), src);
				if (QFile::exists(src))
					return src;
			}

		}
	}
	return "";
}

QString Project::getBuildDirectory(QString configuration, QString platform)
{

	QString basedir = this->GetBaseDir();
	return QDir::cleanPath(PathUtils::combine({ basedir, "bin", configuration, platform }));
}

QStringList Project::getCopyDirList()
{
	QVariant cpath = this->PropertyGetValue("copy_include_path");
	if (cpath.isNull() || !cpath.isValid())
		return QStringList();

	QStringList res = cpath.toStringList();
	MacroManager mm(*properties);
	mm.add(this->runtime_properties);
	
	for (int i = 0; i < res.size(); i++) {
		res[i] = mm.translate(res.at(i));
	}

	return res;
}

QStringList Project::getCopyExtList()
{
	QVariant cs = this->PropertyGetValue("copy_ext_list");
	if (cs.isNull() || !cs.isValid())
		return QStringList();

	return cs.toString().split(",");
}

bool Project::hasHttpService()
{
	QList <ProjectFile*>::iterator it;
	QList<ProjectFile*> files = this->getAllCompilableFiles();

	for (it = files.begin(); it != files.end(); ++it) {
		if ((*it)->isHttpService())
			return true;
	}
	return false;
}

bool Project::isEsql()
{
	return PropertyGetValue("preprocess_esql", false).toBool();
}

QMap<QString, QString> Project::getBuildFileMap()
{
	//QMap<QString, QVariant> opts;
	//opts["configuration"] = Ide::TaskManager()->getCurrentConfiguration();
	//opts["platform"] = Ide::TaskManager()->getCurrentPlatform();
	//BuildTarget* bt;

	//bt = this->getBuilder(nullptr)->getBuildTarget(opts, nullptr);

	return QMap<QString, QString>();
}

ProjectItemType Project::GetItemType()
{
	return ProjectItemType::TProject;
}

QString Project::GetDisplayName()
{
	return this->GetFilename();
}

PropertyDefinitionCollection& Project::PropertyGetDefinitions()
{
	return property_defs;
}

// These are used at runtime (e.g. as placeholders for a execute target)
// and must not be exported to the project file
static QStringList property_exclude_list({ "filepath" });

void Project::dump_children(QDomNode& xparent, ProjectItem* pi)
{
	for (int i = 0; i < pi->GetChildren()->size(); i++) {
		ProjectItem* child = pi->GetChildren()->at(i);
		switch (child->GetItemType()) {
			case ProjectItemType::TFile:
			{
				QDomElement xfile = xparent.ownerDocument().createElement("file");
				ProjectFile* pf = (ProjectFile*)child;
				xfile.setAttribute("path", pf->GetFilename());
				QList<QString> keys = pf->PropertyGetCurrentValues()->keys();
				for (int n = 0; n < keys.size(); n++) {
					if (property_exclude_list.contains(keys.at(n)) || keys.at(n).startsWith("__"))
						continue;

					QDomElement xprop = xparent.ownerDocument().createElement(keys.at(n));
					PropertyDefinition* pd = pf->PropertyGetDefinitions().getPropertyDefinition(keys.at(n));
					if (!pd)
						continue;

					xprop.appendChild(xparent.ownerDocument().createTextNode(pf->PropertyGetValue(keys.at(n), pd->DefaultValue).toString()));
					xfile.appendChild(xprop);
				}

				xparent.appendChild(xfile);
			}
			break;

			case ProjectItemType::TFolder:
			{
				QDomElement xfldr = xparent.ownerDocument().createElement("folder");
				ProjectFolder* pf = (ProjectFolder*)child;
				xfldr.setAttribute("path", pf->GetFilename());
				xparent.appendChild(xfldr);
				dump_children(xfldr, child);
			}
			break;
		}
	}

}

void Project::add_compilable_children(QList<ProjectFile*>& res, ProjectItem* pi, bool include_copy_files)
{
	for (int i = 0; i < pi->GetChildren()->size(); i++) {
		ProjectItem* child = pi->GetChildren()->at(i);

		if (child->GetItemType() == ProjectItemType::TFile) {
			ProjectFile* pf = (ProjectFile*)child;
			if (pf->PropertyGetValue("build_action").toString() == BuildConsts::BUILD_ACTION_COMPILE ||
				(include_copy_files && pf->PropertyGetValue("build_action").toString() == BuildConsts::BUILD_ACTION_COPY)
				)
				res.append((ProjectFile*)child);
		}

		if (child->GetItemType() == ProjectItemType::TFolder) {
			ProjectFolder* pff = (ProjectFolder*)child;
			if (!pff->isVirtual())
				add_compilable_children(res, child);
		}
	}
}

void Project::add_copy_children(QList<ProjectFile*>& res, ProjectItem* pi)
{
	for (int i = 0; i < pi->GetChildren()->size(); i++) {
		ProjectItem* child = pi->GetChildren()->at(i);

		if (child->GetItemType() == ProjectItemType::TFile) {
			ProjectFile* pf = (ProjectFile*)child;
			if (pf->PropertyGetValue("build_action").toString() == BuildConsts::BUILD_ACTION_COPY)
				res.append((ProjectFile*)child);
		}

		if (child->GetItemType() == ProjectItemType::TFolder) {
			ProjectFolder* pff = (ProjectFolder*)child;
			if (!pff->isVirtual())
				add_copy_children(res, child);
		}
	}
}