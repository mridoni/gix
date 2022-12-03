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

#include "ProjectFile.h"
#include "PathUtils.h"
#include "SysUtils.h"
#include "RsrcUtils.h"
#include "BuildConsts.h"
#include "PropertyConsts.h"
#include "IGixLogManager.h"
#include "GixGlobals.h"
#include "ProjectFilePropertyDefinitionCollection.h"
#include "CompilerConfiguration.h"
#include "SourceFileFormat.h"

#include <QFileInfo>
#include <QFile>
#include <QRegularExpression>

static ProjectFilePropertyDefinitionCollection property_defs;

ProjectFile::ProjectFile() : ProjectItem()
{
	is_virtual = false;
}


ProjectFile::~ProjectFile()
{

}

void ProjectFile::extractCopyDeps(QList<ProjectFile *> &res, const QStringList &copy_dirs, const QStringList &copy_ext_list)
{
	add_copy_deps(copy_dirs, copy_ext_list, this, res);
}

Project *ProjectFile::getParentProject()
{
	ProjectItem *parent = this->GetParent();
	while (parent != nullptr) {
		Project *prj = dynamic_cast<Project *>(parent);
		if (prj)
			return prj;

		parent = parent->GetParent();
	}

	return nullptr;
}

bool ProjectFile::getOutputModuleAndFile(QString configuration, QString platfom, QString &module_name, QString &filepath)
{
	//if (this->PropertyGetValue("build_action") != "compile")
	//	return false;

	//Project *prj = this->getParentProject();
	//QDir bdir(prj->getBuildDirectory(configuration, platfom));
	//QMap<QString, QVariant> opts;
	//opts["configuration"] = configuration;
	//opts["platform"] = platfom;
	//BuildTarget *bt;

	//if (prj->getType() != ProjectType::MultipleBinaries) {
	//	bt = prj->getBuilder(nullptr)->getBuildTarget(opts, nullptr);
	//	filepath = bt->filename();
	//	filepath = resolve_module_reference(filepath);
	//	module_name = PathUtils::toModuleName(filepath);
	//}
	//else {
	//	// This is just a convenience, we don't need 
	//	// the full execute target chain, so we pass null as parent
	//	bt = this->getBuildTarget(opts, nullptr);
	//	if (bt == nullptr)
	//		return false;

	//	filepath = bt->filename();
	//	if (QDir(filepath).isRelative())
	//		filepath = PathUtils::combine(bdir.absolutePath(), filepath);

	//	filepath = resolve_module_reference(filepath);
	//	module_name = PathUtils::toModuleName(filepath);
	//	delete bt;
	//}
	return true;
}

bool ProjectFile::isStartupItem()
{
	return(*properties)[PropertyConsts::IsStartupItem].toBool();
}

bool ProjectFile::isHttpService()
{
	return isRestService() || isSoapService();
}

bool ProjectFile::isRestService()
{
	return this->getSubProperty("is_rest_ws", "enabled").toBool();
}

bool ProjectFile::isSoapService()
{
	return this->getSubProperty("is_soap_ws", "enabled").toBool();
}

bool ProjectFile::isModule()
{
	return this->PropertyGetValue("build_action").toString() == BuildConsts::BUILD_ACTION_COMPILE;
}

SourceFileFormat ProjectFile::getSourceFormat()
{
	QSettings settings;

	SourceFileFormat fmt = (SourceFileFormat) settings.value("cobc_default_source_format", (int)SourceFileFormat::Fixed).toInt();
	if (PropertyExists("cobc_source_format")) {
		fmt = (SourceFileFormat)PropertyGetValue("cobc_default_source_format", (int)SourceFileFormat::Fixed).toInt();
	}
	return fmt;
}

void ProjectFile::add_copy_deps(const QStringList &copy_dirs, const QStringList &copy_ext_list, ProjectFile *pf, QList<ProjectFile *> &res)
{
	IGixLogManager *logger = GixGlobals::getLogManager();
	QRegularExpression rxCopy("COPY ([A-Za-z0-9_]+)\\ *\\.");
	if (!QFile::exists(this->GetFileFullPath())) {
		logger->error(LOG_METADATA, "File not found: {}", GetFileFullPath().toStdString());
		return;
	}

	QFile f(this->GetFileFullPath());
	if (!f.open(QIODevice::OpenModeFlag::ReadOnly)) {
		logger->error(LOG_METADATA, "Cannot open file: {}", GetFileFullPath());
		return;
	}

	while (true) {
		QByteArray a = f.readLine();
		if (!a.size())
			break;

		QString line = QString(a);
		if (line.length() <= 6)
			continue;

		line = line.mid(6);
		if (line.startsWith("*") || line.trimmed() == "")
			continue;

		auto m = rxCopy.match(line);
		if (!m.hasMatch())
			continue;

		QString cpy_name = m.captured(1);
		if (cpy_name.trimmed().isEmpty())
			continue;

		QString cpy_file = locate_copy_file(cpy_name, copy_dirs, copy_ext_list);
		if (cpy_file.isEmpty()) {
			logger->error(LOG_METADATA, "Cannot locate copy: {}", cpy_name);
			cpy_file = cpy_name;
		}

		ProjectFile *cpy = ProjectFile::fromCopyFile(cpy_name, cpy_file);
		res.append(cpy);
	}
	f.readLine();

	f.close();
}

QString ProjectFile::locate_copy_file(QString cpy_name, const QStringList &copy_dirs, const QStringList &copy_ext_list)
{
	for (QString copy_dir : copy_dirs) {
		QString cpath = PathUtils::combine(copy_dir, cpy_name);
		for (QString ext : copy_ext_list) {
			if (ext == ".") {
				ext = "";
			}

			QString full_path = PathUtils::changeExtension(cpath, ext);
			if (QFile::exists(full_path))
				return full_path;
		}
	}
	return QString();
}

bool ProjectFile::writeSourceTemplate(Project * prj, QFile &f, ProjectFileType t)
{
	QString template_name;
	switch (t) {
		case ProjectFileType::Source:
			template_name = (prj != nullptr && prj->getType() == ProjectType::Web) ? ":/templates/PROGRAM_WSVC.cbl" : ":/templates/PROGRAM.cbl";
			break;

		case ProjectFileType::Copy:
			template_name = (prj != nullptr && prj->getType() == ProjectType::Web) ? ":/templates/COPY_WSVC.cpy" : ":/templates/COPY.cpy";
			break;
	}
	

	QFileInfo fi(f);
	QString basename = fi.baseName();
	QString sTemplate = RsrcUtils::getAsString(template_name);

	sTemplate = sTemplate.replace("${PGID}", basename.toUpper());
	auto data = sTemplate.toUtf8();
	if (!f.open(QIODevice::OpenModeFlag::WriteOnly))
		return false;

	uint64_t bw = f.write(data) >= 0;
	f.close();

	return (bw > 0);
}

QString ProjectFile::extractModuleName()
{
	QString src_file = this->GetFileFullPath();

	QRegularExpression rxProgramId("PROGRAM\\-ID\\.\\ [\"']?+([A-Za-z0-9_\\-]+)[\"']?\\ *\\.");
	if (QFile::exists(src_file)) {

		QString src = SysUtils::FileReadAllText(src_file);
		QRegularExpressionMatch m = rxProgramId.match(src);
		if (m.hasMatch()) {
		        QString module_name = m.captured(1).trimmed();
			return module_name;
		}
	}
	return QString();
}

QString ProjectFile::getSymbolFilename(QString configuration, QString platform)
{
	QString build_action = PropertyGetValue("build_action", QString()).toString();
	if (build_action != "compile")
		return QString();

	Project *prj = getParentProject();
	if (!prj)
		return QString();

	if (configuration.isEmpty())
		configuration = GixGlobals::getCurrentConfiguration();

	if (platform.isEmpty())
		platform = GixGlobals::getCurrentPlatform();

	QString build_dir = prj->getBuildDirectory(configuration, platform);
	QString sym_file = PathUtils::changeExtension(this->GetFilename(), ".sym");

	return PathUtils::combine(build_dir, sym_file);
}

QString ProjectFile::resolve_module_reference(QString fp)
{
	if (!fp.contains("${module_name}"))
		return fp;

	QString module_name = extractModuleName();
	return module_name.isEmpty() ? fp : fp.replace("${module_name}", module_name);
}

ProjectFile *ProjectFile::fromCopyFile(QString cpy_name, QString cpy_file)
{
	ProjectFile *pf = new ProjectFile();
	pf->setVirtual(false);
	pf->filepath = cpy_file;
	pf->PropertySetValue("build_action", "copy");
	return pf;
}


ProjectItemType ProjectFile::GetItemType()
{
	return ProjectItemType::TFile;
}

QString ProjectFile::GetDisplayName()
{
	return this->GetFilename();
}

PropertyDefinitionCollection &ProjectFile::PropertyGetDefinitions()
{
	return property_defs;
}


bool ProjectFile::load(ProjectItem *owner, QString filepath)
{
	QFile prj_file(PathUtils::combine(owner->GetBaseDir(), filepath));
	if (!prj_file.exists())
		return false;

	this->filepath = filepath;
	this->parent_item = owner;

	//customPropertyFunc fcn = std::bind(&ProjectFile::GetFileFullPath, this);
	//this->PropertySetValue("filepath", QVariant::fromValue<customPropertyFunc>(fcn));
	this->PropertySetValue("filepath", this->GetFileFullPath());

	this->runtime_properties["prjfile.path"] = this->GetFileFullPath();
	this->runtime_properties["prjfile.path.noext"] = PathUtils::changeExtension(this->GetFileFullPath(), "");
	this->runtime_properties["prjfile.name.noext"] = PathUtils::changeExtension(this->GetFilename(), "");
	this->runtime_properties["prjfile.basedir"] = this->GetBaseDir();

	return true;
}

ProjectFile *ProjectFile::newProjectFile(ProjectItem *owner, QString filepath, bool type_from_ext)
{
	QString parent_path = "";

	switch (owner->GetItemType()) {
		case ProjectItemType::TProject:
			parent_path = owner->GetBaseDir();
			break;

		case ProjectItemType::TFolder:
			parent_path = owner->GetFileFullPath();
			break;

		default:
			return NULL;
	}

	ProjectFile *pf = new ProjectFile();
	pf->parent_item = owner;

	QFile prj_file(PathUtils::combine(parent_path, filepath));
	if (prj_file.exists())
		return nullptr;

	if (!QDir::isAbsolutePath(filepath))
		pf->filepath = filepath;
	else
		pf->filepath = PathUtils::rebasePath(filepath, parent_path);

	QFileInfo fi(filepath);

	if (type_from_ext) {
		QString ext = fi.suffix().toLower();
		if (ext == "cbl" || ext == "cob")
			pf->PropertySetValue("build_action", "compile");
		else
			if (ext == "cpy")
				pf->PropertySetValue("build_action", "copy");
			else
				pf->PropertySetValue("build_action", "none");
	}

	pf->PropertySetValue("filepath", pf->GetFileFullPath());

	pf->runtime_properties["prjfile.path"] = pf->GetFileFullPath();
	pf->runtime_properties["prjfile.path.noext"] = PathUtils::changeExtension(pf->GetFileFullPath(), "");
	pf->runtime_properties["prjfile.name.noext"] = PathUtils::changeExtension(pf->GetFilename(), "");
	pf->runtime_properties["prjfile.basedir"] = pf->GetBaseDir();

	return pf;
}

bool ProjectFile::save(ProjectItem *owner, QString filepath)
{
	return true;
}

bool ProjectFile::revert()
{
	return false;
}

bool ProjectFile::isDirty()
{
	return false;
}

void ProjectFile::setDirty(bool b)
{

}

BuildTarget *ProjectFile::getBuildTarget(QMap<QString, QVariant> env, BuildTarget *parent)
{
	return nullptr;

}

QList<IBuildableItem *> ProjectFile::getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership)
{
	QList<IBuildableItem *> res;
	res.append(this);
	return res;
}

ProjectFile *ProjectFile::clone()
{
	ProjectFile *pf = new ProjectFile();
	pf->is_virtual = is_virtual;

	// from ProjectItem
	pf->parent_item = parent_item;
	pf->filepath = filepath;
	
	// from PropertySource
	for (QString k : properties->keys())
		pf->properties->insert(k, properties->value(k));

	pf->runtime_properties = runtime_properties;

	return pf;
}

QList<BuildTarget *> ProjectFile::getCobolCopyFiles()
{
	return QList<BuildTarget *>();
}

bool ProjectFile::isCopy()
{
	return this->PropertyGetValue("build_action").toString() == BuildConsts::BUILD_ACTION_COPY;
}
