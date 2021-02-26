#include "ProjectCollection.h"

#include <QDomDocument>
#include <QFile>
#include <QDir>
#include <QTextStream>

#include "Project.h"
#include "GixGlobals.h"
#include "TargetManager.h"
#include "RsrcUtils.h"
#include "PathUtils.h"
#include "PrjCollPropertyDefinitionCollection.h"
#include "linq/linq.hpp"

using namespace cpplinq;

static PrjCollPropertyDefinitionCollection property_defs;

ProjectCollection::ProjectCollection() : ProjectItem()
{
	parent_item = nullptr;
}


ProjectCollection::~ProjectCollection()
{

}

void ProjectCollection::addProject(Project *prj)
{
	prj->SetParent(this);
	this->children->append(prj);
}

ProjectFile *ProjectCollection::locateProjectFileByPath(QString filepath, bool include_copy_files)
{
	auto prjs = from(*this->GetChildren()).where([](ProjectItem *a) { return a->GetItemType() == ProjectItemType::TProject;  }).to_vector();
	for (ProjectItem *pi : prjs) {
		Project *prj = dynamic_cast<Project *>(pi);
		ProjectFile *pf = prj->locateProjectFileByPath(filepath, include_copy_files);
		if (pf != nullptr)
			return pf;
	}
	return nullptr;
}

int ProjectCollection::getLoadableItemCount(QString filepath)
{
	int res = 0;

	QDomDocument doc;
	QFile prjcoll_file(filepath);
	if (!prjcoll_file.open(QIODevice::ReadOnly) || !doc.setContent(&prjcoll_file))
		return false;

	prjcoll_file.close();

	QDomNode projects_container = doc.elementsByTagName("projects").item(0);
	QDomNodeList projects = projects_container.childNodes();
	for (int i = 0; i < projects.size(); i++) {
		QDomNode xprj = projects.item(i);
		if (QString::compare(xprj.nodeName(), "project") != 0)
			continue;

		QDomElement xprj_path = xprj.firstChildElement("path");
		QString prj_path = xprj_path.text();
		prj_path = PathUtils::combine(QDir(filepath).dirName(), prj_path);
		res += Project::getLoadableItemCount(prj_path);
	}

	return res;
}

ProjectCollection *ProjectCollection::newProjectCollection(ProjectType add_main_project_type, QString ppj_filepath, const QVariantMap opts)
{
	ProjectCollection *ppj = new ProjectCollection();
	ppj->filepath = ppj_filepath;

	QFileInfo fn(ppj_filepath);
	QString pname = PathUtils::changeExtension(fn.baseName(), "");

	if (add_main_project_type != ProjectType::NoProject) {
		Project *prj = Project::newProject(add_main_project_type, ProjectFileType::Source, pname, opts);
		if (prj)
			ppj->addProject(prj);
	}

	for (auto pdef : property_defs.getAll()) {
		ppj->properties->insert(pdef->Name, pdef->DefaultValue);
	}

	ppj->runtime_properties["prjc.path"] = ppj->GetFileFullPath();
	ppj->runtime_properties["prjc.basedir"] = ppj->GetBaseDir();

	return ppj;
}

int ProjectCollection::getLoadableItemCount()
{
	int res = 0;
	auto prjs = from(*this->GetChildren()).where([](ProjectItem *a) { return a->GetItemType() == ProjectItemType::TProject;  }).to_vector();
	for (ProjectItem *pi : prjs) {
		Project *prj = dynamic_cast<Project *>(pi);
		res += prj->getLoadableItemCount();
	}
	return res;
}

ProjectItemType ProjectCollection::GetItemType()
{
	return ProjectItemType::TProjectCollection;
}

QString ProjectCollection::GetDisplayName()
{
	QString a = this->GetFilename();
	return this->GetFilename();
}

PropertyDefinitionCollection &ProjectCollection::PropertyGetDefinitions()
{
	return property_defs;
}

bool ProjectCollection::load(ProjectItem *owner, QString filepath)
{
	this->filepath = filepath;
	this->parent_item = nullptr;

	QDomDocument doc;
	QFile prjcoll_file(filepath);
	if (!prjcoll_file.open(QIODevice::ReadOnly) || !doc.setContent(&prjcoll_file))
		return false;

	prjcoll_file.close();

	QDomNode projects_container = doc.elementsByTagName("projects").item(0);
	QDomNodeList projects = projects_container.childNodes();
	for (int i = 0; i < projects.size(); i++) {
		QDomNode xprj = projects.item(i);
		if (QString::compare(xprj.nodeName(), "project") != 0)
			continue;

		QDomElement xprj_path = xprj.firstChildElement("path");
		QString prj_path = xprj_path.text();

		Project *prj = Project::loadProject(this, prj_path);
		if (!prj)
			return false;

		this->GetChildren()->append(prj);
	}

	this->runtime_properties["prjc.path"] = this->GetFileFullPath();
	this->runtime_properties["prjc.basedir"] = this->GetBaseDir();

	return true;
}

bool ProjectCollection::save(ProjectItem *owner, QString filepath)
{
	QDomDocument doc;
	QFile prjcoll_file(filepath);

	QDomElement xprj = doc.createElement("gix-projectcollection");
	doc.appendChild(xprj);
	xprj.setAttribute("version", "1.0");

	QDomElement items_container = doc.createElement("projects");
	xprj.appendChild(items_container);
	this->dump_children(items_container, this);

	//// Collection settings
	//QDomElement settings_container = doc.createElement("settings");
	//xprj.appendChild(settings_container);
	//QMap<QString, QVariant>::iterator si;

	//for (si = properties->begin(); si != properties->end(); ++si) {
	//	QString pkey = si.key();
	//	PropertyDefinition *pd = property_defs.getPropertyDefinition(pkey);
	//	QDomElement xs = doc.createElement(si.key());
	//	xs.appendChild(doc.createTextNode(pd->serialize(si.value())));
	//	settings_container.appendChild(xs);
	//}

	QFile file(GetFileFullPath());

	if (file.exists())
		file.copy(GetFileFullPath() + ".bak");

	if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
		return false;
	}

	QTextStream stream(&file);
	stream << doc.toString();
	file.close();

	auto prjs = from(*this->GetChildren()).where([](ProjectItem *a) { return a->GetItemType() == ProjectItemType::TProject;  }).to_vector();
	for (ProjectItem *pi : prjs) {
		Project *prj = dynamic_cast<Project *>(pi);
		if (prj->isDirty() && !prj->isVirtual()) {
			if (!prj->save(this, prj->GetFileRelativePath())) {
				return false;
			}
		}
	}

	return true;
}

//bool ProjectCollection::save()
//{
//	return save(this->parent_item, this->filepath);
//}

bool ProjectCollection::revert()
{
	return false;
}

bool ProjectCollection::isDirty()
{
	return false;
}

void ProjectCollection::setDirty(bool b)
{}

void ProjectCollection::dump_children(QDomNode &xparent, ProjectItem *pi)
{
	for (int i = 0; i < pi->GetChildren()->size(); i++) {
		ProjectItem *child = pi->GetChildren()->at(i);

		QDomElement xprj = xparent.ownerDocument().createElement("project");
		Project *prj = (Project *)child;

		QDomElement xprj_path = xparent.ownerDocument().createElement("path");
		xprj.appendChild(xprj_path);
		xprj_path.appendChild(xparent.ownerDocument().createTextNode(prj->GetFileRelativePath()));

		//QList<QString> keys = prj->PropertyGetCurrentValues()->keys();
		//for (int n = 0; n < keys.size(); n++) {
		//	QDomElement xprop = xparent.ownerDocument().createElement(keys.at(n));
		//	PropertyDefinition *pd = prj->PropertyGetDefinitions().getPropertyDefinition(keys.at(n));
		//	xprop.appendChild(xparent.ownerDocument().createTextNode(prj->PropertyGetValue(keys.at(n), pd->DefaultValue).toString()));
		//	xprj.appendChild(xprop);
		//}

		xparent.appendChild(xprj);
	}
}

BuildTarget *ProjectCollection::getBuildTarget(QMap<QString, QVariant> env, BuildTarget *parent)
{
	MacroManager mm;
	// TODO : add something?
	return GixGlobals::getTargetManager()->composeBuildTarget(parent, "collection", &env, ((IBuildableItem *)this));
}

QList<IBuildableItem *> ProjectCollection::getDependencies(const QString &use, QVariantMap *props, bool *yield_ownership)
{
	QList<IBuildableItem *> res;
	ProjectType pt = ProjectType::NoProject;
	QString build_type;

	if (use != "*") {
		QString p1 = use.mid(0, use.indexOf("/"));
		build_type = use.mid(use.indexOf("/") + 1);

		if (p1 == "singleartifact")
			pt = ProjectType::SingleBinary;
		else
			if (p1 == "multiartifact")
				pt = ProjectType::MultipleBinaries;
			else
				if (p1 == "webartifact")
					pt = ProjectType::Web;
	}

	for (int i = 0; i < this->GetChildren()->size(); i++) {
		Project *prj = (Project *)this->GetChildren()->at(i);

		if (use != "*") {
			if (prj->getType() != pt || (!build_type.isEmpty() && prj->PropertyGetValue("build_type") != build_type))
				continue;
		}

		res.append(static_cast<IBuildableItem *>(prj));
	}

	return res;
}
