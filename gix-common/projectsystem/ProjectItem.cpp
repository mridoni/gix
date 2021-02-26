#include "ProjectItem.h"
#include "PathUtils.h"
#include "PropertyDefinition.h"
#include "PropertySource.h"

ProjectItem::ProjectItem()
{
	parent_item = nullptr;
	children = new QList<ProjectItem *>();
	is_virtual = false;
	guid = QUuid::createUuid();
}


ProjectItem::~ProjectItem()
{
	for (ProjectItem *c : *children) {
		delete c;
	}

	delete children;
}

QString ProjectItem::GetFileRelativePath()
{
	return filepath;
}

QString ProjectItem::GetFileFullPath()
{
	QString res = filepath;
	ProjectItem *cur = this;
	while (cur->parent_item != nullptr) {
		QString pfp = cur->parent_item->filepath;
		res = PathUtils::combine(PathUtils::getDirectory(cur->parent_item->filepath), res);
		cur = cur->parent_item;
	}
	return QDir::cleanPath(res);
}

QString ProjectItem::GetFilename()
{
	return PathUtils::getFilename(filepath);
}

QString ProjectItem::GetBaseDir()
{
	QString fullpath = GetFileFullPath();
	return PathUtils::getDirectory(fullpath);
}

bool ProjectItem::HasChildren()
{
	return children->size() > 0;
}

bool ProjectItem::isVirtual()
{
	return is_virtual;
}

void ProjectItem::setVirtual(bool f)
{
	is_virtual = f;
}

QList<ProjectItem*> *ProjectItem::GetChildren()
{
	return children;
}

ProjectItem * ProjectItem::GetParent()
{
	return parent_item;
}

void ProjectItem::SetParent(ProjectItem *p)
{
	parent_item = p;
}

int ProjectItem::getIndex()
{
	if (parent_item != nullptr) {
		return parent_item->children->indexOf((ProjectItem*) this);
	}
	return 0;
}

int ProjectItem::columnCount() const
{
	return 1;
}

QString ProjectItem::getGuid()
{
	return guid.toString();
}

bool ProjectItem::isPropertyVisible(PropertyDefinition *pd)
{
	if (!pd->show_depending_on)
		return true;

	return pd->show_depending_on(dynamic_cast<PropertySource *>(this));
}

