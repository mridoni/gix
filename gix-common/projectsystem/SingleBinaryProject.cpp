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
