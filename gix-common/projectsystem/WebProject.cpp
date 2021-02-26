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
