#include "ProjectDependencyTracker.h"
#include "linq/linq.hpp"

using namespace cpplinq;

ProjectDependencyTracker::ProjectDependencyTracker(Project *p)
{
	project = p;
	dependencies = new QList<ProjectFile *>();
}


ProjectDependencyTracker::~ProjectDependencyTracker()
{
	delete dependencies;
}

QList<ProjectFile*> *ProjectDependencyTracker::getDependencies()
{
	return dependencies;
}

void ProjectDependencyTracker::run()
{
	QStringList copy_dirs = project->getCopyDirList();
	QStringList copy_ext_list = project->getCopyExtList();

	QList<ProjectFile *> prj_files;
	project->add_compilable_children(prj_files, (ProjectItem *)project);
	QList<ProjectFile *> tdeps;
	for (ProjectFile * pf : prj_files) {
		QList<ProjectFile *> fdeps;
		pf->extractCopyDeps(fdeps, copy_dirs, copy_ext_list);
		if (fdeps.size() > 0) {
			tdeps.append(fdeps);
		}
	}

	dependencies->clear();

	bool exists = false;
	for (ProjectFile *ppf : tdeps) {

		if (from(*dependencies).where([ppf](ProjectFile *a) { return a->GetFileFullPath() == ppf->GetFileFullPath(); }).count() > 0) {
			delete ppf;
		}
		else {
			dependencies->append(ppf);
		}
	}

	emit finishedComputingDependencies();
}
