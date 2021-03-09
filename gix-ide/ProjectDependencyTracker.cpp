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
