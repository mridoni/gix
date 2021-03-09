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


//#include <QString>


#include "NewProjectDialog.h"
#include "SysUtils.h"
#include "UiUtils.h"
#include "ProjectCollection.h"
#include "IdeTaskManager.h"
#include "Ide.h"

#include <QtWidgets>

static const char* DEFAULT_CONFIGURATION = "debug";
static const char* DEFAULT_PLATFORM = "x64";

NewProjectDialog::NewProjectDialog(ProjectCollection* ppj, QWidget* parent)
{
	prj_collection = ppj;

	setupUi(this);
	this->setAttribute(Qt::WA_DeleteOnClose);

	rbSingleBinary->setChecked(true);
	rbExe->setChecked(true);
	cbESQL->setChecked(true);

	grp_projectName->setVisible(prj_collection != nullptr);
	grp_ProjectFile->setVisible(prj_collection == nullptr);

	connect(bCancel, &QPushButton::clicked, this, [this] { close(); });
	connect(bAccept, &QPushButton::clicked, this, [this] { accept(); });

	connect(rbSingleBinary, &QRadioButton::toggled, this, &NewProjectDialog::projectTypeSelectionChanged);
	connect(rbMultiBinary, &QRadioButton::toggled, this, &NewProjectDialog::projectTypeSelectionChanged);
	connect(rbWebProject, &QRadioButton::toggled, this, &NewProjectDialog::projectTypeSelectionChanged);
	connect(rbNoProject, &QRadioButton::toggled, this, &NewProjectDialog::projectTypeSelectionChanged);

	connect(bChoosePrjFile, &QPushButton::clicked, this, [this] { choosePrjFile(); });

}

void NewProjectDialog::projectTypeSelectionChanged(bool b)
{
	rbDll->setEnabled(rbSingleBinary->isChecked() || rbMultiBinary->isChecked());
	rbExe->setEnabled(rbSingleBinary->isChecked() || rbMultiBinary->isChecked());
}

void NewProjectDialog::show()
{
	QWidget::show();
}

NewProjectDialog::~NewProjectDialog()
{
	//delete(tabs);
}

void NewProjectDialog::accept()
{
	QString err;
	QMap<QString, QVariant> opts;

	if (!prj_collection && prjFile->text().isEmpty()) {
		UiUtils::ErrorDialog(tr("Please enter a filename for this project collection"));
		return;
	}

	if (prj_collection && prjName->text().isEmpty()) {
		UiUtils::ErrorDialog(tr("Please enter a name for this project"));
		return;
	}

	ProjectType main_prj_type = ProjectType::NoProject;

	opts["preprocess_esql"] = cbESQL->isChecked();

	if (rbExe->isChecked())
		opts["build_type"] = "exe";
	else
		if (rbDll->isChecked())
			opts["build_type"] = "dll";

	if (rbSingleBinary->isChecked())
		main_prj_type = ProjectType::SingleBinary;
	else
		if (rbMultiBinary->isChecked())
			main_prj_type = ProjectType::MultipleBinaries;
		else
			if (rbWebProject->isChecked())
				main_prj_type = ProjectType::Web;
			else
				if (rbNoProject->isChecked())
					main_prj_type = ProjectType::NoProject;

	opts["output_path"] = "bin/${configuration}/${platform}";

	if (!prj_collection) {
		prj_collection = ProjectCollection::newProjectCollection(main_prj_type, prjFile->text(), opts);

		if (prj_collection && prj_collection->save()) {

			create_prj_file((Project *)prj_collection->GetChildren()->at(0), 0);

			if (Ide::TaskManager()->loadProjectCollection(prjFile->text()))
				hide();
		}
		else {
			UiUtils::ErrorDialog(tr("Cannot create project collection at ") + prjFile->text());
		}
	}
	else {
		QString prj_name = prjName->text();
		if (is_valid_prj_name(prj_name)) {
			Project* prj = Project::newProject(main_prj_type, ProjectFileType::Source, prj_name, opts);
			if (prj) {
				prj_collection->addProject(prj);

				if (prj_collection->save()) {
					create_prj_file(prj, 0);

					emit Ide::TaskManager()->projectAdded(prj);
				}
				else {
					prj_collection->GetChildren()->removeOne(prj);
					delete prj;
					UiUtils::ErrorDialog(tr("Cannot create project"));
				}
			}
		}
		else {
			UiUtils::ErrorDialog(tr("Invalid project name"));
		}
	}

	this->close();
}

bool  NewProjectDialog::create_prj_file(Project* prj, int idx)
{
	if (!prj || idx >= prj->GetChildren()->size())
		return false;

	ProjectFile * pf = (ProjectFile *) prj->GetChildren()->at(idx);
	QString filepath = pf->GetFileFullPath();
	QFile prj_file(filepath);
	if (prj_file.exists())
		return false;

	if (!pf->writeSourceTemplate(prj_file, ProjectFileType::Source)) {
		return false;
	}

	return true;
}

void NewProjectDialog::choosePrjFile()
{
	QString path = QStandardPaths::locate(QStandardPaths::DocumentsLocation, ".", QStandardPaths::LocateOption::LocateDirectory);
	path = PathUtils::combine(path, "gix");

	QFileDialog dialog;
	dialog.setAcceptMode(QFileDialog::AcceptSave);
	dialog.setOption(QFileDialog::Option::DontConfirmOverwrite, false);
	dialog.setFileMode(QFileDialog::AnyFile);
	dialog.setNameFilter(tr("GiX IDE Project Collection (*.gix)"));
	dialog.setDefaultSuffix("gix");
	dialog.setDirectory(path);

	if (prj_collection)
		dialog.setDirectory(prj_collection->GetBaseDir());

	if (dialog.exec()) {
		QString path = dialog.selectedFiles()[0];
		prjFile->setText(path);
	}

}

bool NewProjectDialog::is_valid_prj_name(QString p)
{
	QRegExp rxPrjName("^[a-zA-Z0-9\\-_]+$");
	return p.contains(rxPrjName);
}

