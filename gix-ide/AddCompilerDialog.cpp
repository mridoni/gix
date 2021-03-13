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

#include "AddCompilerDialog.h"
#include "CustomDialog.h"
#include "UiUtils.h"
#include "PathUtils.h"
#include "GixGlobals.h"
#include "Ide.h"
#include "utils.h"
#include "IdeTaskManager.h"
#include "SysUtils.h"

#include <QtWidgets>
#include <QFrame>
#include <QSet>

static QStringList platform_ids({ "", "x86", "x64", "ARM" });

static QLabel *mk_label(QString t, int w, QWidget *p)
{
	QLabel *l = new QLabel(t, p); 
	l->setMaximumWidth(w);
	return l;
}

class PlatformTab : public QFrame
{
public:
	PlatformTab(int _platform_id, QString _bin_dir_path, QString _lib_dir_path, QString _include_dir_path, QString _config_dir_path, QString _copy_dir_path, QWidget *parent = nullptr) :QFrame(parent)
	{
		platform_id = _platform_id;
		bin_dir_path = _bin_dir_path;
		lib_dir_path = _lib_dir_path;
		include_dir_path = _include_dir_path;
		config_dir_path = _config_dir_path;
		copy_dir_path = _copy_dir_path;

		QGridLayout *qgl = new QGridLayout(this);
		
		qgl->addWidget(mk_label(tr("Platform ID"), 100, this), 0, 0);
		qgl->addWidget(new QLabel(platform_ids.at(platform_id), this), 0, 1);

		qgl->addWidget(mk_label(tr("Binary directory"), 100, this), 1, 0);
		qgl->addWidget(new QLabel(bin_dir_path, this), 1, 1);

		qgl->addWidget(mk_label(tr("Library directory"), 100, this), 2, 0);
		qgl->addWidget(new QLabel(lib_dir_path, this), 2, 1);

		qgl->addWidget(mk_label(tr("Include directory"), 100, this), 3, 0);
		qgl->addWidget(new QLabel(include_dir_path, this), 3, 1);

		qgl->addWidget(mk_label(tr("Config directory"), 100, this), 4, 0);
		qgl->addWidget(new QLabel(config_dir_path, this), 4, 1);

		qgl->addWidget(mk_label(tr("Copy directory"), 100, this), 5, 0);
		qgl->addWidget(new QLabel(copy_dir_path, this), 5, 1);

		this->setLayout(qgl);
	}

	QString bin_dir_path, lib_dir_path, include_dir_path, config_dir_path, copy_dir_path;
	int platform_id;
};

AddCompilerDialog::AddCompilerDialog(QWidget *parent) : QDialog(parent)
{
	setupUi(this);
	this->setAttribute(Qt::WA_DeleteOnClose);

	connect(bCancel, &QPushButton::clicked, this, [this] { close(); });
	connect(bAccept, &QPushButton::clicked, this, [this] { __accept(); });

	connect(btnAddPlatform, &QPushButton::clicked, this, [this] { addPlatform(); });
	connect(btnChooseBaseDir, &QPushButton::clicked, this, [this] { chooseBaseDir(); });

	if (!SysUtils::isWindows()) {
		chkIsVsBased->hide();
	}
}

AddCompilerDialog::~AddCompilerDialog()
{}


void AddCompilerDialog::show()
{
	QWidget::show();
}


void AddCompilerDialog::__accept()
{
	if (validateCompilerDefinition()) {
		if (saveCompilerDefinition()) {
			GixGlobals::getCompilerManager()->init();
			QDialog::accept();

			this->close();
		}
	}
}

bool AddCompilerDialog::validateCompilerDefinition()
{
	if (txtDescription->text().isEmpty() || txtBaseDir->text().isEmpty() || txtVersion->text().isEmpty()) {
		UiUtils::ErrorDialog(tr("Please fill in all  the parameters"));
		return false;
	}

	if (tabPlatforms->count() == 0) {
		UiUtils::ErrorDialog(tr("Please add at least one platform"));
		return false;
	}

	QString version = txtVersion->text();
	QRegularExpression rxVer(R"(^(\d+\.)?(\d+\.)?(\*|\d+)$)");
	if (!rxVer.match(version).hasMatch()) {
		UiUtils::ErrorDialog(tr("Please enter a valid version number"));
		return false;
	}

	QSet<QString> sel_platform_ids;
	for (int i = 0; i < tabPlatforms->count(); i++) {
		PlatformTab *p = (PlatformTab * )tabPlatforms->widget(i);
		if (sel_platform_ids.contains(platform_ids.at(p->platform_id))) {
			UiUtils::ErrorDialog(tr("You cannot have the same platform appear twice"));
			return false;
		}

		sel_platform_ids.insert(platform_ids.at(p->platform_id));
	}

	return true;
}

bool AddCompilerDialog::saveCompilerDefinition()
{
	QString defdir = GixGlobals::getCompilerDefsDir();
    if (defdir.isEmpty() || !QDir(defdir).exists()) {
        UiUtils::ErrorDialog(tr("Cannot find the compiler directory, please check your configuration"));
        return false;
    }

	QString subp = chkIsVsBased->isChecked() ? "vs" : "gcc";
	QStringList platforms;

	for (int i = 0; i < tabPlatforms->count(); i++) {
		PlatformTab *p = (PlatformTab *)tabPlatforms->widget(i);
		platforms.append(platform_ids.at(p->platform_id));
	}

	QString id = QString("gnucobol-%1-std-%2-%3").arg(txtVersion->text()).arg(subp).arg(platforms.join('_'));
	QString def_file = PathUtils::combine(defdir, id + ".def");
	if (QFile::exists(def_file)) {
		UiUtils::ErrorDialog(tr("There is already a definition for this compiler"));
		return false;
	}

	CompilerDefinition cd;
	cd.setDefinitionFile(def_file);
	cd.setId(id);
	cd.setName(txtDescription->text());
	cd.setVersion(txtVersion->text());
	cd.setHomedir(txtBaseDir->text());
	cd.setHostPlatform("x64");
	cd.setIsVsBased(chkIsVsBased->isChecked());


	for (int i = 0; i < tabPlatforms->count(); i++) {
		PlatformTab *p = (PlatformTab *)tabPlatforms->widget(i);
		CompilerPlatformDefinition *cpd = new CompilerPlatformDefinition();
		cpd->setBinDirPath(p->bin_dir_path);
		cpd->setLibDirPath(p->lib_dir_path);
		cpd->setIncludeDirPath(p->include_dir_path);
		cpd->setConfigDirPath(p->config_dir_path);
		cpd->setCopyDirPath(p->copy_dir_path);
		cd.addPlatform(platform_ids.at(p->platform_id), cpd);
	}

    if (!cd.save()) {
        UiUtils::ErrorDialog(tr("Cannot save this compiler definition"));
        return false;
    }

    return true;
}

void AddCompilerDialog::chooseBaseDir()
{
	QFileDialog dialog;
	dialog.setFileMode(QFileDialog::Directory);
	dialog.setOption(QFileDialog::ShowDirsOnly);
	if (dialog.exec()) {
		txtBaseDir->setText(dialog.selectedFiles()[0]);
	}
}

void AddCompilerDialog::addPlatform()
{
	std::string bin_dir_path, lib_dir_path, include_dir_path, config_dir_path, copy_dir_path;
	int platform_id = 0;

	if (txtBaseDir->text().trimmed().isEmpty()) {
		UiUtils::ErrorDialog(tr("Please select a home directory for the compiler before adding a platform"));
		return;
	}

    QString basedir = txtBaseDir->text().trimmed();

	CustomDialog *cdlg = new CustomDialog(tr("Add compiler platform"), this);
	
	cdlg->addLabel(tr("Platform ID"));
	cdlg->addComboBox("", "(" + tr("Unknown") + "|x86|x64|ARM", &platform_id);

	cdlg->addLabel(tr("Binary directory"));
    cdlg->addGetFolder("", &bin_dir_path, basedir.toStdString());
		
	cdlg->addLabel(tr("Library directory"));
    cdlg->addGetFolder("", &lib_dir_path, basedir.toStdString());
		
	cdlg->addLabel(tr("Include directory"));
    cdlg->addGetFolder("", &include_dir_path, basedir.toStdString());
		
	cdlg->addLabel(tr("Config directory"));
    cdlg->addGetFolder("", &config_dir_path, basedir.toStdString());
		
	cdlg->addLabel(tr("Copy directory"));
    cdlg->addGetFolder("", &copy_dir_path, basedir.toStdString());

	cdlg->setValidationCallback([this, &platform_id, &bin_dir_path, &lib_dir_path, &include_dir_path, &config_dir_path, &copy_dir_path](CustomDialog *cd) {
		
		std::string basedir = txtBaseDir->text().toStdString();

		if (!platform_id) {
			UiUtils::ErrorDialog(tr("Please select a platform"));
			return false;
		}

		if (bin_dir_path.empty() || !QDir(QString::fromStdString(bin_dir_path)).exists() || !starts_with(bin_dir_path, basedir)) {
			UiUtils::ErrorDialog(tr("Please select a valid binary directory"));
			return false;
		}

		if (lib_dir_path.empty() || !QDir(QString::fromStdString(lib_dir_path)).exists() || !starts_with(lib_dir_path, basedir)) {
			UiUtils::ErrorDialog(tr("Please select a valid library directory"));
			return false;
		}

		if (include_dir_path.empty() || !QDir(QString::fromStdString(include_dir_path)).exists() || !starts_with(include_dir_path, basedir)) {
			UiUtils::ErrorDialog(tr("Please select a valid include directory"));
			return false;
		}

		if (config_dir_path.empty() || !QDir(QString::fromStdString(config_dir_path)).exists() || !starts_with(config_dir_path, basedir)) {
			UiUtils::ErrorDialog(tr("Please select a valid config directory"));
			return false;
		}

		
		if (copy_dir_path.empty() || !QDir(QString::fromStdString(copy_dir_path)).exists() || !starts_with(copy_dir_path, basedir)) {
			UiUtils::ErrorDialog(tr("Please select a valid copy directory"));
			return false;
		}

		return true;
	});

	if (cdlg->exec()) {
		PlatformTab * pt = new PlatformTab(platform_id, QString::fromStdString(bin_dir_path), QString::fromStdString(lib_dir_path), 
											QString::fromStdString(include_dir_path), QString::fromStdString(config_dir_path), 
												QString::fromStdString(copy_dir_path), this);

		tabPlatforms->addTab(pt, platform_ids.at(platform_id));
	}


}
