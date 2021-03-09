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

#include "ProjectCollectionWindow.h"

#include <QTreeWidget>
#include <QTreeWidgetItem>
#include <QIcon>
#include <QMenu>
#include <QInputDialog>
#include <QMessageBox>
#include <QFileDialog>
#include <QProgressDialog>
#include <qmdisubwindow.h>

#include "ProjectCollection.h"
#include "Project.h"
#include "ProjectItem.h"
#include "ProjectFile.h"
#include "ProjectFolder.h"
#include "PathUtils.h"
#include "UiUtils.h"
#include "CustomDialog.h"
#include "Ide.h"
#include "IdeTaskManager.h"
#include "NewProjectDialog.h"
#include "linq/linq.hpp"
#include "PropertyConsts.h"

ProjectCollectionWindow::ProjectCollectionWindow(QWidget *parent, MainWindow *mw) : QMainWindow(parent)
{
	this->setMinimumWidth(300);
	this->setWindowFlags(Qt::Widget); // <---------
	QToolBar* toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;
	this->treeview = nullptr;

	connect(Ide::TaskManager(), &IdeTaskManager::projectCollectionClosed, this, [this]() {
		setContent(nullptr, true);
	});

	connect(Ide::TaskManager(), &IdeTaskManager::projectAdded, this, [this](Project *prj) {
		refreshContent();
	});
}


ProjectCollectionWindow::~ProjectCollectionWindow()
{
}

void ProjectCollectionWindow::refreshContent()
{
	ProjectCollection * ppj = Ide::TaskManager()->getCurrentProjectCollection();
	if (!ppj)
		return;

	setContent(ppj, true);
}

void ProjectCollectionWindow::setContent(ProjectCollection* prjc, bool refresh_only)
{
	if (prjc == nullptr) {
		treeview->clear();
		return;
	}

	Ide::TaskManager()->setBackgroundTasksEnabled(false);

	if (!refresh_only) {
		treeview = new QTreeWidget();
		treeview->setColumnCount(1);
		treeview->setSelectionMode(QAbstractItemView::SelectionMode::SingleSelection);
		treeview->setMinimumHeight(130);

		UiUtils::setTreeViewFont(treeview);
	}
	else
		treeview->clear();

	QTreeWidgetItem* rootItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(prjc->GetDisplayName()));
	rootItem->setIcon(0, QIcon(":/icons/prjcollection.png"));
	rootItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<void*>(prjc)));

	QProgressDialog progress(tr("Loading project"), "", 0, prjc->GetChildren()->size(), this);
	progress.setCancelButton(0);
	progress.setWindowModality(Qt::WindowModal);
	progress.setMinimumDuration(0);

	int nitems = prjc->getLoadableItemCount();
	progress.setMinimum(0);
	progress.setMaximum(nitems);

	for (int i = 0; i < prjc->GetChildren()->size(); i++) {
		Project* prj = (Project*)prjc->GetChildren()->at(i);

		progress.setValue(i);
		QCoreApplication::processEvents();

		QTreeWidgetItem* prjTreeWidgetItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(prj->GetDisplayName()));
		prjTreeWidgetItem->setIcon(0, QIcon(":/icons/project.png"));
		prjTreeWidgetItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<void*>(prj)));
		rootItem->addChild(prjTreeWidgetItem);

		// Dependencies are now tracked in a separate window

		//ProjectFolder *fdeps = new ProjectFolder(tr("Dependencies"), true);
		//fdeps->SetParent(prj);
		//fdeps->PropertySetValue("name", tr("Dependencies"));
		//fdeps->PropertySetValue("is_virtual", true);
		//QTreeWidgetItem *copyDepsTreeWidgetItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(tr("Dependencies")));
		//copyDepsTreeWidgetItem->setIcon(0, QIcon(":/icons/vfolder.png"));
		//copyDepsTreeWidgetItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<void*>(fdeps)));
		//prjTreeWidgetItem->addChild(copyDepsTreeWidgetItem);
		//add_copy_deps_children(prj, copyDepsTreeWidgetItem);

		add_children(prj, prjTreeWidgetItem, progress);
	}
	treeview->setContextMenuPolicy(Qt::CustomContextMenu);

	if (!refresh_only) {
		connect(treeview, &QTreeWidget::itemClicked, this, &ProjectCollectionWindow::myItemClicked);
		connect(treeview, &QTreeWidget::itemDoubleClicked, this, &ProjectCollectionWindow::myItemDoubleClicked);
		connect(treeview, &QTreeWidget::customContextMenuRequested, this, &ProjectCollectionWindow::prepareMenu);
	}
	treeview->setRootIsDecorated(true);
	treeview->setHeaderHidden(true);
	treeview->insertTopLevelItem(0, rootItem);
	this->setCentralWidget(treeview);
	treeview->expandAll();

	Ide::TaskManager()->setBackgroundTasksEnabled(true);
}


void ProjectCollectionWindow::setContent(ProjectCollection * prjc)
{
	setContent(prjc, false);
}

ProjectItem * ProjectCollectionWindow::getCurrentSelection()
{
	QList<QTreeWidgetItem *> sel_items = treeview->selectedItems();
	if (sel_items.size() == 0)
		return nullptr;

	QTreeWidgetItem *item = sel_items[0];
	QVariant q = item->data(0, Qt::UserRole);
	return static_cast<ProjectItem *>(q.value<void*>());
}

void ProjectCollectionWindow::add_children(ProjectItem *parent, QTreeWidgetItem *parent_widget, QProgressDialog& progress)
{
	for (int n = 0; n < parent->GetChildren()->size(); n++) {
		ProjectItem *child = parent->GetChildren()->at(n);

		if (child->GetItemType() == ProjectItemType::TFile) {
			ProjectFile *file = (ProjectFile *)child;
			QTreeWidgetItem *fileItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(file->GetDisplayName()));
			fileItem->setIcon(0, QIcon(":/icons/cblfile.png"));
			fileItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<void*>(file)));

			if (file->isStartupItem()) {
				setFontBold(fileItem, true);
			}

			parent_widget->addChild(fileItem);
			progress.setValue(progress.value() + 1);

			// Dependencies are now shown and updated in a separate window
		}

		if (child->GetItemType() == ProjectItemType::TFolder) {
			ProjectFolder *folder = (ProjectFolder *)child;
			QTreeWidgetItem *folderItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(folder->GetDisplayName()));
			folderItem->setIcon(0, QIcon(":/icons/folder.png"));
			folderItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<void*>(folder)));
			parent_widget->addChild(folderItem);
			add_children(folder, folderItem, progress);
		}
	}
}

void ProjectCollectionWindow::setFontBold(QTreeWidgetItem* fileItem, bool b)
{
	auto item_font = fileItem->font(0);
	item_font.setBold(b);
	fileItem->setFont(0, item_font);
}

void ProjectCollectionWindow::add_copy_deps_children(Project * prj, QTreeWidgetItem * parent_widget)
{
	//ProjectDependencyTracker *pdt = new ProjectDependencyTracker(prj);
	//connect(pdt, &ProjectDependencyTracker::finishedComputingDependencies, this, [pdt, parent_widget] {
	//	QList<ProjectFile *> deps = *pdt->getDependencies();
	//	for (ProjectFile *file : deps) {
	//		QTreeWidgetItem *fileItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(file->GetDisplayName()));
	//		fileItem->setIcon(0, QIcon(":/icons/cblfile.png"));
	//		fileItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<void*>(file)));
	//		parent_widget->addChild(fileItem);
	//	}
	//	parent_widget->setExpanded(false);
	//	delete(pdt);
	//});
	//pdt->start();
}

void ProjectCollectionWindow::eraseItemRecursively(ProjectItem * pi)
{
	for (ProjectItem *child : *pi->GetChildren()) {
		eraseItemRecursively(child);
	}
	delete(pi);
}

void ProjectCollectionWindow::checkUiAndSaveStatusBeforeDelete(ProjectItem * pi)
{
	QList<ProjectFile *> files;

	if (pi->GetItemType() == ProjectItemType::TFile) {
		files.append(static_cast<ProjectFile *>(pi));
	}

	if (pi->GetItemType() == ProjectItemType::TFolder) {
		static_cast<ProjectFolder *>(pi)->getFilesRecursively(files);
	}

	for (ProjectFile *pf : files) {
		auto f = mainWindow->findMdiChild(pf->GetFileFullPath());
		if (f) {
			f->close();
		}
	}

}

void ProjectCollectionWindow::myItemClicked(QTreeWidgetItem *item, int column)
{
	QVariant q = item->data(0, Qt::UserRole);
	ProjectItem * p = static_cast<ProjectItem *>(q.value<void*>());
	PropertySource *ips = (PropertySource *)p;

	emit selectionChanged(p);

}

void ProjectCollectionWindow::myItemDoubleClicked(QTreeWidgetItem *item, int column)
{
	QVariant q = item->data(0, Qt::UserRole);
	ProjectItem * p = static_cast<ProjectItem *>(q.value<void*>());

	switch (p->GetItemType()) {
		case ProjectItemType::TFile:
			mainWindow->openFile(p->GetFileFullPath());
			break;
	}
}

void ProjectCollectionWindow::prepareMenu(const QPoint & pos)
{
	QMenu *menu = nullptr;
	QTreeWidget *tree = treeview;
	QTreeWidgetItem *item = tree->itemAt(pos);
	QVariant q = item->data(0, Qt::UserRole);
	ProjectItem * p = static_cast<ProjectItem *>(q.value<void*>());

	switch (p->GetItemType()) {
		case ProjectItemType::TProjectCollection:
			menu = PrepareProjectCollectionMenu(item, (ProjectCollection*)p);
			break;

		case ProjectItemType::TProject:
			menu = PrepareProjectMenu(item, (Project *)p);
			break;

		case ProjectItemType::TFolder:
			menu = PrepareProjectFolderMenu(item, (ProjectFolder *)p);
			break;

		case ProjectItemType::TFile:
			menu = PrepareProjectFileMenu(item, (ProjectFile *)p);
			break;
	}

	if (menu != nullptr) {
		QPoint pt(pos);
		menu->exec(tree->mapToGlobal(pos));
	}
}

QMenu * ProjectCollectionWindow::PrepareProjectCollectionMenu(QTreeWidgetItem *item, ProjectCollection * p)
{
	if (!Ide::TaskManager()->getCurrentProjectCollection())
		return nullptr;

	QMenu* menu = new QMenu(this);

	QAction* addNewProjectAct = new QAction(QIcon(":/icons/folder_add.png"), tr("&Add new project..."), this);
	addNewProjectAct->setStatusTip(tr("Add project"));
	connect(addNewProjectAct, &QAction::triggered, this, [this, p, item] { 
		NewProjectDialog* pdlg = new NewProjectDialog(Ide::TaskManager()->getCurrentProjectCollection(), this);
		pdlg->show();
	});
	menu->addAction(addNewProjectAct);

	return menu;
}

QMenu *ProjectCollectionWindow::PrepareProjectMenu(QTreeWidgetItem *item, Project * p)
{	
	QMenu *menu = new QMenu(this);
	
	QAction *addNewFolderAct = new QAction(QIcon(":/icons/folder_add.png"), tr("&Add new folder..."), this);
	addNewFolderAct->setStatusTip(tr("Add folder"));
	connect(addNewFolderAct, &QAction::triggered, this, [this,p,item] { addProjectNewFolder(item, p); });
	menu->addAction(addNewFolderAct);

	menu->addSeparator();

	QAction *addNewSourceFileAct = new QAction(QIcon(":/icons/file_add.png"), tr("&Add new source file..."), this);
	addNewSourceFileAct->setStatusTip(tr("Add new source file"));
	connect(addNewSourceFileAct, &QAction::triggered, this, [this, p, item] { addProjectNewSourceFile(item, p); });
	menu->addAction(addNewSourceFileAct);

	QAction *addExistingSourceFileAct = new QAction(QIcon(":/icons/file_add2.png"), tr("&Add existing source file(s)..."), this);
	addExistingSourceFileAct->setStatusTip(tr("Add existing source file(s)"));
	connect(addExistingSourceFileAct, &QAction::triggered, this, [this, p, item] { addProjectExistingSourceFile(item, p); });
	menu->addAction(addExistingSourceFileAct);

	return menu;
}

QMenu * ProjectCollectionWindow::PrepareProjectFileMenu(QTreeWidgetItem *item, ProjectFile * p)
{
	QMenu *menu = new QMenu(this);

	QAction *deleteItemAct = new QAction(QIcon(":/icons/bullet_cross.png"), tr("Remove or delete"), this);
	deleteItemAct->setStatusTip(tr("Remove or delete"));
	connect(deleteItemAct, &QAction::triggered, this, [this, p, item] { deleteOrRemoveItemFromProject(item, p); });
	menu->addAction(deleteItemAct);

	return menu;
}

QMenu * ProjectCollectionWindow::PrepareProjectFolderMenu(QTreeWidgetItem *item, ProjectFolder * p)
{
	QMenu *menu = new QMenu(this);

	QAction *addNewFolderAct = new QAction(QIcon(":/icons/folder_add.png"), tr("&Add new folder..."), this);
	addNewFolderAct->setStatusTip(tr("Add folder"));
	connect(addNewFolderAct, &QAction::triggered, this, [this, p, item] { addProjectNewFolder(item, p); });
	menu->addAction(addNewFolderAct);

	menu->addSeparator();

	QAction *addNewSourceFileAct = new QAction(QIcon(":/icons/file_add.png"), tr("&Add new source file..."), this);
	addNewSourceFileAct->setStatusTip(tr("Add new source file"));
	connect(addNewSourceFileAct, &QAction::triggered, this, [this, p, item] { addProjectNewSourceFile(item, p); });
	menu->addAction(addNewSourceFileAct);

	QAction *addExistingSourceFileAct = new QAction(QIcon(":/icons/file_add2.png"), tr("&Add existing source file(s)..."), this);
	addExistingSourceFileAct->setStatusTip(tr("Add existing source file(s)"));
	connect(addExistingSourceFileAct, &QAction::triggered, this, [this, p, item] { addProjectExistingSourceFile(item, p); });
	menu->addAction(addExistingSourceFileAct);

	menu->addSeparator();

	QAction *deleteItemAct = new QAction(QIcon(":/icons/bullet_cross.png"), tr("Remove or delete"), this);
	deleteItemAct->setStatusTip(tr("Remove or delete"));
	connect(deleteItemAct, &QAction::triggered, this, [this, p, item] { deleteOrRemoveItemFromProject(item, p); });
	menu->addAction(deleteItemAct);

	return menu;
}

void ProjectCollectionWindow::addProjectNewFolder(QTreeWidgetItem *item, ProjectItem *p)
{
	bool ok;
	bool add_to_copy_path;
	std::string name;

	CustomDialog dlg(tr("Enter a folder name"), this);
	//dlg.addLabel    ("Please enter your details below ...");
	dlg.addLineEdit ("Folder name:  ", &name, tr("Invalid folder name"));
	dlg.addCheckBox (tr("Add to the COPY path list of the project"), &add_to_copy_path);
	dlg.exec();                    // Execution stops here until user closes dialog
	      
	QString text(name.c_str());

	if(dlg.wasCancelled() || !PathUtils::isValidDirectoryName(text)) {
		UiUtils::ErrorDialog(tr("Invalid folder name"));
		return;
	}

	ProjectFolder *folder = new ProjectFolder(text, false);
	folder->SetParent(p);
	folder->CreateIfNotExists();
	p->GetChildren()->insert(0, folder);
	QTreeWidgetItem *folderItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(text));
	folderItem->setIcon(0, QIcon(":/icons/folder.png"));
	folderItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<void*>(folder)));
	item->insertChild(0, folderItem);
}

void ProjectCollectionWindow::addProjectNewSourceFile(QTreeWidgetItem *item, ProjectItem *parent)
{
	bool ok;
	int sel = 1;
	std::string text;

	CustomDialog d(tr("New file"), this);
	d.addLabel    (tr("Please enter a filename"));
	d.addLineEdit ("", &text);
	d.addRadioGrp(tr("File type"), "Copy|Source", &sel);
	      
	d.exec();                    // Execution stops here until user closes dialog
	      
	if(d.wasCancelled()) {
		return;
	}

	QString filename = QString::fromStdString(text);
	if (filename.isEmpty() || !PathUtils::isValidFileName(filename)) {
		UiUtils::ErrorDialog(tr("Invalid file name"));
		return;
	}

	QMap<QString, QVariant> file_opts;
	ProjectFile* file = ProjectFile::newProjectFile(parent, PathUtils::combine(parent->GetBaseDir(), filename), false);
	if (file) {
		QFile prj_file(file->GetFileFullPath());
		if (!prj_file.open(QIODevice::WriteOnly)) {
			UiUtils::ErrorDialog(tr("Cannot create file"));
			delete file;
			return;
		}
		prj_file.close();

		QString ggp = file->GetFileFullPath();

		if (!file->writeSourceTemplate(prj_file, sel == 0 ? ProjectFileType::Copy : ProjectFileType::Source))
			return;

		file->PropertySetValue("build_action", sel == 0 ? "copy" : "compile");

		parent->GetChildren()->insert(0, file);
		file->SetParent(parent);

		QTreeWidgetItem *fileItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(filename));
		fileItem->setIcon(0, QIcon(":/icons/cblfile.png"));
		fileItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<void*>(file)));
		item->insertChild(0, fileItem);
		this->mainWindow->openFile(file->GetFileFullPath());
	}
	else {
		UiUtils::ErrorDialog(tr("Cannot create file ") + filename);
	}
}

void ProjectCollectionWindow::addProjectExistingSourceFile(QTreeWidgetItem *item, ProjectItem *parent)
{
	bool copy_files = false;
	QString project_dir = (dynamic_cast<ProjectFolder *>(parent) != nullptr) ? parent->GetFileFullPath() : parent->GetBaseDir();

	QFileDialog fdlg(this);
	fdlg.setFileMode(QFileDialog::ExistingFiles);
	fdlg.setNameFilter("COBOL source files (*.cbl);;COBOL copy files (*.cpy);;All files (*.*)");

	const QStringList fileNames = QFileDialog::getOpenFileNames(this);
	if (!fileNames.size())
		return;

	QFileInfo f(fileNames.at(0));
	if (f.absoluteDir() != project_dir) {
		QString msg = QString(tr("You can only add existing files under the directory of the project you are adding files to.\n\nWould you like to copy these files to the project directory (%1)?").arg(project_dir));
		copy_files = UiUtils::YesNoDialog(msg);
		if (!copy_files)
			return;
	}

	QStringList newPrjFiles;

	if (copy_files) {
		for (const QString& fileName : fileNames) {
			QString filetoAdd = PathUtils::combine(project_dir, QFileInfo(fileName).fileName());
			if (!QFile::copy(fileName, filetoAdd)) {
				UiUtils::ErrorDialog(QString(tr("Cannot copy file %1 to %2")).arg(fileName).arg(filetoAdd));
				return;
			}
			newPrjFiles.append(filetoAdd);
		}
	}
	else {
		newPrjFiles = fileNames;
	}

	for (const QString& fileName : newPrjFiles) {
		QFileInfo f(fileName);
		if ((!fileName.isEmpty()) && QFile::exists(fileName) && f.isReadable()) {


			ProjectFile* file = new ProjectFile();
			file->PropertySetValue("build_action", "compile");
			file->load(parent, PathUtils::getFilename(fileName));
			parent->GetChildren()->append(file);

			QTreeWidgetItem* fileItem = new QTreeWidgetItem((QTreeWidget*)0, QStringList(PathUtils::getFilename(fileName)));
			fileItem->setIcon(0, QIcon(":/icons/cblfile.png"));
			fileItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<void*>(file)));
			item->addChild(fileItem);

			this->mainWindow->openFile(file->GetFileFullPath());
		}
	}

}

void ProjectCollectionWindow::deleteOrRemoveItemFromProject(QTreeWidgetItem *item, ProjectItem *pi)
{
	if (pi->isVirtual()) {
//		delete pi;
		return;
	}

	CustomDialog cd(tr("Remove or delete file"), this, BS_CUSTOM);

	cd.addLabel(tr("Do you wanto to remove or delete permanently this file?"));
	cd.addCustomButton(tr("&Remove"), BB_ACCEPT);
	cd.addCustomButton(tr("&Delete"), BB_ACCEPT);
	cd.addCustomButton(tr("&Cancel"), BB_REJECT);

	if (cd.exec()) {
		int res = cd.customBtnClicked;
		if (res < 0 || res > 1)
			return;

		if (res == 1) {
			CustomDialog confirm(tr("Please confirm"), this, BS_NO_YES);
			confirm.addLabel(tr("Are you sure? This opeation is irreversible"));
			if (!confirm.exec())
				return;
		}

		// Close files open/being edited in UI
		checkUiAndSaveStatusBeforeDelete(pi);

		// Remove from UI
		item->parent()->removeChild(item);

		// Remove from project system tree
		int idx = pi->GetParent()->GetChildren()->indexOf(pi, 0);
		if (idx < 0)
			return;

		pi->GetParent()->GetChildren()->removeAt(idx);

		// If deleting, delete folder and files
		if (res == 1) {
			if (pi->GetItemType() == ProjectItemType::TFolder) {
				QDir d(pi->GetFileFullPath());
				if (d.isAbsolute() && d.exists()) {
					d.removeRecursively();
				}
			}

			if (pi->GetItemType() == ProjectItemType::TFile) {
				QFile d(pi->GetFileFullPath());
				if (QDir::isAbsolutePath(pi->GetFileFullPath()) && d.exists()) {
					d.remove();
				}
			}
		}

		// Delete objects from project system tree
		eraseItemRecursively(pi);
	}
}

void ProjectCollectionWindow::notifyPropertyValueChanged(PropertyDefinition *pd, QVariant v, ProjectItem* pi)
{
	if (pd->Name == PropertyConsts::IsStartupItem) {
		refresh_main_module_viewstate(pi, v);
	}
}


void ProjectCollectionWindow::refresh_main_module_viewstate(ProjectItem* pi, QVariant v)
{
	bool new_item_state = v.toBool();

	ProjectFile* pf = dynamic_cast<ProjectFile*>(pi);
	if (pf == nullptr)
		return;

	Project *prj = pf->getParentProject();
	if (prj == nullptr)
		return;

	QList<ProjectFile *> prj_files = prj->getAllCompilableFiles();
	QList<ProjectFile*>::iterator it;

	for (it = prj_files.begin(); it != prj_files.end(); ++it) {
		ProjectFile* f = (*it);
		f->PropertySetValue(PropertyConsts::IsStartupItem, (pf == f) ? new_item_state : !new_item_state);
	}

	QTreeWidgetItem* item = getProjectWidget(prj);
	if (item == nullptr)
		return;

	refresh_main_module_viewstate_children(item, pf, new_item_state);

	prj->save();
}

void ProjectCollectionWindow::refresh_main_module_viewstate_children(QTreeWidgetItem* item, ProjectFile *pf, bool b)
{
	for (int i = 0; i < item->childCount(); i++) {
		QTreeWidgetItem* child = item->child(i);
		if (child == nullptr)
			continue;

		ProjectItem *pi = (ProjectItem *) child->data(0, Qt::UserRole).value<void*>();
		ProjectFile* item_prjfile = dynamic_cast<ProjectFile*>(pi);
		if (item_prjfile) {
			setFontBold(child, (item_prjfile == pf) ? b : !b);
		}
	}
}

QTreeWidgetItem* ProjectCollectionWindow::getProjectWidget(Project* prj)
{
	QTreeWidgetItem *rootItem = treeview->topLevelItem(0);
	if (rootItem == nullptr)
		return nullptr;

	for (int i = 0; i < rootItem->childCount(); i++) {
		QTreeWidgetItem* item = rootItem->child(i);
		if (item == nullptr)
			continue;

		ProjectItem* pi = (ProjectItem*)item->data(0, Qt::UserRole).value<void*>();
		Project* item_prj = dynamic_cast<Project*>(pi);
		if (item_prj && item_prj == prj) {
			return item;
		}
		
	}
	return nullptr;
}

