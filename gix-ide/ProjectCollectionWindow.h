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

#pragma once

#include <QMainWindow>
#include <QToolBar>
#include <QTreeWidgetItem>
#include <QFileSystemModel>
#include <QMenu>
#include <QProgressDialog>

#include "MainWindow.h"
#include "DragDropTreeWidget.h"

class QWidget;
class ProjectCollection;
class Project;
class ProjectItem;
class ProjectFile;
class ProjectFolder;
class MainWindow;

class ProjectCollectionWindow : public QMainWindow
{
	Q_OBJECT
	

public:
	ProjectCollectionWindow(QWidget * parent, MainWindow * mw);
	~ProjectCollectionWindow();

	void setContent(ProjectCollection *prjc);
	void refreshContent();
	ProjectItem *getCurrentSelection();

	void myItemDoubleClicked(QTreeWidgetItem * item, int column);
	void myItemClicked(QTreeWidgetItem * item, int column);
	void prepareMenu(const QPoint & pos);

public slots:
	void addProjectNewFolder(QTreeWidgetItem *, ProjectItem *);
	void addProjectNewSourceFile(QTreeWidgetItem *, ProjectItem *);
	void addProjectExistingSourceFile(QTreeWidgetItem *, ProjectItem *);
	
	void deleteOrRemoveItemFromProject(QTreeWidgetItem *, ProjectItem *);
	void notifyPropertyValueChanged(PropertyDefinition*, QVariant, ProjectItem* pi);


signals:
	void selectionChanged(ProjectItem *);

public slots:
	void itemDropped(QTreeWidgetItem *tvi, const QMimeData *md);

private:
	DragDropTreeWidget *treeview;
	MainWindow *mainWindow;

	void setContent(ProjectCollection* prjc, bool refresh_only);
	
	QMenu *PrepareProjectCollectionMenu(QTreeWidgetItem *, ProjectCollection * p);
	QMenu *PrepareProjectMenu(QTreeWidgetItem *, Project * p);
	QMenu *PrepareProjectFileMenu(QTreeWidgetItem *, ProjectFile * p);
	QMenu *PrepareProjectFolderMenu(QTreeWidgetItem *, ProjectFolder * p);
	void add_children(ProjectItem *parent, QTreeWidgetItem *parent_widget, QProgressDialog& progress);
	
	void eraseItemRecursively(ProjectItem *pi);
	void checkUiAndSaveStatusBeforeDelete(ProjectItem *pi);
	
	void refresh_main_module_viewstate(ProjectItem* pi, QVariant v);
	void refresh_main_module_viewstate_children(QTreeWidgetItem* item, ProjectFile *, bool);
	QTreeWidgetItem * getProjectWidget(Project* prj);

    bool addExistingFiles(const QStringList &fileNames, const QString &project_dir, ProjectItem *parent, QTreeWidgetItem *item);

};

