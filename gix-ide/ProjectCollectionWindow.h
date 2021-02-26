#pragma once

#include <QMainWindow>
#include <QToolBar>
#include <QTreeWidgetItem>
#include <QFileSystemModel>
#include <QMenu>
#include <QProgressDialog>

#include "MainWindow.h"

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

private:
	QTreeWidget *treeview;
	MainWindow *mainWindow;

	void setContent(ProjectCollection* prjc, bool refresh_only);
	
	QMenu *PrepareProjectCollectionMenu(QTreeWidgetItem *, ProjectCollection * p);
	QMenu *PrepareProjectMenu(QTreeWidgetItem *, Project * p);
	QMenu *PrepareProjectFileMenu(QTreeWidgetItem *, ProjectFile * p);
	QMenu *PrepareProjectFolderMenu(QTreeWidgetItem *, ProjectFolder * p);
	void add_children(ProjectItem *parent, QTreeWidgetItem *parent_widget, QProgressDialog& progress);
	
	void add_copy_deps_children(Project *prj, QTreeWidgetItem *parent_widget);
	void eraseItemRecursively(ProjectItem *pi);
	void checkUiAndSaveStatusBeforeDelete(ProjectItem *pi);
	
	void refresh_main_module_viewstate(ProjectItem* pi, QVariant v);
	void refresh_main_module_viewstate_children(QTreeWidgetItem* item, ProjectFile *, bool);
	void setFontBold(QTreeWidgetItem* fileItem, bool b);
	QTreeWidgetItem * getProjectWidget(Project* prj);

};

