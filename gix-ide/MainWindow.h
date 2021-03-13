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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

//#define QT_NO_CLIPBOARD

#ifdef __MINGW32__
#include <cstddef>
#endif

#include <QMainWindow>
#include <ProjectCollection.h>
#include <IdeSearchManager.h>

#include "IdeStatus.h"

class MdiChild;
class ProjectCollectionWindow;
class OutputWindow;
class WatchWindow;
class PropertyWindow;
class SettingsDialog;
class NewProjectDialog;
class SearchDialog;
class DataWindow;
class DependenciesWindow;
class DbManagerWindow;
class ConsoleWindow;
class NavigationWindow;

QT_BEGIN_NAMESPACE
class QAction;
class QLabel;
class QMenu;
class QMdiArea;
class QMdiSubWindow;
class QComboBox;
class QProgressBar;
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
	Q_OBJECT

	friend class IdeTaskManager;

public:
    MainWindow();

    bool openFile(const QString &fileName);
	QMdiSubWindow *findMdiChild(const QString &fileName) const;

	QVariant getCurrentConfiguration();
	QVariant getCurrentPlatform();
	QStringList getCurrentOpenFileList();
	void setAllMdiChildrenReadOnly(bool);
	void removeAllDebugMarkers();
    MdiChild* activeMdiChild() const;

    void blockMdiSignals(bool f);

protected:
    void closeEvent(QCloseEvent *event) override;

private slots:
    void newFile();
	void newPrj();
    void open();
	void openPrj();
	void openPrj(QString fileName);
	void editSettings();
	void closePrjCollection();
    void save();
    void run(bool run_detached);
    void debug();
    void debug_stop();
    void debug_step();
    void debug_continue();
    void breakpoint_toggle();
	void saveAll();
    void saveAs();
    void buildAll();
    void buildClean();
    void buildStop();
    void updateRecentFileActions();
    void updateRecentProjectsActions();
    void openRecentFile();
    void openRecentProject();
#ifndef QT_NO_CLIPBOARD
    void cut();
    void copy();
    void paste();
#endif
    void showSpecialChars();
    void about();
    void updateMenus();
	void subWindowActivated();
	void subWindowClosed(MdiChild* w);
    void updateWindowMenu();
    MdiChild *createMdiChild();
    void switchLayoutDirection();
    void updateClipboardActions(MdiChild* c, int p);
	void setupEolMenu();

    void bookmarkToggle();
    void bookmarkPrev();
    void bookmarkNext();
    void bookmarkClearAll();

	void convertFileEol(EolMode m);

private:
	QMenuBar *mainMenuBar;

    enum { MaxRecentFiles = 5, MaxRecentProjects = 5 };

	void IdeStatusChanged(IdeStatus);
	void IdeEditorChangedPosition(QString, int);

    void createActions();
    void createStatusBar();
    void readSettings();
    void writeSettings();
    bool loadFile(const QString &fileName);

    static bool hasRecentFiles();
    void prependToRecentFiles(const QString &fileName);
    void setRecentFilesVisible(bool visible);    
	
	static bool hasRecentProjects();
    void prependToRecentProjects(const QString &fileName);
    void setRecentProjectsVisible(bool visible);

	Project *get_runnable_project();

    void openSearch(SearchType search_type);

    QStringList getPlatformsForConfiguration(QString config);

	//void loadProjectCollectionState(ProjectCollection *);
	//bool closeCurrentProjectCollection();

    QMdiArea *mdiArea;

    QMenu *windowMenu;
	
    // File
    QAction *newPrjAct;
    QAction *newFileAct;
    QAction *saveAct;
    QAction *saveAllAct;
    QAction *saveAsAct;
    QAction *closePrjCollAct;
    QAction *recentFileActs[MaxRecentFiles];
    QAction *recentProjectsActs[MaxRecentProjects];
    QAction *recentFileSeparator;
    QAction *recentProjectsSeparator;
    QAction *recentFileSubMenuAct;
    QAction *recentProjectsSubMenuAct;

    // Edit
#ifndef QT_NO_CLIPBOARD
    QAction *cutAct;
    QAction *copyAct;
    QAction *pasteAct;
#endif
	QAction* showSpecialCharsAct;
	QMenu* submenuEolOps;
	QAction* eol_win_act;
	QAction* eol_linux_act;
	QAction* eol_mac_act;

    // Windows
    QAction *closeAct;
    QAction *closeAllAct;
    QAction *tileAct;
    QAction *cascadeAct;
    QAction *nextAct;
    QAction *previousAct;
    QAction *windowMenuSeparatorAct;

    // Settings
	QAction *settingsAct;
	
    // Build
    QAction *buildAllAct;
	QAction *buildCleanAct;
	QAction *buildStopAct;
	QAction *runAct;
	QAction *runDetachedAct;
	QAction *debugAct;
	QAction *debugStopAct;
	QAction *debugStepAct;
	QAction *debugContinueAct;
	QAction *toggleBreakpointAct;

    // Search
    QAction* findAct;
    QAction* findInFilesAct;
    QAction* findNextAct;
    QAction* findPrevAct;
    QAction* replaceAct;
    QAction* bookmarkToggleAct;
    QAction* bookmarkNextAct;
    QAction* bookmarkPrevAct;
    QAction* bookmarkClearAllAct;

	QDockWidget* watch_dock;
	QDockWidget* prjcoll_dock;
	QDockWidget* propwin_dock;
	QDockWidget* output_dock;
	QDockWidget* working_storage_dock;
	QDockWidget* deps_dock;
	QDockWidget* dbmanager_dock;
	QDockWidget* console_dock;
	QDockWidget* navigation_dock;

	ProjectCollectionWindow* prjcoll_window;
	PropertyWindow* property_window;
	OutputWindow* output_window;
	WatchWindow* watch_window;
	DataWindow* working_storage_window;
	DependenciesWindow* deps_window;
	DbManagerWindow *dbmanager_window;
	ConsoleWindow *console_window;
	NavigationWindow *navigation_window;
	

	QComboBox *cbPlatform;
	QComboBox *cbConfiguration;

	QProgressBar * progress_bar;
	QLabel * status_label;

	NewProjectDialog *new_prj_dlg;
	SettingsDialog *settings_dlg;
	SearchDialog *search_dlg;
};

#endif
