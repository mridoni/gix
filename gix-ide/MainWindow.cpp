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

#include "MainWindow.h"

#include <QtWidgets>
#include <QSettings>

#include "MdiChild.h"
#include "ProjectCollectionWindow.h"
#include "PropertyWindow.h"
#include "OutputWindow.h"
#include "WatchWindow.h"
#include "SettingsDialog.h"
#include "SearchDialog.h"
#include "BuildDriver.h"
#include "UiUtils.h"
#include "IdeTaskManager.h"
#include "PathUtils.h"
#include "Ide.h"
#include "CompilerManager.h"
#include "linq/linq.hpp"
#include "NewProjectDialog.h"
#include "DataWindow.h"
#include "DependenciesWindow.h"
#include "DbManagerWindow.h"
#include "ConsoleWindow.h"
#include "NavigationWindow.h"
#include "GixGlobals.h"
#include "GixVersion.h"


using namespace cpplinq;

MainWindow::MainWindow()
	: mdiArea(new QMdiArea)
{
#ifdef __APPLE__
	mainMenuBar = new QMenuBar(0);
	this->setUnifiedTitleAndToolBarOnMac(true);
#else
	mainMenuBar = menuBar();
#endif

	mdiArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
	mdiArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
	mdiArea->setViewMode(QMdiArea::TabbedView);

	setCentralWidget(mdiArea);
	mdiArea->setTabsClosable(true);
	connect(mdiArea, &QMdiArea::subWindowActivated, this, &MainWindow::subWindowActivated);

	prjcoll_dock = new QDockWidget(tr("Project Collection"), this);
	prjcoll_window = new ProjectCollectionWindow(prjcoll_dock, this);
	prjcoll_dock->setWidget(prjcoll_window);
	addDockWidget(Qt::RightDockWidgetArea, prjcoll_dock);

	deps_dock = new QDockWidget(tr("Dependencies"), this);
	deps_window = new DependenciesWindow(deps_dock, this);
	deps_dock->setWidget(deps_window);
	addDockWidget(Qt::RightDockWidgetArea, deps_dock);

	propwin_dock = new QDockWidget(tr("Properties"), this);
	property_window = new PropertyWindow(propwin_dock, this);
	propwin_dock->setWidget(property_window);
	addDockWidget(Qt::RightDockWidgetArea, propwin_dock);

	output_dock = new QDockWidget(tr("Output"), this);
	output_window = new OutputWindow(output_dock, this);
	output_dock->setWidget(output_window);
	addDockWidget(Qt::BottomDockWidgetArea, output_dock);

	watch_dock = new QDockWidget(tr("Watch"), this);
	watch_window = new WatchWindow(watch_dock, this);
	watch_dock->setWidget(watch_window);
	addDockWidget(Qt::BottomDockWidgetArea, watch_dock);

	working_storage_dock = new QDockWidget(tr("Data Section"), this);
	working_storage_window = new DataWindow(working_storage_dock, this);
	working_storage_dock->setWidget(working_storage_window);
	addDockWidget(Qt::BottomDockWidgetArea, working_storage_dock);

	dbmanager_dock = new QDockWidget(tr("DB Manager"), this);
	dbmanager_window = new DbManagerWindow(dbmanager_dock, this);
	dbmanager_dock->setWidget(dbmanager_window);
	addDockWidget(Qt::LeftDockWidgetArea, dbmanager_dock);

	console_dock = new QDockWidget(tr("Console"), this);
	console_window = new ConsoleWindow(console_dock, this);
	console_dock->setWidget(console_window);
	addDockWidget(Qt::BottomDockWidgetArea, console_dock);

	navigation_dock = new QDockWidget(tr("Navigator"), this);
	navigation_window = new NavigationWindow(navigation_dock, this);
	navigation_dock->setWidget(navigation_window);
	addDockWidget(Qt::LeftDockWidgetArea, navigation_dock);

	watch_dock->hide();

	createActions();
	createStatusBar();
	updateMenus();

	readSettings();

	setWindowTitle(tr("Gix-IDE"));
	setUnifiedTitleAndToolBarOnMac(true);

	QList<QTabBar *> tabBarList = mdiArea->findChildren<QTabBar*>();
	QTabBar *tabBar = tabBarList.at(0);
	if (tabBar) {
		tabBar->setExpanding(false);
	}

	setCorner(Qt::BottomRightCorner, Qt::RightDockWidgetArea);

	prjcoll_dock->setContentsMargins(0, 0, 8, 0);
	propwin_dock->setContentsMargins(0, 0, 8, 0);

	connect(prjcoll_window, &ProjectCollectionWindow::selectionChanged, property_window, &PropertyWindow::setContent);

	Ide::TaskManager()->init(this, output_window, prjcoll_window, console_window, watch_window, navigation_window);
	Ide::TaskManager()->setCurrentProjectCollection(nullptr);

    connect(Ide::TaskManager(), &IdeTaskManager::IdeEditorChangedPosition, this, [this](QString a, int b) { IdeEditorChangedPosition(a, b);  }, Qt::ConnectionType::QueuedConnection);

	status_label = new QLabel();
	statusBar()->addPermanentWidget(status_label);

	progress_bar = new QProgressBar(this);
	progress_bar->setFixedHeight(16);
	progress_bar->setFixedWidth(200);
	progress_bar->setStyleSheet("margin-right:8px");
	progress_bar->setAlignment(Qt::AlignCenter);
	statusBar()->addPermanentWidget(progress_bar);

	progress_bar->hide();

	connect(property_window, &PropertyWindow::notifyPropertyValueChanged, prjcoll_window, [this](PropertyDefinition* pd, QVariant value, ProjectItem* pi) {
		prjcoll_window->notifyPropertyValueChanged(pd, value, pi);
	});

	connect(Ide::TaskManager(), &IdeTaskManager::SettingsChanged, this, [this] {
		QString cur_platform = cbPlatform->currentData().toString();

		this->cbPlatform->clear();
		QStringList available_platforms = this->getPlatformsForConfiguration(cbConfiguration->itemData(DEFAULT_TARGET_CONFIG).toString());
		for (QString p : available_platforms)
			cbPlatform->addItem(p, p);

		if (cur_platform != "" && this->cbPlatform->findData(cur_platform) >= 0)
			this->cbPlatform->setCurrentIndex(this->cbPlatform->findData(cur_platform));
		else {
			this->cbPlatform->setCurrentIndex(0);
		}
	});

#ifdef WIN32
	if (Ide::TaskManager()->checkAndSetupTestHelper()) {
		GixGlobals::getLogManager()->trace(LOG_TEST, "WARNING! test helper started");
	}
#endif


	output_window->addPanes({ "IDE", "Build", "Debug" } );

	emit Ide::TaskManager()->IdeReady();
}

void MainWindow::closeEvent(QCloseEvent *event)
{
	Ide::TaskManager()->setBackgroundTasksEnabled(false);

	if (!Ide::TaskManager()->closeCurrentProjectCollection())
		return;

	mdiArea->closeAllSubWindows();
	if (mdiArea->currentSubWindow()) {
		event->ignore();
	}
	else {
		writeSettings();
		event->accept();
	}
}

void MainWindow::newFile()
{
	MdiChild *child = createMdiChild();
	child->newFile();
	child->show();
}

void MainWindow::newPrj()
{
	new_prj_dlg = new NewProjectDialog(nullptr, this);
	new_prj_dlg->show();
}

void MainWindow::open()
{
	const QString fileName = QFileDialog::getOpenFileName(this);
	if (!fileName.isEmpty())
		openFile(fileName);
}

void MainWindow::openPrj()
{
	QString docs_dir = QStandardPaths::locate(QStandardPaths::DocumentsLocation, ".", QStandardPaths::LocateOption::LocateDirectory);
	docs_dir = PathUtils::combine(docs_dir, "gix");

	QString fileName = QFileDialog::getOpenFileName(this,
		tr("Open Project/Project Collection"), docs_dir,
		tr("Project Collection (*.gix);;All Files (*)"));

	if (fileName.isEmpty())
		return;

	openPrj(fileName);
}

void MainWindow::openPrj(QString fileName)
{

	if (Ide::TaskManager()->getCurrentProjectCollection() != nullptr) {
		if (!Ide::TaskManager()->closeCurrentProjectCollection())
			return;
	}

	bool rc = Ide::TaskManager()->loadProjectCollection(fileName);
	if (!rc) {
		UiUtils::ErrorDialog(tr(QString("Cannot load project collection %1").arg(fileName).toUtf8()));
	}
}

void MainWindow::editSettings()
{
	settings_dlg = new SettingsDialog(this);
	settings_dlg->exec();
}

void MainWindow::closePrjCollection()
{
	bool has_prj_collection = (Ide::TaskManager()->getCurrentProjectCollection() != nullptr);
	if (!has_prj_collection)
		return;

	Ide::TaskManager()->closeCurrentProjectCollection();

	updateMenus();
}

bool MainWindow::openFile(const QString &fileName)
{
	if (QMdiSubWindow *existing = findMdiChild(fileName)) {
		mdiArea->setActiveSubWindow(existing);
		return true;
	}
	const bool succeeded = loadFile(fileName);
	if (succeeded) {
		statusBar()->showMessage(tr("File loaded"), 2000);
		if (Ide::TaskManager()->getStatus() != IdeStatus::LoadingOrSaving)
			Ide::TaskManager()->saveCurrentProjectCollectionState();
	}

	return succeeded;
}

QVariant MainWindow::getCurrentConfiguration()
{
	return cbConfiguration->currentData();
}

QVariant MainWindow::getCurrentPlatform()
{
	return cbPlatform->currentData();
}

void MainWindow::setAllMdiChildrenReadOnly(bool b)
{
	foreach(QMdiSubWindow *window, mdiArea->subWindowList()) {
		MdiChild *mdiChild = qobject_cast<MdiChild *>(window->widget());
		mdiChild->setReadOnly(b);
	}
}

QStringList MainWindow::getCurrentOpenFileList()
{
	QStringList res;

	foreach(QMdiSubWindow *window, mdiArea->subWindowList()) {
		MdiChild *mdiChild = qobject_cast<MdiChild *>(window->widget());
		if (mdiChild)
			res.append(mdiChild->currentFile());
	}

	return res;
}

bool MainWindow::loadFile(const QString &fileName)
{
	MdiChild *child = createMdiChild();
	const bool succeeded = child->loadFile(fileName);
	if (succeeded) {
		child->show();
		MainWindow::prependToRecentFiles(fileName);
	}
	else {
		child->close();
		mdiArea->removeSubWindow((QWidget *) child->parent());
		delete child;
	}
	
	return succeeded;
}

static inline QString recentFilesKey() { return QStringLiteral("recentFileList"); }
static inline QString recentProjectsKey() { return QStringLiteral("recentProjectsList"); }
static inline QString fileKey() { return QStringLiteral("file"); }
static inline QString projectKey() { return QStringLiteral("project"); }

static QStringList readRecentFiles(QSettings &settings)
{
	QStringList result;
	const int count = settings.beginReadArray(recentFilesKey());
	for (int i = 0; i < count; ++i) {
		settings.setArrayIndex(i);
		result.append(settings.value(fileKey()).toString());
	}
	settings.endArray();
	return result;
}

static QStringList readRecentProjects(QSettings &settings)
{
	QStringList result;
	const int count = settings.beginReadArray(recentProjectsKey());
	for (int i = 0; i < count; ++i) {
		settings.setArrayIndex(i);
		result.append(settings.value(projectKey()).toString());
	}
	settings.endArray();
	return result;
}

static void writeRecentFiles(const QStringList &files, QSettings &settings)
{
	const int count = files.size();
	settings.beginWriteArray(recentFilesKey());
	for (int i = 0; i < count; ++i) {
		settings.setArrayIndex(i);
		settings.setValue(fileKey(), files.at(i));
	}
	settings.endArray();
}

static void writeRecentProjects(const QStringList &projects, QSettings &settings)
{
	const int count = projects.size();
	settings.beginWriteArray(recentProjectsKey());
	for (int i = 0; i < count; ++i) {
		settings.setArrayIndex(i);
		settings.setValue(projectKey(), projects.at(i));
	}
	settings.endArray();
}

bool MainWindow::hasRecentFiles()
{
	QSettings settings(QCoreApplication::organizationName(), QCoreApplication::applicationName());
	const int count = settings.beginReadArray(recentFilesKey());
	settings.endArray();
	return count > 0;
}

bool MainWindow::hasRecentProjects()
{
	QSettings settings(QCoreApplication::organizationName(), QCoreApplication::applicationName());
	const int count = settings.beginReadArray(recentProjectsKey());
	settings.endArray();
	return count > 0;
}

void MainWindow::prependToRecentProjects(const QString & fileName)
{
	QSettings settings(QCoreApplication::organizationName(), QCoreApplication::applicationName());

	const QStringList oldRecentProjects = readRecentProjects(settings);
	QStringList recentProjects = oldRecentProjects;
	recentProjects.removeAll(fileName);
	recentProjects.prepend(fileName);
	if (oldRecentProjects != recentProjects)
		writeRecentProjects(recentProjects, settings);

	setRecentProjectsVisible(!recentProjects.isEmpty());
}

void MainWindow::setRecentProjectsVisible(bool visible)
{
	recentProjectsSubMenuAct->setVisible(visible);
	recentProjectsSeparator->setVisible(visible);
}

Project * MainWindow::get_runnable_project()
{
	Project *prj = nullptr;
	ProjectCollection *ppj = Ide::TaskManager()->getCurrentProjectCollection();
	auto prjs = from(*(ppj->GetChildren())).where([](ProjectItem *a) { return a->GetItemType() == ProjectItemType::TProject;  }).to_vector();
	if (prjs.size() == 1) {
		prj = static_cast<Project *>(prjs.at(0));
	}
	else {
		ProjectItem *pi = prjcoll_window->getCurrentSelection();
		prj = dynamic_cast<Project *>(pi);
	}
	return prj;
}


void MainWindow::prependToRecentFiles(const QString &fileName)
{
	QSettings settings(QCoreApplication::organizationName(), QCoreApplication::applicationName());

	const QStringList oldRecentFiles = readRecentFiles(settings);
	QStringList recentFiles = oldRecentFiles;
	recentFiles.removeAll(fileName);
	recentFiles.prepend(fileName);
	if (oldRecentFiles != recentFiles)
		writeRecentFiles(recentFiles, settings);

	setRecentFilesVisible(!recentFiles.isEmpty());
}

void MainWindow::setRecentFilesVisible(bool visible)
{
	recentFileSubMenuAct->setVisible(visible);
	recentFileSeparator->setVisible(visible);
}

void MainWindow::updateRecentFileActions()
{
	QSettings settings(QCoreApplication::organizationName(), QCoreApplication::applicationName());

	const QStringList recentFiles = readRecentFiles(settings);
	const int count = qMin(int(MaxRecentFiles), recentFiles.size());
	int i = 0;
	for (; i < count; ++i) {
		const QString fileName = QFileInfo(recentFiles.at(i)).fileName();
		recentFileActs[i]->setText(tr("&%1 %2").arg(i + 1).arg(fileName));
		recentFileActs[i]->setData(recentFiles.at(i));
		recentFileActs[i]->setVisible(true);
	}
	for (; i < MaxRecentFiles; ++i)
		recentFileActs[i]->setVisible(false);
}

void MainWindow::updateRecentProjectsActions()
{
	QSettings settings(QCoreApplication::organizationName(), QCoreApplication::applicationName());

	const QStringList recentProjects = readRecentProjects(settings);
	const int count = qMin(int(MaxRecentProjects), recentProjects.size());
	int i = 0;
	for (; i < count; ++i) {
		const QString fileName = QFileInfo(recentProjects.at(i)).fileName();
		recentProjectsActs[i]->setText(tr("&%1 %2").arg(i + 1).arg(fileName));
		recentProjectsActs[i]->setData(recentProjects.at(i));
		recentProjectsActs[i]->setVisible(true);
	}
	for (; i < MaxRecentProjects; ++i)
		recentProjectsActs[i]->setVisible(false);
}

void MainWindow::openRecentFile()
{
	if (const QAction *action = qobject_cast<const QAction *>(sender()))
		openFile(action->data().toString());
}

void MainWindow::openRecentProject()
{
	if (const QAction *action = qobject_cast<const QAction *>(sender()))
		openPrj(action->data().toString());
}

void MainWindow::save()
{
	if (activeMdiChild() && activeMdiChild()->save())
		statusBar()->showMessage(tr("File saved"), 2000);
}

void MainWindow::run(bool run_detached)
{
	Project *prj = get_runnable_project();
	if (prj == nullptr) {
		UiUtils::ErrorDialog(tr("Select a project to be run"));
		return;
	}

	Ide::TaskManager()->runProject(prj, cbConfiguration->currentData().toString(), cbPlatform->currentData().toString(), run_detached);
}

void MainWindow::debug()
{
	if (cbConfiguration->currentData().toString() != "debug") {
		UiUtils::ErrorDialog(tr("Select a debug configuration"));
		return;
	}

	Project *prj = get_runnable_project();
	if (prj == nullptr) {
		UiUtils::ErrorDialog(tr("Select a project to be debugged"));
		return;
	}

	Ide::TaskManager()->debugProject(prj, cbConfiguration->currentData().toString(), cbPlatform->currentData().toString());
}

void MainWindow::debug_stop()
{
	Ide::TaskManager()->debugStop();
}

void MainWindow::debug_step()
{
	Ide::TaskManager()->debugStep();
}

void MainWindow::debug_continue()
{
	Ide::TaskManager()->debugContinue();
}

void MainWindow::breakpoint_toggle()
{
	MdiChild* child = activeMdiChild();
	if (!child)
		return;

	child->toggleBreakpoint();
}

void MainWindow::saveAll()
{
	bool rc = Ide::TaskManager()->saveAll();
}

void MainWindow::saveAs()
{
	MdiChild *child = activeMdiChild();
	if (child && child->saveAs()) {
		statusBar()->showMessage(tr("File saved"), 2000);
		MainWindow::prependToRecentFiles(child->currentFile());
	}
}

void MainWindow::buildAll()
{
	Ide::TaskManager()->buildAll(cbConfiguration->currentData().toString(), cbPlatform->currentData().toString());
}

void MainWindow::buildClean()
{
	Ide::TaskManager()->buildClean(cbConfiguration->currentData().toString(), cbPlatform->currentData().toString());
}

void MainWindow::buildStop()
{
	Ide::TaskManager()->buildStop();
}

#ifndef QT_NO_CLIPBOARD
void MainWindow::cut()
{
	if (activeMdiChild())
		activeMdiChild()->cut();
}

void MainWindow::copy()
{
	if (activeMdiChild())
		activeMdiChild()->copy();
}

void MainWindow::paste()
{
	if (activeMdiChild())
		activeMdiChild()->paste();
}
#endif

void MainWindow::showSpecialChars()
{
	if (activeMdiChild())
		activeMdiChild()->showSpecialChars(showSpecialCharsAct->isChecked());
}

void MainWindow::setupEolMenu()
{
	MdiChild *c = activeMdiChild();
	if (!c)
		return;

	EolMode m = c->getFileConfiguredEolMode();

	eol_win_act->setEnabled(eol_win_act->data().toInt() != ((int)m));
	eol_linux_act->setEnabled(eol_linux_act->data().toInt() != ((int)m));
	eol_mac_act->setEnabled(eol_mac_act->data().toInt() != ((int)m));
}

void MainWindow::about()
{
	QString info_msg = tr("About Gix-IDE");
	QString main_msg = tr("Gix-IDE is an IDE and platform for GnuCOBOL, providing a native debugger, ESQL and HTTP REST services.");
	QString license_msg = tr("Gix-IDE is licensed under the GPL 3.0 license.");
	QString ver = getGixIdePrintableVersion();
	QMessageBox::about(this, info_msg,
		"<h1><b>Gix-IDE</h1>v" + ver + "<p>" + main_msg + "</p><p>(c) Marco Ridoni 2021-2022</p><p>" + license_msg + "</p>");
}

void MainWindow::updateMenus()
{
	bool hasMdiChild = (activeMdiChild() != 0);

	saveAct->setEnabled(hasMdiChild);
	saveAsAct->setEnabled(hasMdiChild);
	showSpecialCharsAct->setEnabled(hasMdiChild);
	showSpecialCharsAct->setChecked(hasMdiChild && activeMdiChild()->isShowingSpecialChars());
#ifndef QT_NO_CLIPBOARD
	pasteAct->setEnabled(hasMdiChild);
#endif

	submenuEolOps->setEnabled(hasMdiChild);

	closeAct->setEnabled(hasMdiChild);
	closeAllAct->setEnabled(hasMdiChild);
	tileAct->setEnabled(hasMdiChild);
	cascadeAct->setEnabled(hasMdiChild);
	nextAct->setEnabled(hasMdiChild);
	previousAct->setEnabled(hasMdiChild);
	windowMenuSeparatorAct->setVisible(hasMdiChild);

	findAct->setEnabled(hasMdiChild);
	findInFilesAct->setEnabled(hasMdiChild);
	findNextAct->setEnabled(hasMdiChild);
	findPrevAct->setEnabled(hasMdiChild);
	replaceAct->setEnabled(hasMdiChild);
	bookmarkToggleAct->setEnabled(hasMdiChild);
	bookmarkNextAct->setEnabled(hasMdiChild);
	bookmarkPrevAct->setEnabled(hasMdiChild);
	bookmarkClearAllAct->setEnabled(hasMdiChild);

	bool has_prj_collection = (Ide::TaskManager()->getCurrentProjectCollection() != nullptr);

	closePrjCollAct->setEnabled(has_prj_collection && Ide::TaskManager()->getStatus() == IdeStatus::Editing);
	saveAllAct->setEnabled(has_prj_collection && Ide::TaskManager()->getStatus() == IdeStatus::Editing);
	buildAllAct->setEnabled(has_prj_collection && Ide::TaskManager()->getStatus() == IdeStatus::Editing);
	buildCleanAct->setEnabled(has_prj_collection && Ide::TaskManager()->getStatus() == IdeStatus::Editing);
	buildStopAct->setEnabled(has_prj_collection && Ide::TaskManager()->getStatus() == IdeStatus::Building);
	runAct->setEnabled(has_prj_collection && Ide::TaskManager()->getStatus() == IdeStatus::Editing);
	runDetachedAct->setEnabled(has_prj_collection && Ide::TaskManager()->getStatus() == IdeStatus::Editing);
	debugAct->setEnabled(has_prj_collection && Ide::TaskManager()->getStatus() == IdeStatus::Editing);
	debugStopAct->setEnabled(has_prj_collection &&
		(Ide::TaskManager()->getStatus() == IdeStatus::Debugging || Ide::TaskManager()->getStatus() == IdeStatus::DebuggingOnBreak));

	debugStepAct->setEnabled(has_prj_collection &&
		(Ide::TaskManager()->getStatus() == IdeStatus::DebuggingOnBreak));

	debugContinueAct->setEnabled(has_prj_collection &&
		(Ide::TaskManager()->getStatus() == IdeStatus::DebuggingOnBreak));

	cbConfiguration->setEnabled(has_prj_collection);
	cbPlatform->setEnabled(Ide::TaskManager()->getCurrentProjectCollection() != nullptr);

	toggleBreakpointAct->setEnabled(has_prj_collection &&
		(Ide::TaskManager()->getStatus() == IdeStatus::Debugging || Ide::TaskManager()->getStatus() == IdeStatus::DebuggingOnBreak));

#ifndef QT_NO_CLIPBOARD
	bool hasSelection = (activeMdiChild() &&
		activeMdiChild()->getSelText().size() > 0);
	cutAct->setEnabled(hasSelection);
	copyAct->setEnabled(hasSelection);
#endif
}

void MainWindow::subWindowActivated()
{
	if (this->activeMdiChild() != nullptr) {
		bool subwindow_changed = last_active != this->activeMdiChild();
		last_active = this->activeMdiChild();

		QString f = this->activeMdiChild()->currentFile();

		GixGlobals::getLogManager()->trace(LOG_TEST, "Activated window for {}", f);

		ProjectCollection *ppj = Ide::TaskManager()->getCurrentProjectCollection();
		if (ppj != nullptr) {
			ProjectFile *pf = ppj->locateProjectFileByPath(f, true);
			if (subwindow_changed)
				emit Ide::TaskManager()->fileActivated(pf);
		}
	}
	else
		last_active = nullptr;

	updateMenus();
}

void MainWindow::subWindowClosed(MdiChild* w)
{
	this->working_storage_window->setContent(nullptr);
	updateMenus();

	ProjectCollection* ppj = Ide::TaskManager()->getCurrentProjectCollection();
	if (ppj != nullptr) {
		ProjectFile* pf = ppj->locateProjectFileByPath(w->currentFile());
		emit Ide::TaskManager()->fileClosed(nullptr);
	}
}

void MainWindow::updateWindowMenu()
{
	windowMenu->clear();

	windowMenu->addAction(prjcoll_dock->toggleViewAction());
	windowMenu->addAction(propwin_dock->toggleViewAction());
	windowMenu->addAction(output_dock->toggleViewAction());
	windowMenu->addAction(dbmanager_dock->toggleViewAction());
	windowMenu->addAction(navigation_dock->toggleViewAction());
	windowMenu->addAction(deps_dock->toggleViewAction());
	windowMenu->addAction(console_dock->toggleViewAction());

	if (Ide::TaskManager()->getStatus() == IdeStatus::Debugging || Ide::TaskManager()->getStatus() == IdeStatus::DebuggingOnBreak)
		windowMenu->addAction(watch_dock->toggleViewAction());

	windowMenu->addAction(closeAct);
	windowMenu->addAction(closeAllAct);
	windowMenu->addSeparator();
	windowMenu->addAction(tileAct);
	windowMenu->addAction(cascadeAct);
	windowMenu->addSeparator();
	windowMenu->addAction(nextAct);
	windowMenu->addAction(previousAct);
	windowMenu->addAction(windowMenuSeparatorAct);

	QList<QMdiSubWindow *> windows = mdiArea->subWindowList();
	windowMenuSeparatorAct->setVisible(!windows.isEmpty());

	for (int i = 0; i < windows.size(); ++i) {
		QMdiSubWindow *mdiSubWindow = windows.at(i);
		MdiChild *child = qobject_cast<MdiChild *>(mdiSubWindow->widget());

		QString text;
		if (i < 9) {
			text = tr("&%1 %2").arg(i + 1)
				.arg(child->userFriendlyCurrentFile());
		}
		else {
			text = tr("%1 %2").arg(i + 1)
				.arg(child->userFriendlyCurrentFile());
		}
		QAction *action = windowMenu->addAction(text, mdiSubWindow, [this, mdiSubWindow]() {
			mdiArea->setActiveSubWindow(mdiSubWindow);
		});
		action->setCheckable(true);
		action->setChecked(child == activeMdiChild());
	}
}

MdiChild *MainWindow::createMdiChild()
{
	QMdiArea *mda = mdiArea;
	MdiChild *child = new MdiChild();
	QMdiSubWindow *sw = mdiArea->addSubWindow(child);
	QList<QTabBar *> tabBarList = mdiArea->findChildren<QTabBar*>();
	//if (tabBarList.size() > 0) {
	//	int i = mdiArea->subWindowList().indexOf(sw);
	//	if (i >= 0) {
	//		QTabBar *tabBar = tabBarList.at(0);
	//		QPushButton *b = new QPushButton();
	//		b->setIcon(QIcon(":/icons/bullet_cross.png"));
	//		b->setFixedSize(8, 8);
	//		b->setFlat(true);
	//		connect(b, &QPushButton::clicked, tabBar, [sw, mda] { 
	//			if (sw->isVisible())
	//				sw->close(); 
	//		});
	//		//connect(child, &MdiChild::windowClosed, this, &MainWindow::subWindowClosed);
	//		tabBar->setTabButton(i, QTabBar::ButtonPosition::RightSide, b);
	//	}
	//}

#ifndef QT_NO_CLIPBOARD
	connect(child, &CodeEditor::updateUi, this, [this, child] (int p){ 
		updateClipboardActions(child, p); 
		if (p & SC_UPDATE_SELECTION) {
			emit child->caretPositionChanged();
		}
	});
#endif

	return child;
}

void MainWindow::IdeStatusChanged(IdeStatus s)
{
	updateMenus();
	watch_dock->setVisible(s == IdeStatus::Debugging || s == IdeStatus::DebuggingOnBreak);

	if (s != IdeStatus::DebuggingOnBreak) {
		MdiChild *cur = this->activeMdiChild();
		if (cur)
			cur->markerDeleteAll(MRKR_DBG_CURLINE);
	}
}

void MainWindow::IdeEditorChangedPosition(QString src, int ln)
{
    MdiChild* cur = this->activeMdiChild();
    if (cur)
        cur->markerDeleteAll(MRKR_DBG_CURLINE);

    if (QFile(src).exists()) {
        QMdiSubWindow *win = findMdiChild(src);
        if (!win) {
            if (loadFile(src)) {
                win = findMdiChild(src);
            }
        }

        if (win) {
            MdiChild *mdiChild = qobject_cast<MdiChild *>(win->widget());
            mdiChild->highlightDebuggedLine(ln);
            mdiChild->ensureLineVisible(ln);
            mdiArea->setActiveSubWindow(win);
            if (Ide::TaskManager()->getStatus() == IdeStatus::Debugging || Ide::TaskManager()->getStatus() == IdeStatus::DebuggingOnBreak)
                mdiChild->setReadOnly(true);
        }
    }
}

void MainWindow::createActions()
{
	QMenu *fileMenu = mainMenuBar->addMenu(tr("&File"));
	QToolBar *fileToolBar = addToolBar(tr("File"));
	QToolBar *buildToolBar = addToolBar(tr("Build"));
	QToolBar *runDebugToolBar = addToolBar(tr("Run/Debug"));

	fileToolBar->setStyleSheet("QToolButton{margin:0px;spacing:0px;padding:0px;}");
	buildToolBar->setStyleSheet("QToolButton{margin:0px;spacing:0px;padding:0px;}");
	runDebugToolBar->setStyleSheet("QToolButton{margin:0px;spacing:0px;padding:0px;}");

	QToolButton *btnNew = new QToolButton(this);
	QToolButton *btnOpen = new QToolButton(this);

	const QIcon newPrjIcon = QIcon(":/icons/new_prj.png");
	newPrjAct = new QAction(newPrjIcon, tr("&New Project/Project Collection"), this);
	newPrjAct->setShortcuts(QKeySequence::New);
	newPrjAct->setStatusTip(tr("Create a new project/project collection"));
	connect(newPrjAct, &QAction::triggered, this, &MainWindow::newPrj);
	fileMenu->addAction(newPrjAct);

	const QIcon newFileIcon = QIcon(":/icons/new.png");
	newFileAct = new QAction(newFileIcon, tr("&New file"), this);
	newFileAct->setShortcuts(QKeySequence::New);
	newFileAct->setStatusTip(tr("Create a new file"));
	connect(newFileAct, &QAction::triggered, this, &MainWindow::newFile);
	fileMenu->addAction(newFileAct);

	QMenu *menuNew = new QMenu(this);
	menuNew->addAction(newPrjAct);
	menuNew->addAction(newFileAct);
	btnNew->setPopupMode(QToolButton::ToolButtonPopupMode::DelayedPopup);
	btnNew->setMenu(menuNew);
	btnNew->setDefaultAction(newPrjAct);
	fileToolBar->addWidget(btnNew);

	const QIcon openPrjIcon = QIcon(":/icons/open_prj.png");
	QAction *openPrjAct = new QAction(openPrjIcon, tr("&Open Project/Project Collection..."), this);
	openPrjAct->setShortcuts(QKeySequence::Open);
	openPrjAct->setStatusTip(tr("Open an existing Project/Project Collection"));
	connect(openPrjAct, &QAction::triggered, this, [this] { openPrj(); });
	fileMenu->addAction(openPrjAct);

	const QIcon openFileIcon = QIcon(":/icons/open.png");
	QAction *openFileAct = new QAction(openFileIcon, tr("&Open..."), this);
	openFileAct->setShortcuts(QKeySequence::Open);
	openFileAct->setStatusTip(tr("Open an existing file"));
	connect(openFileAct, &QAction::triggered, this, &MainWindow::open);
	fileMenu->addAction(openFileAct);

	QMenu *menuOpen = new QMenu(this);
	menuOpen->addAction(openPrjAct);
	menuOpen->addAction(openFileAct);
	btnOpen->setPopupMode(QToolButton::ToolButtonPopupMode::DelayedPopup);
	btnOpen->setMenu(menuOpen);
	btnOpen->setDefaultAction(openPrjAct);
	fileToolBar->addWidget(btnOpen);

	const QIcon saveIcon = QIcon(":/icons/save.png");
	saveAct = new QAction(saveIcon, tr("&Save"), this);
	saveAct->setShortcuts(QKeySequence::Save);
	saveAct->setStatusTip(tr("Save the document to disk"));
	connect(saveAct, &QAction::triggered, this, &MainWindow::save);
	fileToolBar->addAction(saveAct);
	fileMenu->addAction(saveAct);

	const QIcon saveAsIcon = QIcon(":/icons/save_as.png");
	saveAsAct = new QAction(saveAsIcon, tr("Save &As..."), this);
	saveAsAct->setShortcut(QKeySequence("Ctrl+Shift+S"));
	saveAsAct->setStatusTip(tr("Save the document under a new name"));
	connect(saveAsAct, &QAction::triggered, this, &MainWindow::saveAs);
	fileToolBar->addAction(saveAsAct);
	fileMenu->addAction(saveAsAct);

	const QIcon saveAllIcon = QIcon(":/icons/save_all.png");
	saveAllAct = new QAction(saveAllIcon, tr("&Save All"), this);
	saveAllAct->setShortcut(QKeySequence("Alt+Shift+S"));
	saveAllAct->setStatusTip(tr("Save all documents to disk"));
	connect(saveAllAct, &QAction::triggered, this, &MainWindow::saveAll);
	fileToolBar->addAction(saveAllAct);
	fileMenu->addAction(saveAllAct);

	fileMenu->addSeparator();
	//const QIcon saveAsIcon = QIcon(":/icons/save_as.png");
	closePrjCollAct = new QAction(tr("Close project collection"), this);
	//saveAsAct->setShortcut(QKeySequence("Ctrl+Shift+S"));
	closePrjCollAct->setStatusTip(tr("Close the current project collection"));
	connect(closePrjCollAct, &QAction::triggered, this, &MainWindow::closePrjCollection);
	//fileToolBar->addAction(savclosePrjCollActAsAct);
	fileMenu->addAction(closePrjCollAct);
	fileMenu->addSeparator();

	QMenu *recentMenu = fileMenu->addMenu(tr("Recent..."));
	connect(recentMenu, &QMenu::aboutToShow, this, &MainWindow::updateRecentFileActions);
	recentFileSubMenuAct = recentMenu->menuAction();

	QMenu *recentPrjsMenu = fileMenu->addMenu(tr("Recent projects..."));
	connect(recentPrjsMenu, &QMenu::aboutToShow, this, &MainWindow::updateRecentProjectsActions);
	recentProjectsSubMenuAct = recentPrjsMenu->menuAction();

	for (int i = 0; i < MaxRecentFiles; ++i) {
		recentFileActs[i] = recentMenu->addAction(QString(), this, &MainWindow::openRecentFile);
		recentFileActs[i]->setVisible(false);
	}

	for (int i = 0; i < MaxRecentProjects; ++i) {
		recentProjectsActs[i] = recentPrjsMenu->addAction(QString(), this, &MainWindow::openRecentProject);
		recentProjectsActs[i]->setVisible(false);
	}

	recentFileSeparator = fileMenu->addSeparator();
	setRecentFilesVisible(MainWindow::hasRecentFiles());

	recentProjectsSeparator = fileMenu->addSeparator();
	setRecentProjectsVisible(MainWindow::hasRecentProjects());

	fileMenu->addAction(tr("Switch layout direction"), this, &MainWindow::switchLayoutDirection);

	fileMenu->addSeparator();

	const QIcon editSettingsIcon = QIcon(":/icons/settings.png");
	settingsAct = new QAction(editSettingsIcon, tr("&Manage Settings..."), this);
	settingsAct->setShortcuts(QKeySequence::Open);
	settingsAct->setStatusTip(tr("Manage Settings"));
	connect(settingsAct, &QAction::triggered, this, &MainWindow::editSettings);
	fileMenu->addAction(settingsAct);
	fileToolBar->addAction(settingsAct);


	fileMenu->addSeparator();

	//! [0]
	const QIcon exitIcon = QIcon(":/icons/exit.png");
	QAction *exitAct = fileMenu->addAction(exitIcon, tr("E&xit"), qApp, &QApplication::closeAllWindows);
	exitAct->setShortcuts(QKeySequence::Quit);
	exitAct->setStatusTip(tr("Exit the application"));
	fileMenu->addAction(exitAct);
	//! [0]

	QMenu *editMenu = mainMenuBar->addMenu(tr("&Edit"));
	QToolBar *editToolBar = addToolBar(tr("Edit"));
	editToolBar->setStyleSheet("QToolButton{margin:0px;spacing:0px;padding:0px;}");

#ifndef QT_NO_CLIPBOARD
	const QIcon cutIcon = QIcon(":/icons/cut.png");
	cutAct = new QAction(cutIcon, tr("Cu&t"), this);
	cutAct->setShortcuts(QKeySequence::Cut);
	cutAct->setStatusTip(tr("Cut the current selection's contents to the "
		"clipboard"));
	connect(cutAct, &QAction::triggered, this, &MainWindow::cut);
	editMenu->addAction(cutAct);
	editToolBar->addAction(cutAct);

	const QIcon copyIcon = QIcon(":/icons/copy.png");
	copyAct = new QAction(copyIcon, tr("&Copy"), this);
	copyAct->setShortcuts(QKeySequence::Copy);
	copyAct->setStatusTip(tr("Copy the current selection's contents to the "
		"clipboard"));
	connect(copyAct, &QAction::triggered, this, &MainWindow::copy);
	editMenu->addAction(copyAct);
	editToolBar->addAction(copyAct);

	const QIcon pasteIcon = QIcon(":/icons/paste.png");
	pasteAct = new QAction(pasteIcon, tr("&Paste"), this);
	pasteAct->setShortcuts(QKeySequence::Paste);
	pasteAct->setStatusTip(tr("Paste the clipboard's contents into the current "
		"selection"));
	connect(pasteAct, &QAction::triggered, this, &MainWindow::paste);
	editMenu->addAction(pasteAct);
	editToolBar->addAction(pasteAct);

	editMenu->addSeparator();
#endif

	const QIcon showScIcon = QIcon(":/icons/pilcrow.png");
	showSpecialCharsAct = new QAction(showScIcon, tr("Show special characters"), this);
	showSpecialCharsAct->setStatusTip(tr("Show special characters"));
	showSpecialCharsAct->setCheckable(true);
	connect(showSpecialCharsAct, &QAction::triggered, this, &MainWindow::showSpecialChars);
	editMenu->addAction(showSpecialCharsAct);
	editToolBar->addAction(showSpecialCharsAct);

	editMenu->addSeparator();

	//const QIcon showScIcon = QIcon(":/icons/pilcrow.png");
	submenuEolOps = editMenu->addMenu(tr("EOL operations"));
	eol_win_act = submenuEolOps->addAction("Windows (CR+LF)"); 
	eol_win_act->setData((int)EolMode::Windows);
	eol_linux_act = submenuEolOps->addAction("Linux/Unix (LF)");
	eol_linux_act->setData((int)EolMode::Unix);
	eol_mac_act = submenuEolOps->addAction("Macintosh Classic (CR)");
	eol_mac_act->setData((int)EolMode::ClassicMacOS);
	connect(submenuEolOps, &QMenu::aboutToShow, this, &MainWindow::setupEolMenu);
	connect(eol_win_act, &QAction::triggered, this, [this] { convertFileEol(EolMode::Windows); });
	connect(eol_linux_act, &QAction::triggered, this, [this] { convertFileEol(EolMode::Unix); });
	connect(eol_mac_act, &QAction::triggered, this, [this] { convertFileEol(EolMode::ClassicMacOS); });

	// Search
	QMenu* searchMenu = mainMenuBar->addMenu(tr("&Search"));
	QToolBar* searchToolBar = addToolBar(tr("Search"));
	searchToolBar->setStyleSheet("QToolButton{margin:0px;spacing:0px;padding:0px;}");

	findAct = new QAction(QIcon(":/icons/find.png"), tr("Find..."), this);
	findAct->setShortcut(QKeySequence("Ctrl+F"));
	findAct->setStatusTip(tr("Find"));
	connect(findAct, &QAction::triggered, this, [this]() { openSearch(SearchType::Find); });
	searchMenu->addAction(findAct);
	searchToolBar->addAction(findAct);

	findInFilesAct = new QAction(tr("Find in files..."), this);
	findInFilesAct->setShortcut(QKeySequence("Ctrl+Shift+F"));
	findInFilesAct->setStatusTip(tr("Find in files"));
	//connect(cutAct, &QAction::triggered, this, &MainWindow::cut);
	searchMenu->addAction(findInFilesAct);
	//searchToolBar->addAction(findInFilesAct);

	findPrevAct = new QAction(QIcon(":/icons/find_prev.png"), tr("Find Previous"), this);
	findPrevAct->setShortcut(QKeySequence("Shift+F3"));
	findPrevAct->setStatusTip(tr("Find Previous"));
	//connect(cutAct, &QAction::triggered, this, &MainWindow::cut);
	searchMenu->addAction(findPrevAct);
	searchToolBar->addAction(findPrevAct);

	findNextAct = new QAction(QIcon(":/icons/find_next.png"), tr("Find Next"), this);
	findNextAct->setShortcut(QKeySequence("F3"));
	findNextAct->setStatusTip(tr("Find Next"));
	//connect(cutAct, &QAction::triggered, this, &MainWindow::cut);
	searchMenu->addAction(findNextAct);
	searchToolBar->addAction(findNextAct);

	replaceAct = new QAction(QIcon(":/icons/replace.png"), tr("Replace..."), this);
	replaceAct->setShortcut(QKeySequence("Ctrl+H"));
	replaceAct->setStatusTip(tr("Replace"));
	connect(replaceAct, &QAction::triggered, this, [this]() { openSearch(SearchType::Replace); });
	searchMenu->addAction(replaceAct);
	searchToolBar->addAction(replaceAct);

	searchMenu->addSeparator();

	QToolBar* bookmarkToolBar = addToolBar(tr("Bookmarks"));
	bookmarkToolBar->setStyleSheet("QToolButton{margin:0px;spacing:0px;padding:0px;}");

	bookmarkToggleAct = new QAction(QIcon(":/icons/book.png"), tr("Toggle Bookmark"), this);
	bookmarkToggleAct->setShortcut(QKeySequence("Ctrl+F2"));
	bookmarkToggleAct->setStatusTip(tr("Toggle Bookmark"));
	connect(bookmarkToggleAct, &QAction::triggered, this, &MainWindow::bookmarkToggle);
	searchMenu->addAction(bookmarkToggleAct);
	bookmarkToolBar->addAction(bookmarkToggleAct);

	bookmarkPrevAct = new QAction(QIcon(":/icons/book_previous.png"), tr("Previous Bookmark"), this);
	bookmarkPrevAct->setShortcut(QKeySequence("Shift+F2"));
	bookmarkPrevAct->setStatusTip(tr("Previous Bookmark"));
	connect(bookmarkPrevAct, &QAction::triggered, this, &MainWindow::bookmarkPrev);
	searchMenu->addAction(bookmarkPrevAct);
	bookmarkToolBar->addAction(bookmarkPrevAct);

	bookmarkNextAct = new QAction(QIcon(":/icons/book_next.png"), tr("Next Bookmark"), this);
	bookmarkNextAct->setShortcut(QKeySequence("F2"));
	bookmarkNextAct->setStatusTip(tr("Next Bookmark"));
	connect(bookmarkNextAct, &QAction::triggered, this, &MainWindow::bookmarkNext);
	searchMenu->addAction(bookmarkNextAct);
	bookmarkToolBar->addAction(bookmarkNextAct);

	bookmarkClearAllAct = new QAction(QIcon(":/icons/book_open.png"), tr("Clear All Bookmarks"), this);
	bookmarkClearAllAct->setShortcut(QKeySequence("Alt+F2"));
	bookmarkClearAllAct->setStatusTip(tr("Clear All Bookmarks"));
	connect(bookmarkClearAllAct, &QAction::triggered, this, &MainWindow::bookmarkClearAll);
	searchMenu->addAction(bookmarkClearAllAct);
	bookmarkToolBar->addAction(bookmarkClearAllAct);

	// Search (end)
	cbConfiguration = new QComboBox(this);
	cbConfiguration->addItem("Release", "release");
	cbConfiguration->addItem("Debug", "debug");
	cbConfiguration->setCurrentIndex(DEFAULT_TARGET_CONFIG);	// 0 = Release, 1 = Debug
	buildToolBar->addWidget(cbConfiguration);

	connect(cbConfiguration, qOverload<int>(&QComboBox::currentIndexChanged), this, [this](int i) {
		if (Ide::TaskManager()->getStatus() != IdeStatus::LoadingOrSaving)
			Ide::TaskManager()->saveCurrentProjectCollectionState();
	});

	cbPlatform = new QComboBox(this);

	setAvailablePlatformsForConfiguration();

	connect(cbPlatform, qOverload<int>(&QComboBox::currentIndexChanged), this, [this](int i) {
		if (Ide::TaskManager()->getStatus() != IdeStatus::LoadingOrSaving)
			Ide::TaskManager()->saveCurrentProjectCollectionState();
	});

	buildToolBar->addWidget(cbPlatform);

	QMenu *buildMenu = mainMenuBar->addMenu(tr("Build"));

	const QIcon bulldAllIcon = QIcon(":/icons/build.png");
	buildAllAct = new QAction(bulldAllIcon, tr("&Build"), this);
	//buildAct->setShortcuts(QKeySequence::Save);
	buildAllAct->setStatusTip(tr("Build"));
	connect(buildAllAct, &QAction::triggered, this, &MainWindow::buildAll);
	buildToolBar->addAction(buildAllAct);
	buildMenu->addAction(buildAllAct);

	const QIcon bulldCleanIcon = QIcon(":/icons/build_clean.png");
	buildCleanAct = new QAction(bulldCleanIcon, tr("&Clean"), this);
	//buildAct->setShortcuts(QKeySequence::Save);
	buildCleanAct->setStatusTip(tr("Clean"));
	connect(buildCleanAct, &QAction::triggered, this, &MainWindow::buildClean);
	buildToolBar->addAction(buildCleanAct);
	buildMenu->addAction(buildCleanAct);

	const QIcon bulldStopIcon = QIcon(":/icons/build_stop.png");
	buildStopAct = new QAction(bulldStopIcon, tr("&Stop"), this);
	//buildAct->setShortcuts(QKeySequence::Save);
	buildStopAct->setStatusTip(tr("Stop build"));
	connect(buildStopAct, &QAction::triggered, this, &MainWindow::buildStop);
	buildToolBar->addAction(buildStopAct);
	buildMenu->addAction(buildStopAct);

	QMenu *debugMenu = mainMenuBar->addMenu(tr("Run/Debug"));

	const QIcon runIcon = QIcon(":/icons/run.png");
	runAct = new QAction(runIcon, tr("&Run"), this);
	runAct->setShortcut(QKeySequence("Shift+F5"));
	runAct->setStatusTip(tr("Run"));
	connect(runAct, &QAction::triggered, this, [this]() {
		this->run(false);
	});
	runDebugToolBar->addAction(runAct);
	debugMenu->addAction(runAct);

	runDetachedAct = new QAction(tr("Run (detached)"), this);
	runDetachedAct->setShortcut(QKeySequence("Ctrl+Shift+F5"));
	runDetachedAct->setStatusTip(tr("Run (detached)"));
	connect(runDetachedAct, &QAction::triggered, this, [this]() {
		this->run(true);
	});
	debugMenu->addAction(runDetachedAct);

	const QIcon debugIcon = QIcon(":/icons/debug.png");
	debugAct = new QAction(debugIcon, tr("&Debug"), this);
	debugAct->setShortcut(QKeySequence(Qt::Key::Key_F5));
	debugAct->setStatusTip(tr("Debug"));
	connect(debugAct, &QAction::triggered, this, &MainWindow::debug);
	runDebugToolBar->addAction(debugAct);
	debugMenu->addAction(debugAct);

	debugMenu->addSeparator();

	const QIcon stopDebugIcon = QIcon(":/icons/debug_stop.png");
	debugStopAct = new QAction(stopDebugIcon, tr("Stop Debug"), this);
	debugStopAct->setShortcut(QKeySequence("Ctrl+F5"));
	debugStopAct->setStatusTip(tr("Stop debug"));
	connect(debugStopAct, &QAction::triggered, this, &MainWindow::debug_stop);
	runDebugToolBar->addAction(debugStopAct);
	debugMenu->addAction(debugStopAct);

	const QIcon stepDebugIcon = QIcon(":/icons/step.png");
	debugStepAct = new QAction(stepDebugIcon, tr("&Step"), this);
	debugStepAct->setShortcut(QKeySequence(Qt::Key::Key_F10));
	debugStepAct->setStatusTip(tr("Step"));
	connect(debugStepAct, &QAction::triggered, this, &MainWindow::debug_step);
	runDebugToolBar->addAction(debugStepAct);
	debugMenu->addAction(debugStepAct);

	const QIcon goDebugIcon = QIcon(":/icons/go.png");
	debugContinueAct = new QAction(goDebugIcon, tr("&Go"), this);
	debugContinueAct->setShortcut(QKeySequence(Qt::Key::Key_F8));
	debugContinueAct->setStatusTip(tr("Go"));
	connect(debugContinueAct, &QAction::triggered, this, &MainWindow::debug_continue);
	runDebugToolBar->addAction(debugContinueAct);
	debugMenu->addAction(debugContinueAct);

	debugMenu->addSeparator();

	const QIcon toggleBreakpointIcon = QIcon(":/icons/stop.png");
	toggleBreakpointAct = new QAction(goDebugIcon, tr("Toggle Breakpoint"), this);
	toggleBreakpointAct->setShortcut(QKeySequence(Qt::Key::Key_F9));
	toggleBreakpointAct->setStatusTip(tr("Toggle Breakpoint"));
	connect(toggleBreakpointAct, &QAction::triggered, this, &MainWindow::breakpoint_toggle);
	runDebugToolBar->addAction(toggleBreakpointAct);
	debugMenu->addAction(toggleBreakpointAct);

	windowMenu = mainMenuBar->addMenu(tr("&Windows"));
	connect(windowMenu, &QMenu::aboutToShow, this, &MainWindow::updateWindowMenu);

	closeAct = new QAction(tr("Cl&ose"), this);
	closeAct->setStatusTip(tr("Close the active window"));
	connect(closeAct, &QAction::triggered,
		mdiArea, &QMdiArea::closeActiveSubWindow);

	closeAllAct = new QAction(tr("Close &All"), this);
	closeAllAct->setStatusTip(tr("Close all the windows"));
	connect(closeAllAct, &QAction::triggered, mdiArea, &QMdiArea::closeAllSubWindows);

	tileAct = new QAction(tr("&Tile"), this);
	tileAct->setStatusTip(tr("Tile the windows"));
	connect(tileAct, &QAction::triggered, mdiArea, &QMdiArea::tileSubWindows);

	cascadeAct = new QAction(tr("&Cascade"), this);
	cascadeAct->setStatusTip(tr("Cascade the windows"));
	connect(cascadeAct, &QAction::triggered, mdiArea, &QMdiArea::cascadeSubWindows);

	nextAct = new QAction(tr("Ne&xt"), this);
	nextAct->setShortcuts(QKeySequence::NextChild);
	nextAct->setStatusTip(tr("Move the focus to the next window"));
	connect(nextAct, &QAction::triggered, mdiArea, &QMdiArea::activateNextSubWindow);

	previousAct = new QAction(tr("Pre&vious"), this);
	previousAct->setShortcuts(QKeySequence::PreviousChild);
	previousAct->setStatusTip(tr("Move the focus to the previous "
		"window"));
	connect(previousAct, &QAction::triggered, mdiArea, &QMdiArea::activatePreviousSubWindow);

	windowMenuSeparatorAct = new QAction(this);
	windowMenuSeparatorAct->setSeparator(true);

	updateWindowMenu();

	mainMenuBar->addSeparator();

	QMenu *helpMenu = mainMenuBar->addMenu(tr("&Help"));

	QAction *aboutAct = helpMenu->addAction(tr("&About"), this, &MainWindow::about);
	aboutAct->setStatusTip(tr("Show the application's About box"));

	QAction *aboutQtAct = helpMenu->addAction(tr("About &Qt"), qApp, &QApplication::aboutQt);
	aboutQtAct->setStatusTip(tr("Show the Qt library's About box"));

	connect(Ide::TaskManager(), &IdeTaskManager::IdeStatusChanged, this, [this](IdeStatus s) { 
		IdeStatusChanged(s); 
	});

	connect(Ide::TaskManager(), &IdeTaskManager::IdeStatusChanged, this, [this](IdeStatus s) { 
		watch_window->IdeStatusChanged(s); 
	});
	connect(Ide::TaskManager(), &IdeTaskManager::IdeDebuggerBreak, this, [this] {
		QApplication::alert(this);
		watch_window->onDebuggerBreak(); 
	});

	connect(Ide::TaskManager(), &IdeTaskManager::IdeStatusChanged, this, [this](IdeStatus s) { 
		working_storage_window->IdeStatusChanged(s); 
	});
}

void MainWindow::setAvailablePlatformsForConfiguration()
{
	cbPlatform->clear();

	QStringList available_platforms = this->getPlatformsForConfiguration(cbConfiguration->itemData(DEFAULT_TARGET_CONFIG).toString());
	for (QString p : available_platforms)
		cbPlatform->addItem(p, p);
}

void MainWindow::createStatusBar()
{
	statusBar()->showMessage(tr("Ready"));
}

void MainWindow::readSettings()
{
    QSettings settings(QCoreApplication::organizationName(), QCoreApplication::applicationName());
    const QByteArray geometry = settings.value("geometry", QByteArray()).toByteArray();
    if (geometry.isEmpty()) {
		const QRect availableGeometry = QApplication::desktop()->availableGeometry(this);
		resize(availableGeometry.width(), availableGeometry.height());
		showMaximized();
    }
    else {
        restoreGeometry(geometry);
    }
}

void MainWindow::writeSettings()
{
	QSettings settings(QCoreApplication::organizationName(), QCoreApplication::applicationName());
	settings.setValue("geometry", saveGeometry());
}

void MainWindow::removeAllDebugMarkers()
{
	foreach(QMdiSubWindow * window, mdiArea->subWindowList()) {
		MdiChild* mdiChild = qobject_cast<MdiChild*>(window->widget());
		mdiChild->removeDebugMarkers();
	}
}

MdiChild *MainWindow::activeMdiChild() const
{
	if (QMdiSubWindow *activeSubWindow = mdiArea->activeSubWindow())
		return qobject_cast<MdiChild *>(activeSubWindow->widget());
	return 0;
}

void MainWindow::blockMdiSignals(bool f)
{
	mdiArea->blockSignals(f);
}

PropertyWindow *MainWindow::getPropertyWindow()
{
	return property_window;
}

void MainWindow::openSearch(SearchType search_type)
{
	QString search_spec = "";
	search_dlg = new SearchDialog(this); 
	//MdiChild* cur_win = activeMdiChild();
	//if (cur_win) {
	//	search_spec = cur_win->selectedText();
	//}
	search_dlg->setup(search_type);
	search_dlg->show();


}

QStringList MainWindow::getPlatformsForConfiguration(QString config)
{
	QSettings settings;
	QString key = (config.toLower() == "debug") ? "DebugCompilerId": "ReleaseCompilerId";
	QString compiler_id = settings.value(key).toString();
	QMap<QString, CompilerDefinition *> compilers = GixGlobals::getCompilerManager()->getCompilers();
	if (!compilers.contains(compiler_id))
		return QStringList() << "x86" << "x64" << "ARM";

	CompilerDefinition* c = compilers[compiler_id];
	return c->getTargetPlatforms().keys();
}

QMdiSubWindow *MainWindow::findMdiChild(const QString &fileName) const
{
	QString canonicalFilePath = QFileInfo(fileName).canonicalFilePath();

	foreach(QMdiSubWindow *window, mdiArea->subWindowList()) {
		MdiChild *mdiChild = qobject_cast<MdiChild *>(window->widget());
		if (mdiChild) {
			if (mdiChild->currentFile() == canonicalFilePath)
				return window;
		}
	}
	return 0;
}

void MainWindow::switchLayoutDirection()
{
	if (layoutDirection() == Qt::LeftToRight)
		QGuiApplication::setLayoutDirection(Qt::RightToLeft);
	else
		QGuiApplication::setLayoutDirection(Qt::LeftToRight);
}

void MainWindow::updateClipboardActions(MdiChild *c, int p)
{
	if (p & SC_UPDATE_SELECTION) {
		cutAct->setEnabled(c->getSelText().size() > 0);
		copyAct->setEnabled(c->getSelText().size() > 0);
	}
}

void MainWindow::bookmarkToggle()
{
	MdiChild* child = activeMdiChild();
	if (!child)
		return;

	child->toggleBookmark();
}

void MainWindow::bookmarkPrev()
{
	QString bm = Ide::TaskManager()->getPrevBookmark();
	if (bm.isEmpty())
		return;

	int lpos = bm.lastIndexOf(":");
	if (lpos == -1 || lpos >= (bm.size() - 1))
		return;

	int ln = bm.mid(lpos + 1).toInt();
	if (!ln)
		return;

	QString filename = bm.mid(0, lpos);
	Ide::TaskManager()->gotoFileLine(filename, ln);
}

void MainWindow::bookmarkNext()
{
	QString bm = Ide::TaskManager()->getNextBookmark();
	if (bm.isEmpty())
		return;

	int lpos = bm.lastIndexOf(":");
	if (lpos == -1 || lpos >= (bm.size() - 1))
		return;

	int ln = bm.mid(lpos + 1).toInt();
	if (!ln)
		return;

	QString filename = bm.mid(0, lpos);
	Ide::TaskManager()->gotoFileLine(filename, ln);
}

void MainWindow::bookmarkClearAll()
{
	MdiChild* child = activeMdiChild();
	if (!child)
		return;

	child->clearAllBookmarks();
}

void MainWindow::convertFileEol(EolMode m)
{
	MdiChild* child = activeMdiChild();
	if (!child)
		return;

	child->convertEOLs((int)m);
	child->setEOLMode((int)m);

	setupEolMenu();
}
