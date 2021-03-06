#include "DependenciesWindow.h"
#include "Ide.h"
#include "IdeTaskManager.h"
#include "MetadataManager.h"
#include "UiUtils.h"
#include "PathUtils.h"

#include <QFile>
#include <QMutex>
#include <QDockWidget>

static QMutex update_lock;

DependenciesWindow::DependenciesWindow(QWidget *parent, MainWindow *mw)
{
	mainWindow = mw;
	cur_data = nullptr;
	tab_widget = new QTabWidget(this);


	this->file_dataWidget = new QTreeWidget(this);
	file_dataWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
	file_dataWidget->setColumnCount(1);
	file_dataWidget->setUniformRowHeights(true);
	file_dataWidget->setHeaderHidden(true);

	this->prj_dataWidget = new QTreeWidget(this);
	prj_dataWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
	prj_dataWidget->setColumnCount(1);
	prj_dataWidget->setUniformRowHeights(true);
	prj_dataWidget->setHeaderHidden(true);

	tab_widget->addTab(file_dataWidget, "Current File");
	tab_widget->addTab(prj_dataWidget, "Project");

	this->setCentralWidget(tab_widget);

	connect(file_dataWidget, &QTreeWidget::itemDoubleClicked, this, &DependenciesWindow::file_ItemDoubleClicked);
	connect(prj_dataWidget, &QTreeWidget::itemDoubleClicked, this, &DependenciesWindow::prj_ItemDoubleClicked);

	connect(tab_widget, &QTabWidget::currentChanged, this, &DependenciesWindow::tabChanged);

	connect(GixGlobals::getMetadataManager(), &MetadataManager::updatedModuleMetadata, this, [this](CobolModuleMetadata *cmm) {
		if (tab_widget->currentIndex() == 0)
			updateFileDependencies();
		else
			if (tab_widget->currentIndex() == 1)
				updateProjectDependencies();
	});

	connect(Ide::TaskManager(), &IdeTaskManager::fileActivated, this, [this](ProjectFile *pf) {
		if (tab_widget->currentIndex() == 0)
			updateFileDependencies();
		else
			if (tab_widget->currentIndex() == 1)
				updateProjectDependencies();
	});
}

DependenciesWindow::~DependenciesWindow()
{
	Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, "Shutting down DependenciesWindow", QLogger::LogLevel::Debug);
}

void DependenciesWindow::updateFileDependencies()
{
	this->file_dataWidget->clear();

	UiUtils::setTreeViewFont(file_dataWidget);

	if (mainWindow->activeMdiChild() != nullptr) {
		QString f = mainWindow->activeMdiChild()->currentFile();

		ProjectCollection *ppj = Ide::TaskManager()->getCurrentProjectCollection();
		if (ppj != nullptr) {
			ProjectFile *pf = ppj->locateProjectFileByPath(f, true);
			if (!pf)
				return;

			QMap<QString, QStringList> deps;
			if (!get_project_dependencies(deps))
				return;

			if (!deps.contains(pf->GetFileFullPath()))
				return;

			QDockWidget *qdw = dynamic_cast<QDockWidget *>(this->parent());
			if (qdw && qdw->isVisible()) {

				QBrush _style_not_existing(Qt::red);

				QTreeWidgetItem *fileItem = new QTreeWidgetItem(file_dataWidget, QStringList(pf->GetDisplayName()), 0);
				fileItem->setText(0, pf->GetDisplayName());
				file_dataWidget->addTopLevelItem(fileItem);

				for (QString dep_file : deps[pf->GetFileFullPath()]) {
					QTreeWidgetItem *depItem = new QTreeWidgetItem((QTreeWidget *)0, QStringList(PathUtils::getFilename(dep_file)));
					depItem->setIcon(0, QIcon(":/icons/cblfile.png"));
					depItem->setData(0, Qt::UserRole, dep_file);
					if (!QFile::exists(dep_file)) {
						depItem->setForeground(0, _style_not_existing);
					}
					fileItem->addChild(depItem);
				}
				fileItem->setExpanded(true);

			}
		}

	}

}

void DependenciesWindow::updateProjectDependencies()
{
	this->prj_dataWidget->clear();

	UiUtils::setTreeViewFont(prj_dataWidget);

	if (mainWindow->activeMdiChild() != nullptr) {
		QString f = mainWindow->activeMdiChild()->currentFile();

		ProjectCollection *ppj = Ide::TaskManager()->getCurrentProjectCollection();
		if (ppj != nullptr) {
			ProjectFile *pf = ppj->locateProjectFileByPath(f, true);
			if (!pf)
				return;

			QMap<QString, QStringList> deps;
			if (!get_project_dependencies(deps))
				return;

			QDockWidget *qdw = dynamic_cast<QDockWidget *>(this->parent());
			if (qdw && qdw->isVisible()) {

				QBrush _style_not_existing(Qt::red);

				QTreeWidgetItem *fileItem = new QTreeWidgetItem(prj_dataWidget, QStringList(pf->getParentProject()->GetDisplayName()), 0);
				prj_dataWidget->addTopLevelItem(fileItem);

				for (QString cbl_file : deps.keys()) {
					for (QString dep_file : deps[cbl_file]) {
						QTreeWidgetItem *depItem = new QTreeWidgetItem((QTreeWidget *)0, QStringList(PathUtils::getFilename(dep_file)));
						depItem->setIcon(0, QIcon(":/icons/cblfile.png"));
						depItem->setData(0, Qt::UserRole, dep_file);
						if (!QFile::exists(dep_file)) {
							depItem->setForeground(0, _style_not_existing);
						}
						fileItem->addChild(depItem);
					}
					fileItem->setExpanded(true);
				}
			}
		}
	}
}

void DependenciesWindow::file_ItemDoubleClicked(QTreeWidgetItem *item, int column)
{
	QVariant q = item->data(0, Qt::UserRole);
	QString f = q.toString();
	if (QFile::exists(f))
		mainWindow->openFile(f);

}

void DependenciesWindow::prj_ItemDoubleClicked(QTreeWidgetItem *item, int column)
{
	QVariant q = item->data(0, Qt::UserRole);
	ProjectItem *p = static_cast<ProjectItem *>(q.value<void *>());

	switch (p->GetItemType()) {
		case ProjectItemType::TFile:
			mainWindow->openFile(p->GetFileFullPath());
			break;
	}
}

void DependenciesWindow::tabChanged(int idx)
{
	if (idx == 0) {
		updateFileDependencies();
	}

	if (idx == 1) {
		updateProjectDependencies();
	}
}

bool DependenciesWindow::get_project_dependencies(QMap<QString, QStringList> &deps)
{
	QMap<QString, QStringList> res;

	if (!mainWindow->activeMdiChild())
		return false;

	QString f = mainWindow->activeMdiChild()->currentFile();

	ProjectCollection *ppj = Ide::TaskManager()->getCurrentProjectCollection();
	if (!ppj)
		return false;

	ProjectFile *pf = ppj->locateProjectFileByPath(f, true);
	if (!pf)
		return false;

	Project *prj = pf->getParentProject();
	if (!prj)
		return false;

	QList<ProjectFile *> cfiles = prj->getAllCompilableFiles();
	for (ProjectFile *pf : cfiles) {
		QString ff = pf->GetFileFullPath();
		CobolModuleMetadata *cmm = GixGlobals::getMetadataManager()->getModuleMetadataBySource(pf->GetFileFullPath());
		if (!cmm)
			continue;

		QMap<QString, QStringList> fdeps = cmm->getFileDependencies();
		for (auto it = fdeps.begin(); it != fdeps.end(); ++it) {
			res.insert(it.key(), it.value());
		}
	}

	deps = res;

	return true;
}