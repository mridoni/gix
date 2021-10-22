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

#include "DataWindow.h"
#include "DataEntry.h"
#include "UiUtils.h"
#include "PathUtils.h"
#include "Ide.h"
#include "IdeTaskManager.h"
#include "MetadataManager.h"
#include "MdiChild.h"
#include "MainWindow.h"

#include <QToolBar>
#include <QDockWidget>
#include <QLabel>
#include <QLineEdit>
#include <QComboBox>
#include <QHeaderView>
#include <QBoxLayout>
#include <QPushButton>
#include <QInputDialog>
#include <QMdiSubWindow>
#include <QDir>
#include <QStandardItemModel>
#include <QMenu>

#define DEFAULT_NODE_EXPANDED false

DataWindow::DataWindow(QWidget *parent, MainWindow *mw) : QMainWindow(parent)
{
	//this->cur_file = nullptr;

	this->setWindowTitle("Data");
	this->setMinimumWidth(250);
	this->setWindowFlags(Qt::Widget);
	QToolBar* toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

	bRefresh = new QPushButton(this);
	bRefresh->setFixedSize(16, 16);
	const QIcon refreshIcon = QIcon(":/icons/bullet_refresh.png");
	bRefresh->setIcon(refreshIcon);
	toolBar->addWidget(bRefresh);

	this->dataWidget = new DragDropTreeWidget(true, false);
	this->setCentralWidget(dataWidget);
	dataWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
	dataWidget->setColumnCount(2);
	dataWidget->setUniformRowHeights(true);
	dataWidget->setHeaderLabels(QStringList() << tr("Name") << tr("Format"));

	connect(dataWidget, &QTreeWidget::itemDoubleClicked, this, &DataWindow::dataItemDoubleClicked);

	dataWidget->setContextMenuPolicy(Qt::CustomContextMenu);
	connect(dataWidget, &QTreeWidget::customContextMenuRequested, this, &DataWindow::prepareMenu);
	connect(GixGlobals::getMetadataManager(), &MetadataManager::updatedModuleMetadata, this, [this](CobolModuleMetadata *cmm) {
		refreshContent(true); 
	});

	connect(GixGlobals::getMetadataManager(), &MetadataManager::updatedModuleMetadataBatch, this, [this](bool b) {
		refreshContent(true);
	});

	connect(Ide::TaskManager(), &IdeTaskManager::fileActivated, this, [this](ProjectFile *pf) {
		refreshContent(false);
	});

	connect(dataWidget, &QTreeWidget::itemExpanded, this, [this](QTreeWidgetItem *item) { 
		dataWidget->resizeColumnToContents(0); 
		QVariant v = item->data(0, Qt::UserRole);
		QString the_file = QDir::cleanPath(mainWindow->activeMdiChild()->currentFile());
		if (v.isValid()) {
			if (v.userType() != QMetaType::QString) {
				DataEntry* e = v.value<DataEntry *>();
				if (e)
					Ide::TaskManager()->setIdeElementInfo(the_file + ":" + e->path, 1);
			}
			else
				Ide::TaskManager()->setIdeElementInfo(the_file + ":" + v.toString(), 1);
		}
	});

	connect(dataWidget, &QTreeWidget::itemCollapsed, this, [this](QTreeWidgetItem* item) {
		dataWidget->resizeColumnToContents(0); 
		QVariant v = item->data(0, Qt::UserRole);
		QString the_file = QDir::cleanPath(mainWindow->activeMdiChild()->currentFile());
		if (v.isValid()) {
			if (v.userType() != QMetaType::QString) {
				DataEntry* e = v.value<DataEntry *>();
				if (e)
					Ide::TaskManager()->setIdeElementInfo(the_file + ":" + e->path, 0);
			}
			else
				Ide::TaskManager()->setIdeElementInfo(the_file + ":" + v.toString(), 0);
		}
	});

	connect(GixGlobals::getMetadataManager(), &MetadataManager::invalidateModuleMetadata, this, [this](QString program_id, ProjectFile *pf) {
		//if (pf == cur_file) {
		//	dataWidget->clear();
		//}

	}, Qt::ConnectionType::QueuedConnection);
}


DataWindow::~DataWindow()
{
}

void DataWindow::refreshContent(bool force_refresh)
{
	QString f = mainWindow->activeMdiChild()->currentFile();

	dataWidget->clear();

	Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("DataWindow is refreshing"), QLogger::LogLevel::Debug);
	if (mainWindow->activeMdiChild() != nullptr) {

		ProjectCollection* ppj = Ide::TaskManager()->getCurrentProjectCollection();
		if (ppj != nullptr) {
			ProjectFile* pf = ppj->locateProjectFileByPath(f);
			if (pf != nullptr) {
				QDockWidget *qdw = dynamic_cast<QDockWidget *>(this->parent());
				if (qdw && qdw->isVisible()) {
					this->setContent(pf);
				}
			}
		}
	}
	else
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, "no window", QLogger::LogLevel::Trace);
}

void DataWindow::setContent(ProjectFile *pf)
{

	QString lstPath, module_name, output_path;

	QString configuration = Ide::TaskManager()->getCurrentConfiguration();
	QString platform = Ide::TaskManager()->getCurrentPlatform();

	if (pf == nullptr || pf->PropertyGetValue("build_action") != "compile" || !pf->getOutputModuleAndFile(configuration, platform, module_name, output_path)) {
		refresh_data_items(pf->GetFileFullPath());
		return;
	}

	CobolModuleMetadata* cfm = GixGlobals::getMetadataManager()->getModuleMetadataBySource(pf->GetFileFullPath());
	refresh_data_items(pf->GetFileFullPath());
}

void DataWindow::IdeStatusChanged(IdeStatus s)
{

}

void DataWindow::onDebuggerBreak()
{

}

void DataWindow::refresh_data_items(QString filepath)
{
	dataWidget->clear();

	//if (!cur_file)
	//	return;

	CobolModuleMetadata *cur_data = GixGlobals::getMetadataManager()->getModuleMetadataBySource(filepath);
	if (cur_data == nullptr)
		return;

	QTreeWidgetItem *li = new QTreeWidgetItem(dataWidget, QStringList("Linkage section"));
	QTreeWidgetItem *wi = new QTreeWidgetItem(dataWidget, QStringList("Working storage"));
	QTreeWidgetItem *fi = new QTreeWidgetItem(dataWidget, QStringList("File section"));

	li->setData(0, Qt::UserRole, "LS");
	wi->setData(0, Qt::UserRole, "WS");
	fi->setData(0, Qt::UserRole, "DS");

	QList<DataEntry *> entries = cur_data->getLinkageDataEntries();
	for (int i = 0; i < entries.size(); i++) {
		DataEntry *e = entries.at(i);
		append_children(e, li);
	}

	entries = cur_data->getWorkingStorageDataEntries();
	for (int i = 0; i < entries.size(); i++) {
		DataEntry *e = entries.at(i);
		append_children(e, wi);
	}

	entries = cur_data->getFileDataEntries();
	for (int i = 0; i < entries.size(); i++) {
		DataEntry *e = entries.at(i);
		append_children(e, fi);
	}

	setNodeStatus("LS", li);
	setNodeStatus("WS", wi);
	setNodeStatus("DS", fi);

	dataWidget->resizeColumnToContents(0);
}

void DataWindow::append_children(DataEntry *e, QTreeWidgetItem *parent_item)
{
	QTreeWidgetItem *si = new QTreeWidgetItem(QStringList() << e->name << e->format);
	si->setData(0, Qt::UserRole, QVariant::fromValue<DataEntry *>(e));

	parent_item->addChild(si);

	QString the_file = QDir::cleanPath(mainWindow->activeMdiChild()->currentFile());
	setNodeStatus(e->path, si);

	for (int i = 0; i < e->children.size(); i++) {
		append_children(e->children.at(i), si);
	}
}

void DataWindow::setNodeStatus(QString node_path, QTreeWidgetItem* si)
{
	QString the_file = QDir::cleanPath(mainWindow->activeMdiChild()->currentFile());
	int st = Ide::TaskManager()->getIdeElementInfo(the_file + ":" + node_path).toInt();
	if (st >= 0)
		si->setExpanded(st);
	else
		si->setExpanded(DEFAULT_NODE_EXPANDED);
}

void DataWindow::dataItemDoubleClicked(QTreeWidgetItem * item, int column)
{
	QVariant v = item->data(0, Qt::UserRole);
	if (v.isValid()) {
		DataEntry* e = v.value<DataEntry *>();
		Ide::TaskManager()->gotoDefinition(e);
	}
}

void DataWindow::prepareMenu(const QPoint & pos)
{
	QTreeWidget *tree = dataWidget;
	QTreeWidgetItem *item = tree->itemAt(pos);
	QVariant q = item->data(0, Qt::UserRole);
	DataEntry *e = (DataEntry *)(q.value<DataEntry *>());
	if (!e)
		return;

	QMenu *menu = new QMenu(this);

	QAction *goToDefinitionAct = new QAction(QIcon(":/icons/book_go.png"), tr("Go to definition"), this);
	goToDefinitionAct->setStatusTip(tr("Go to definition"));
	connect(goToDefinitionAct, &QAction::triggered, this, [this, e] { Ide::TaskManager()->gotoDefinition(e); });
	menu->addAction(goToDefinitionAct);

	if (Ide::TaskManager()->getStatus() == IdeStatus::DebuggingOnBreak) {
		menu->addSeparator();

		QAction *addToWatchAct = new QAction(QIcon(":/icons/comment_add.png"), tr("Add to watch window"), this);
		addToWatchAct->setStatusTip(tr("Add to watch window"));
		connect(addToWatchAct, &QAction::triggered, this, [e] { 
			Ide::TaskManager()->getDebugManager()->addWatchedVar(e->name); 
			Ide::TaskManager()->refreshWatchWindow();
		});
		menu->addAction(addToWatchAct);
	}

	if (menu != nullptr) {
		QPoint pt(pos);
		menu->exec(tree->mapToGlobal(pos));
	}
}
