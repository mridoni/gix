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

#include "NavigationWindow.h"
#include "Ide.h"
#include "IdeTaskManager.h"
#include "MetadataManager.h"
#include "MdiChild.h"
#include "PathUtils.h"
#include "MainWindow.h"

#include <QToolBar>
#include <QTreeWidget>
#include <QDockWidget>

NavigationWindow::NavigationWindow(QWidget* parent, MainWindow* mw)
{
	this->setWindowTitle(tr("Navigator"));
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
	connect(bRefresh, &QPushButton::clicked, this, [this] {
			refreshContent();
	});

	this->dataWidget = new QTreeWidget(this);
	this->setCentralWidget(dataWidget);
	dataWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
	dataWidget->setColumnCount(1);
	dataWidget->setUniformRowHeights(true);
	dataWidget->setHeaderLabels(QStringList() << tr("Name"));

	connect(dataWidget, &QTreeWidget::itemDoubleClicked, this, &NavigationWindow::dataItemDoubleClicked);

	dataWidget->setContextMenuPolicy(Qt::CustomContextMenu);
	connect(GixGlobals::getMetadataManager(), &MetadataManager::updatedModuleMetadata, this, [this](CobolModuleMetadata *cmm) {
		refreshContent();
	});	
	
	connect(Ide::TaskManager(), &IdeTaskManager::projectCollectionClosed, this, [this] {
		this->setContent(nullptr);
	});	
	
	connect(Ide::TaskManager(), &IdeTaskManager::fileActivated, this, [this] (ProjectFile *pf) {
		this->setContent(pf);
	});

	connect(Ide::TaskManager(), &IdeTaskManager::fileClosed, this, [this](ProjectFile* pf) {
		this->setContent(pf);
	});
}

NavigationWindow::~NavigationWindow()
{
}

void NavigationWindow::refreshContent()
{
	dataWidget->clear();

	Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("DataWindow is refreshing"), QLogger::LogLevel::Debug);
	if (mainWindow->activeMdiChild() != nullptr) {
		QString f = mainWindow->activeMdiChild()->currentFile();

		ProjectCollection *ppj = Ide::TaskManager()->getCurrentProjectCollection();
		if (ppj != nullptr) {
			ProjectFile *pf = ppj->locateProjectFileByPath(f);
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

void NavigationWindow::setContent(ProjectFile *pf)
{
	QString lstPath, module_name, output_path;

	QString configuration = Ide::TaskManager()->getCurrentConfiguration();
	QString platform = Ide::TaskManager()->getCurrentPlatform();

	if (pf == nullptr || pf->PropertyGetValue("build_action") != "compile" || !pf->getOutputModuleAndFile(configuration, platform, module_name, output_path)) {
		cur_file = nullptr;
		refresh_data_items();
		return;
	}

	CobolModuleMetadata *cfm = GixGlobals::getMetadataManager()->getModuleMetadataBySource(pf->GetFileFullPath());
	if (cfm) {
		cur_file = pf;
	}
	else {
		cur_file = nullptr;
	}
	refresh_data_items();
}

bool NavigationWindow::hasContent()
{
	return false;
}

void NavigationWindow::refresh_data_items()
{
	dataWidget->clear();

	if (!cur_file)
		return;

	CobolModuleMetadata* cur_data = GixGlobals::getMetadataManager()->getModuleMetadataBySource(cur_file->GetFileFullPath());
	if (cur_data == nullptr)
		return;

	//QTreeWidgetItem* root = new QTreeWidgetItem(dataWidget, QStringList(tr("Paragraphs")));

	//root->setData(0, Qt::UserRole, "PS");
	/*dataWidget->addTopLevelItem(root);*/

	QMap<QString, Paragraph *> entries = cur_data->getParagraphs();
	for (int i = 0; i < entries.size(); i++) {
		QString name = entries.keys().at(i);
		Paragraph* p = entries[name];
		
		QTreeWidgetItem* si = new QTreeWidgetItem(QStringList() << name);
		si->setData(0, Qt::UserRole, QVariant::fromValue<void*>(p));
		//root->addChild(si);
		dataWidget->addTopLevelItem(si);
	}

	//setNodeStatus("PS", root);

	dataWidget->resizeColumnToContents(0);

}

void NavigationWindow::append_children(DataEntry* e, QTreeWidgetItem* parent_item)
{
}

void NavigationWindow::goto_definition(DataEntry* e)
{
}

void NavigationWindow::dataItemDoubleClicked(QTreeWidgetItem* item, int column)
{
	QVariant v = item->data(0, Qt::UserRole);
	if (v.isValid()) {
		Paragraph * p = (Paragraph*)v.value<void*>();
		Ide::TaskManager()->gotoDefinition(p);
	}
}

void NavigationWindow::IdeStatusChanged(IdeStatus)
{

}
