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

#include "WatchWindow.h"
#include "UiUtils.h"
#include "Ide.h"
#include "IdeTaskManager.h"

#include <QToolBar>
#include <QLabel>
#include <QLineEdit>
#include <QComboBox>
#include <QHeaderView>
#include <QBoxLayout>
#include <QPushButton>
#include <QInputDialog>
#include <QSettings>


WatchWindow::WatchWindow(QWidget *parent, MainWindow *mw) : QMainWindow(parent)
{
	this->setWindowTitle("Watch");
	this->setMinimumWidth(300);
	this->setWindowFlags(Qt::Widget); // <---------
	QToolBar* toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

	bAdd = new QPushButton(this);
	bAdd->setFixedSize(16, 16);
	const QIcon addIcon = QIcon(":/icons/bullet_add.png");
	bAdd->setIcon(addIcon);
	toolBar->addWidget(bAdd);

	bRemove = new QPushButton(this);
	bRemove->setFixedSize(16, 16);
	const QIcon removeIcon = QIcon(":/icons/bullet_delete.png");
	bRemove->setIcon(removeIcon);
	toolBar->addWidget(bRemove);

	bRefresh = new QPushButton(this);
	bRefresh->setFixedSize(16, 16);
	const QIcon refreshIcon = QIcon(":/icons/bullet_refresh.png");
	bRefresh->setIcon(refreshIcon);
	toolBar->addWidget(bRefresh);

	this->varTable = new DragDropTableWidget(0, 2, false, true, this);
	this->setCentralWidget(varTable);

	varTable->setRowCount(0);
	QHeaderView* header = varTable->horizontalHeader();
	header->setSectionResizeMode(QHeaderView::Stretch);
	header->hide();

	connect(bAdd, &QPushButton::clicked, this, &WatchWindow::addButtonClicked);
	connect(bRemove, &QPushButton::clicked, this, &WatchWindow::removeButtonClicked);
	connect(bRefresh, &QPushButton::clicked, this, &WatchWindow::refreshButtonClicked);

	//connect(varTable, &DragDropTableWidget::itemDropped, this, &WatchWindow::itemDropped);
	connect(varTable, &DragDropTableWidget::itemDropped, this, &WatchWindow::itemDropped);
}


WatchWindow::~WatchWindow()
{
}

void WatchWindow::IdeStatusChanged(IdeStatus s)
{
	bAdd->setEnabled(s == IdeStatus::DebuggingOnBreak);
	bRefresh->setEnabled(s == IdeStatus::DebuggingOnBreak);
	bRemove->setEnabled(s == IdeStatus::DebuggingOnBreak);

	if (s == IdeStatus::DebuggingOnBreak)
		refreshContent();
}

void WatchWindow::onDebuggerBreak()
{
	refreshContent();
}

void WatchWindow::itemDropped(QVariant v, const QMimeData *md)
{
	DataEntry *e = (DataEntry *)v.value<DataEntry *>();
	if (!e || e->name.isEmpty())
		return;

	Ide::TaskManager()->addWatchedVar(e->name);
	if (Ide::TaskManager()->getDebugManager())
		Ide::TaskManager()->getDebugManager()->addWatchedVar(e->name);

	refreshContent();

	Ide::TaskManager()->saveCurrentProjectCollectionState();
}

void WatchWindow::refreshContent()
{
	QSettings settings;

	varTable->clear();

	if (Ide::TaskManager()->getDebugManager() == nullptr)
		return;

	DebugManager *debug_manager = Ide::TaskManager()->getDebugManager();

	varTable->setRowCount(debug_manager->getWatchedVarCount());
	varTable->setColumnCount(2);

	QHeaderView* header = varTable->horizontalHeader();
	header->setSectionResizeMode(QHeaderView::Stretch);
	header->setSizeAdjustPolicy(QAbstractScrollArea::AdjustToContents);
	header->hide();

	QHeaderView* vheader = varTable->verticalHeader();
	vheader->hide();

	vheader->setDefaultSectionSize(20);
	int font_size = settings.value("console_font_size", 8).toInt();

	QMap<QString,QString> watched_var_contents = Ide::TaskManager()->getDebugManager()->getPrintableVarListContent(debug_manager->getWatchedVarList());

	for (int i = 0; i < watched_var_contents.size(); i++) {
		QString name = watched_var_contents.keys().at(i);
		QString content = watched_var_contents.value(name);

		QTableWidgetItem *tw = new QTableWidgetItem();
		tw->setText(name);
		tw->setFlags(tw->flags() &  ~Qt::ItemIsEditable);

		QFont originalFont = tw->font();
		originalFont.setPointSize(UiUtils::computeFontSize(this, font_size));
		tw->setFont(originalFont);

		QLabel *qle = new QLabel(varTable);
		qle->setFont(originalFont);
		qle->setText(content);
		varTable->setCellWidget(i, 1, qle);
		varTable->setItem(i, 0, tw);
	}
}

void WatchWindow::addButtonClicked()
{
	bool ok;
	QString text = QInputDialog::getText(this, tr("Enter a variable/file name"),
		tr("Variable/field name:"), QLineEdit::Normal,
		"", &ok);

	if (!ok)
		return;

	if (text.isEmpty()) {
		UiUtils::ErrorDialog(tr("Invalid variable/field name"));
		return;
	}

	Ide::TaskManager()->addWatchedVar(text);
	if (Ide::TaskManager()->getDebugManager())
		Ide::TaskManager()->getDebugManager()->addWatchedVar(text);


	refreshContent();

	Ide::TaskManager()->saveCurrentProjectCollectionState();
}

void WatchWindow::removeButtonClicked()
{
	QList<QTableWidgetItem *> selected = varTable->selectedItems();
	if (selected.size() == 0)
		return;

	for (int i = 0; i < selected.size(); i++) {
		QTableWidgetItem *item = selected.at(i);
		QString text = item->text();
		if (!text.isEmpty()) {
			Ide::TaskManager()->removeWatchedVar(text);
			if (Ide::TaskManager()->getDebugManager())
				Ide::TaskManager()->getDebugManager()->removeWatchedVar(text);
		}
	}

	refreshContent();

	Ide::TaskManager()->saveCurrentProjectCollectionState();
}

void WatchWindow::refreshButtonClicked()
{
	refreshContent();
}
