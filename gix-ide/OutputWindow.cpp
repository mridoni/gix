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

#include "OutputWindow.h"
#include "UiUtils.h"
#include "Ide.h"
#include "IdeTaskManager.h"
#include "ide_sink.h"


OutputWindow::OutputWindow(QWidget* parent, MainWindow* mw) : QMainWindow(parent)
{
	this->setWindowTitle("Output");
	this->setMinimumHeight(100);
	this->setMinimumWidth(250);
	this->setWindowFlags(Qt::Widget); // <---------
	toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

	pane_selector = new QComboBox(this);
	QFont f = pane_selector->font();
	f.setPointSize(UiUtils::computeFontSize(this, 9));
	pane_selector->setFont(f);

	connect(pane_selector, QOverload<int>::of(&QComboBox::currentIndexChanged), this, QOverload<int>::of(&OutputWindow::switchPane));
	toolBar->addWidget(pane_selector);

	layout = new QHBoxLayout();
	layout->setStretch(10, 10);
	QWidget* widget = new QWidget(this);
	widget->setLayout(layout);
	this->setCentralWidget(widget);

	//connect(Ide::TaskManager(), &IdeTaskManager::print, this, &OutputWindow::print, Qt::ConnectionType::QueuedConnection);

}

OutputWindow::~OutputWindow()
{
	delete(toolBar);
}

QString OutputWindow::getTextContent(OutputWindowPaneType index)
{
	if (!panes.contains(index))
		return QString();

	QTextEdit* pane = panes[index]->getWindowPane();
	return pane->toPlainText();
}

void OutputWindow::addLoggerSection(OutputWindowPaneType pt, QString name)
{
	OutputWindowLogger* p = new OutputWindowLogger(pt, this);

	layout->addWidget(p->getWindowPane());
	pane_selector->addItem(name, (int)pt);
	panes[pt] = p;
#if _DEBUG
	p->getWindowPane()->setPlainText(name);
#endif
}

OutputWindowLogger* OutputWindow::getLoggerSection(OutputWindowPaneType index)
{
	if (panes.contains(index))
		return panes[index];

	return nullptr;
}

void OutputWindow::switchPane(OutputWindowPaneType t)
{
	for (auto it = panes.begin(); it != panes.end(); ++it) {
		OutputWindowLogger* p = it.value();
		bool is_visible = it.key() == t;
		p->getWindowPane()->setVisible(is_visible);
	}
}

void OutputWindow::switchPane(int index)
{
	auto v = pane_selector->itemData(index).toInt();
	OutputWindowPaneType pt = (OutputWindowPaneType)v;
	if (panes.contains(pt))
		switchPane(pt);
}

void OutputWindow::clearAll()
{
	for (auto it = panes.begin(); it != panes.end(); ++it) {
		QTextEdit* pane = it.value()->getWindowPane();
		pane->clear();
	}
}

