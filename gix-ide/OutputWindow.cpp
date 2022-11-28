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
	connect(pane_selector, QOverload<int>::of(&QComboBox::currentIndexChanged), this, &OutputWindow::switchPane);
	toolBar->addWidget(pane_selector);



	//connect(Ide::TaskManager(), &IdeTaskManager::print, this, &OutputWindow::print, Qt::ConnectionType::QueuedConnection);

	//Ide::TaskManager()->flushLog();
}


OutputWindow::~OutputWindow()
{
	delete(toolBar);
}

//
//void OutputWindow::print(QString msg, QLogger::LogLevel log_level)
//{
//#ifdef WIN32	
//    if (Ide::TaskManager()->getTestHelper()) {
//        QString ide_out_dup_file = Ide::TaskManager()->getIdeOutputDupFile();
//        if (!ide_out_dup_file.isEmpty()) {
//
//            if (!msg.endsWith("\n"))
//                msg += "\n";
//
//            QFile f(ide_out_dup_file);
//            f.open(QIODevice::OpenModeFlag::Append);
//            f.write(msg.toUtf8().constData());
//            f.close();
//        }
//    }
//#endif
//    if (!Ide::TaskManager()->isDebugOutputEnabled() && (log_level == QLogger::LogLevel::Debug || log_level == QLogger::LogLevel::Trace))
//        return;
//
//    QColor textColor(Qt::black);
//
//    switch (log_level) {
//        case QLogger::LogLevel::Trace:
//            textColor = Qt::blue;
//            break;
//
//        case QLogger::LogLevel::Debug:
//            textColor = Qt::darkCyan;
//            break;
//
//        case QLogger::LogLevel::Info:
//            textColor = Qt::darkGray;
//            break;
//
//        case QLogger::LogLevel::Error:
//            textColor = Qt::red;
//            break;
//
//        case QLogger::LogLevel::Success:
//            textColor = Qt::darkGreen;
//            break;
//    }
//    textArea->setTextColor(textColor);
//    textArea->append(msg);
//    textArea->ensureCursorVisible();
//
//}

QString OutputWindow::getTextContent(int index)
{
	if (index >= panes.size() || index < 0)
		return QString();

	QTextEdit* pane = panes[index];
	return pane->toPlainText();
}

void OutputWindow::addPanes(QStringList names)
{
	for (int i = 0; i < names.size(); i++) {
		QTextEdit *pane = new QTextEdit(this);
		pane->setReadOnly(true);
		QFont f = pane->font();
		f.setFamily("Courier New");
		f.setPointSize(UiUtils::computeFontSize(this, 9));
		pane->setFont(f);
		this->setVisible(false);

		this->setCentralWidget(pane);

		pane_selector->addItem(names.at(i));

		panes[i] = pane;
	}

	pane_selector->setCurrentIndex(0);
}

void OutputWindow::switchPane(int index)
{
	for (int i = 0; i < panes.size(); i++) {
		QTextEdit* pane = panes[i];
		pane->setVisible(i == index);
	}
}

void OutputWindow::clearAll()
{
	for (int i = 0; i < panes.size(); i++) {
		QTextEdit* pane = panes[i];
		pane->clear();
	}
}
