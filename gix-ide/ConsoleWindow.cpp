/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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

#include "ConsoleWindow.h"
#include "UiUtils.h"

#include <QSettings>

ConsoleWindow::ConsoleWindow(QWidget* parent, MainWindow* mw)
{
	this->setWindowTitle("Output");
	this->setMinimumHeight(100);
	this->setMinimumWidth(500);
	this->setWindowFlags(Qt::Widget); // <---------
	toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

	this->console = new ConsoleWidget(this);
	//this->console->setReadOnly(true);
	
	setConsoleFont();

	this->setCentralWidget(console);
}

void ConsoleWindow::setConsoleFont()
{
	QSettings settings;

	QFont f = this->console->font();

	QString font_name = settings.value("console_font_name", "").toString();
	int font_size = settings.value("console_font_size", 0).toInt();

	if (font_name.isEmpty() || font_size == 0) {
		f.setFamily("Courier New");
		f.setPointSize(UiUtils::computeFontSize(this, 8));
	}
	else {
		f.setFamily(font_name);
		f.setPointSize(font_size);
	}

	this->console->setFont(f);
}

ConsoleWindow::~ConsoleWindow()
{
	
}

void ConsoleWindow::clear()
{
	console->clear();
	setConsoleFont();
}

void ConsoleWindow::append(QString s, bool is_err)
{
	console->putData(s.toUtf8());
}

void ConsoleWindow::appendOut(QString s)
{
	append(s, false);
}

void ConsoleWindow::appendErr(QString s)
{
	append(s, true);
}

