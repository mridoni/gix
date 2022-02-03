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

#pragma once

#include <QMainWindow>
#include <QToolBar>

#include "MainWindow.h"
#include "ConsoleWidget.h"

#if defined(__linux__)
#include <qtermwidget5/qtermwidget.h>
#endif

class ConsoleWindow : public QMainWindow
{
    Q_OBJECT

public:
	ConsoleWindow(QWidget* parent, MainWindow* mw);
    virtual ~ConsoleWindow();

    void setEcho(bool b);
	void clear();
    void setConsoleFont();

	void append(QString s, bool is_err = false);
	void appendOut(QString s);
	void appendErr(QString s);

    QString getTextContent();

signals:
    void inputAvailable(const QString& s);

private:

#if defined(__linux__)
    QTermWidget *console;
    QString console_buffer;
#else
    ConsoleWidget* console;
#endif

	MainWindow* mainWindow;
	QToolBar* toolBar;

    bool has_echo = true;
};

