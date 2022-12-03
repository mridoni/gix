#pragma once

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

#include <QString>
#include <QList>
#include <QMap>
#include <QToolBar>
#include <QFile>
#include <QTextEdit>
#include <QComboBox>
#include <QHBoxLayout>
#include <QTableWidget>

#include "MainWindow.h"
#include "ErrorWarningFilter.h"

class ErrorWindow : public QMainWindow
{

public:
	ErrorWindow(QWidget* parent, MainWindow* mw);
	~ErrorWindow();

	void clear();
	void addEntries(QList<ErrorWarningFilterEntry> entries);
	void updateErrorList();

public slots:
	void errorListDoubleClicked(const QModelIndex& mi);


private:
	MainWindow* mainWindow;
	QToolBar* toolBar;
	QHBoxLayout* layout;
	QTableWidget* err_grid = nullptr;

	QList<ErrorWarningFilterEntry> entries;
	QLabel* get_label(QString t, QString tt = QString());
};

