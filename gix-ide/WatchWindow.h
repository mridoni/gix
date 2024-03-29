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
#include <QTableWidget>
#include <QStringList>
#include <QPushButton>

#include "MainWindow.h"
#include "DragDropTableWidget.h"


class QWidget;
class MainWindow;

class WatchWindow : public QMainWindow
{
	friend class IdeTaskManager;

public:
	WatchWindow(QWidget *parent, MainWindow *mw);
	~WatchWindow();

public slots:
	void addButtonClicked();
	void removeButtonClicked();
	void refreshButtonClicked();
	void IdeStatusChanged(IdeStatus);
	void onDebuggerBreak();
	void itemDropped(QVariant v, const QMimeData *md);

private:
	MainWindow * mainWindow;
	DragDropTableWidget *varTable;

	QPushButton *bAdd;
	QPushButton *bRemove;
	QPushButton *bRefresh;

	void refreshContent();
};

