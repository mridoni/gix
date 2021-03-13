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

#include <QTreeWidget>
#include <QStringList>
#include <QPushButton>
#include <QStandardItemModel>

#include "MainWindow.h"
#include "ProjectFile.h"
#include "ListingFileParser.h"

class QWidget;
class MainWindow;

class DependenciesWindow : public QMainWindow
{
	friend class MainWindow;

public:
	DependenciesWindow(QWidget* parent, MainWindow* mw);
	~DependenciesWindow();

public slots:
	void IdeStatusChanged(IdeStatus);
	//void onDebuggerBreak();
	void updateFileDependencies();
	void updateProjectDependencies();

private:
	MainWindow* mainWindow;

	QTabWidget *tab_widget;
	QTreeWidget* file_dataWidget;
	QTreeWidget* prj_dataWidget;
	ListingFileParser* cur_data;
	

	void file_ItemDoubleClicked(QTreeWidgetItem* item, int column);
	void prj_ItemDoubleClicked(QTreeWidgetItem* item, int column);
	void tabChanged(int idx);

	bool get_project_dependencies(QMap<QString, QStringList> &deps);
};

