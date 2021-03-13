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

#ifdef __MINGW32__
typedef unsigned char byte;
#include <cstddef>
#endif

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>

#include "ui_Search.h"
//#include "MainWindow.h"
#include "IdeSearchManager.h"

class QDialogButtonBox;
class QFileInfo;
class QTabWidget;
class SearchDialog;
class ProjectCollection;
class MainWindow;

class SearchDialog : public QDialog, private Ui::Ui_Search
{
	Q_OBJECT

public:
	SearchDialog(MainWindow *mw);
	~SearchDialog();

	void setup(SearchType mode = SearchType::Find, QString search_spec = QString());
	void show();

	virtual bool event(QEvent*);

private:
	MainWindow* main_window;

	void findNext();
	void count();
	void replace();
	void replaceAll();
	void replaceInOpenDocs();

	void setupUiForSearchType(SearchType mode);
	SearchType tabIndexToSearchType(int idx);

private slots:
	void searchTypeTabChanged(int idx);

};
