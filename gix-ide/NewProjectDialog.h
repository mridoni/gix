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

#pragma once

#ifdef __MINGW32__
#include <cstddef>
#endif

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>

#include "ui_NewProject.h"

class QDialogButtonBox;
class QFileInfo;
class QTabWidget;
class NewProjectDialog;
class ProjectCollection;
class Project;


class NewProjectDialog : public QDialog, private Ui::Ui_NewProject
{
	Q_OBJECT

public:
	NewProjectDialog(ProjectCollection *, QWidget *parent = 0);
	~NewProjectDialog() ;

	void show();

	void accept();

public slots:
	void projectTypeSelectionChanged(bool b);

private:
	ProjectCollection* prj_collection;

	void choosePrjFile();
	bool is_valid_prj_name(QString p);
	bool create_prj_file(Project* prj, int idx);
};
