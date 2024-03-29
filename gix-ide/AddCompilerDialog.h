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
#include <cstddef>
#endif

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>

#include "CompilerDefinition.h"
#include "ui_AddCompilerDialog.h"

class QDialogButtonBox;
class QFileInfo;
class QTabWidget;
class NewProjectDialog;
class ProjectCollection;
class Project;


class AddCompilerDialog : public QDialog, private Ui::Ui_AddCompilerDialog
{
	Q_OBJECT

public:
	AddCompilerDialog(QWidget *parent = 0);
	~AddCompilerDialog();

	void show();

	void __accept();

public slots:

private:
	void addPlatform();
	bool validateCompilerDefinition();
	bool saveCompilerDefinition();
	void chooseBaseDir();

};
