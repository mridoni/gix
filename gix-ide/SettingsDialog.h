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

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>
#include <QList>

#include "ui_Settings.h"

class SettingsDialog : public QDialog, private Ui::Ui_Settings
{
	Q_OBJECT

public:
	explicit SettingsDialog(QWidget *parent = 0);
	~SettingsDialog();

	void accept();

public slots:
	void updateCompilerInfo(int t, int idx);

private:
		void GnuCobolCfgTab_LoadSettings();
		bool GnuCobolCfgTab_CheckSettings();
		bool GnuCobolCfgTab_SaveSettings();
		
		void GeneralCfgTab_LoadSettings();
		bool GeneralCfgTab_CheckSettings();
		bool GeneralCfgTab_SaveSettings();
		
		void ESQLCfgTab_LoadSettings();
		bool ESQLCfgTab_CheckSettings();
		bool ESQLCfgTab_SaveSettings();

		void EditorCfgTab_LoadSettings();
		bool EditorCfgTab_CheckSettings();
		bool EditorCfgTab_SaveSettings();

		void DebugCfgTab_LoadSettings();
		bool DebugCfgTab_CheckSettings();
		bool DebugCfgTab_SaveSettings();

		void choosePath(QLabel *l, bool is_dir);

		bool settingsSetValue(QString n, QString v);
		bool settingsSetValue(QString n, bool v);
		bool settingsSetValue(QString n, int v);

		void addCompiler();
		void initCompilers();
};

