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
};

