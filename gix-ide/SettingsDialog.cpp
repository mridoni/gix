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

#include "SettingsDialog.h"
#include "UiUtils.h"
#include "SysUtils.h"
#include "Ide.h"
#include "IdeTaskManager.h"
#include "CompilerManager.h"
#include "CompilerDefinition.h"
#include "CustomDialog.h"
#include "AddCompilerDialog.h"
#include "GixGlobals.h"

#include <QSettings>
#include <QFileDialog>

SettingsDialog::SettingsDialog(QWidget* parent)
{
	setupUi(this);
	this->setAttribute(Qt::WA_DeleteOnClose);

	connect(cancelButton, &QPushButton::clicked, this, [this] { close(); });
	connect(okButton, &QPushButton::clicked, this, [this] { accept(); });

	initCompilers();

	cbEditorFontSize->clear();
	for (int i = 6; i <= 24; i++) {
		cbEditorFontSize->addItem(QString::number(i));
		cbGridFontSize->addItem(QString::number(i));
		cbTreeviewFontSize->addItem(QString::number(i));
		cbConsoleFontSize->addItem(QString::number(i));
	}

	cbEolMode->clear();
	cbEolMode->addItem(tr("Platform Default"), (int)EolMode::PlatformDefault);
	cbEolMode->addItem(tr("Windows (CR+LF)"), (int)EolMode::Windows);
	cbEolMode->addItem(tr("Unix/Linux (LF)"), (int)EolMode::Unix);
	cbEolMode->addItem(tr("Classic MacOS (CR)"), (int)EolMode::ClassicMacOS);

	cbDebugStartupBehaviour->addItem(tr("Stop at breakpoints"), (int)0x01);
	cbDebugStartupBehaviour->addItem(tr("Stop at the first line of the main module"), (int)0x02);
	cbDebugStartupBehaviour->addItem(tr("Stop at the first line of every module"), (int)0x03);
	cbSkipCheckPackageVersionCheck->setChecked(false);

	GeneralCfgTab_LoadSettings();
	GnuCobolCfgTab_LoadSettings();
	ESQLCfgTab_LoadSettings();
	EditorCfgTab_LoadSettings();
	DebugCfgTab_LoadSettings();

	this->tabWidget->setCurrentIndex(0);

	// For now, we do not show ESQL options
	this->tabWidget->setTabEnabled(3, false);
	this->setStyleSheet("QTabBar::tab::disabled {width: 0; height: 0; margin: 0; padding: 0; border: none;} ");
	// 
	label_9->setVisible(false);
	label_10->setVisible(false);
	tHostPort->setVisible(false);
	cbSkipCheckPackageVersionCheck->setVisible(false);

	connect(btnChoosePreprocBin, &QPushButton::clicked, this, [this] { choosePath(binPreprocessorPath, false); });
	connect(btnChooseLinkLib, &QPushButton::clicked, this, [this] { choosePath(libLinkPath, false); });
	connect(btnChooseRuntimeLib, &QPushButton::clicked, this, [this] { choosePath(libRuntimePath, false); });

	connect(btnAddCompiler, &QPushButton::clicked, this, [this] { addCompiler(); });

}


SettingsDialog::~SettingsDialog()
{}


void SettingsDialog::initCompilers()
{
	cbReleaseCompiler->clear();
	cbDebugCompiler->clear();
	QMap<QString, CompilerDefinition *> compilers = GixGlobals::getCompilerManager()->getCompilers();
	QMap<QString, CompilerDefinition *>::iterator it;
	for (it = compilers.begin(); it != compilers.end(); ++it) {
		CompilerDefinition *cd = it.value();
		cbReleaseCompiler->addItem(cd->getName(), cd->getId());
		cbDebugCompiler->addItem(cd->getName(), cd->getId());
	}
}

void SettingsDialog::accept()
{
	if (!GeneralCfgTab_CheckSettings() || !GnuCobolCfgTab_CheckSettings() ||
		!EditorCfgTab_CheckSettings() || !DebugCfgTab_CheckSettings()) {	// ESQLCfgTab is hidden/disabled
		UiUtils::ErrorDialog(tr("Please check your settings"));
		return;
	}

	bool changed = GeneralCfgTab_SaveSettings();
	changed |= GnuCobolCfgTab_SaveSettings();
	changed |= ESQLCfgTab_SaveSettings();
	changed |= EditorCfgTab_SaveSettings();
	changed |= DebugCfgTab_SaveSettings();

	QDialog::accept();

	if (changed) {
		emit Ide::TaskManager()->SettingsChanged();
	}

	this->close();
}

bool SettingsDialog::GnuCobolCfgTab_SaveSettings()
{
	QSettings settings;
	QMap<QString, CompilerDefinition*> compilers = GixGlobals::getCompilerManager()->getCompilers();

	bool changed = settingsSetValue("ReleaseCompilerId", cbReleaseCompiler->currentData().toString());
	changed |= settingsSetValue("DebugCompilerId", cbDebugCompiler->currentData().toString());

	return changed;
}

void SettingsDialog::GnuCobolCfgTab_LoadSettings()
{
	QSettings settings;
	int idx = 0;

	idx = cbReleaseCompiler->findData(settings.value("ReleaseCompilerId", ""));
	cbReleaseCompiler->setCurrentIndex((idx >= 0) ? idx : 0);

	idx = cbDebugCompiler->findData(settings.value("DebugCompilerId", ""));
	cbDebugCompiler->setCurrentIndex((idx >= 0) ? idx : 0);
}

bool SettingsDialog::GnuCobolCfgTab_CheckSettings()
{
	return true;
}

bool SettingsDialog::GeneralCfgTab_SaveSettings()
{
	QSettings settings;

	bool changed = settingsSetValue("Ide_DebugOutput", cbDebugOutput->isChecked());
	return changed;
}

void SettingsDialog::GeneralCfgTab_LoadSettings()
{
	QSettings settings;
	cbDebugOutput->setChecked(settings.value("Ide_DebugOutput", false).toBool());
}

bool SettingsDialog::GeneralCfgTab_CheckSettings()
{
	return true;
}

bool SettingsDialog::ESQLCfgTab_SaveSettings()
{
	QSettings settings;
	bool changed = settingsSetValue("ESQL_PreprocBinPath", binPreprocessorPath->text());
	changed |= settingsSetValue("ESQL_RuntimeLibPath", libRuntimePath->text());
	changed |= settingsSetValue("ESQL_LinkLibPath", libLinkPath->text());
	return changed;
}

void SettingsDialog::EditorCfgTab_LoadSettings()
{
	QSettings settings;
	cbEditorFontName->setCurrentText(settings.value("editor_font_name", "Courier New").toString());
	cbEditorFontSize->setCurrentText(settings.value("editor_font_size", 10).toString());
	cbGridFontName->setCurrentText(settings.value("grid_font_name", "Courier New").toString());
	cbGridFontSize->setCurrentText(settings.value("grid_font_size", 10).toString());
	cbTreeviewFontName->setCurrentText(settings.value("treeview_font_name", "Courier New").toString());
	cbTreeviewFontSize->setCurrentText(settings.value("treeview_font_size", 10).toString());
	cbConsoleFontName->setCurrentText(settings.value("console_font_name", "Courier New").toString());
	cbConsoleFontSize->setCurrentText(settings.value("console_font_size", 10).toString());
	cbEolMode->setCurrentIndex(cbEolMode->findData(settings.value("default_eol_mode", -1).toInt()));
	cbConvertOnOpen->setChecked(settings.value("convert_eols_on_open", false).toBool());
}

bool SettingsDialog::EditorCfgTab_CheckSettings()
{
	return true;
}

bool SettingsDialog::EditorCfgTab_SaveSettings()
{
	QSettings settings;
	bool changed = settingsSetValue("editor_font_name", cbEditorFontName->currentText());
	changed |= settingsSetValue("editor_font_size", cbEditorFontSize->currentText().toInt());
	changed |= settingsSetValue("grid_font_name", cbGridFontName->currentText());
	changed |= settingsSetValue("grid_font_size", cbGridFontSize->currentText().toInt());
	changed |= settingsSetValue("treeview_font_name", cbTreeviewFontName->currentText());
	changed |= settingsSetValue("treeview_font_size", cbTreeviewFontSize->currentText().toInt());
	changed |= settingsSetValue("console_font_name", cbConsoleFontName->currentText());
	changed |= settingsSetValue("console_font_size", cbConsoleFontSize->currentText().toInt());
	changed |= settingsSetValue("default_eol_mode", cbEolMode->currentData().toInt());
	changed |= settingsSetValue("convert_eols_on_open", cbConvertOnOpen->isChecked());
	return changed;
}

void SettingsDialog::DebugCfgTab_LoadSettings()
{
	QSettings settings;
	cbDebugStartupBehaviour->setCurrentIndex(settings.value("debugger_startup_behaviour", 1).toInt()-1);
	tHostPort->setText(settings.value("host_debugger_port", 13008).toString());
	cbSkipCheckPackageVersionCheck->setChecked(settings.value("debugger_skip_package_version_check", false).toBool());
}

bool SettingsDialog::DebugCfgTab_CheckSettings()
{
	bool ok;
	int port = tHostPort->text().toInt(&ok, 10);
	return (ok && port > 0 && port <= 65535);
}

bool SettingsDialog::DebugCfgTab_SaveSettings()
{
	QSettings settings;
	bool changed = settingsSetValue("host_debugger_port", tHostPort->text().toInt());
	changed |= settingsSetValue("debugger_startup_behaviour", cbDebugStartupBehaviour->currentIndex() + 1);
	changed |= settingsSetValue("debugger_skip_package_version_check", cbSkipCheckPackageVersionCheck->isChecked());
	return changed;
}

void SettingsDialog::ESQLCfgTab_LoadSettings()
{
	QSettings settings;
	binPreprocessorPath->setText(settings.value("ESQL_PreprocBinPath", "").toString());
	libRuntimePath->setText(settings.value("ESQL_RuntimeLibPath", "").toString());
	libLinkPath->setText(settings.value("ESQL_LinkLibPath", "").toString());
}

bool SettingsDialog::ESQLCfgTab_CheckSettings()
{
	return !binPreprocessorPath->text().isEmpty() &&
		!libRuntimePath->text().isEmpty() &&
		!libLinkPath->text().isEmpty();
}

void SettingsDialog::choosePath(QLabel* l, bool is_dir)
{
	QFileDialog dialog;
	if (is_dir) {
		dialog.setFileMode(QFileDialog::Directory);
		dialog.setOption(QFileDialog::ShowDirsOnly);
	}
	if (dialog.exec()) {
		QString path = dialog.selectedFiles()[0];
		l->setText(path);
	}
}

bool SettingsDialog::settingsSetValue(QString n, QString v)
{
	QSettings settings;
	bool res = false;

	if (settings.contains(n)) {
		QVariant cur_value = settings.value(n);

		if (cur_value.type() == QVariant::Type::String)
			res = cur_value.toString() != v;
		else
			res = true;
	}
	else
		res = true;

	settings.setValue(n, v);
	return res;
}

bool SettingsDialog::settingsSetValue(QString n, bool v)
{
	QSettings settings;
	bool res = false;

	if (settings.contains(n)) {
		QVariant cur_value = settings.value(n);

		if (cur_value.type() == QVariant::Type::Bool)
			res = cur_value.toBool() != v;
		else 
			res = true;
	}
	else
		res = true;

	settings.setValue(n, v);
	return res;
}

bool SettingsDialog::settingsSetValue(QString n, int v)
{
	QSettings settings;
	bool res = false;

	if (settings.contains(n)) {
		QVariant cur_value = settings.value(n);

		if (cur_value.type() == QVariant::Type::Int)
			res = cur_value.toInt() != v;
		else
			res = true;
	}
	else
		res = true;

	settings.setValue(n, v);
	return res;
}

void SettingsDialog::addCompiler()
{
	std::string basedir;
	bool b;
	AddCompilerDialog *adlg = new AddCompilerDialog(this);

	if (adlg->exec()) {
		initCompilers();
		GnuCobolCfgTab_LoadSettings();
	}

}

