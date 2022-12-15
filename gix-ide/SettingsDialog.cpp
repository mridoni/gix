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
#include "ESQLConfiguration.h"
#include "AddCompilerWizard.h"
#include "DebugDriverFactory.h"
#include "NetworkManager.h"

#include <QSettings>
#include <QFileDialog>
#include <QMenu>
#include <QLineEdit>
#include <QToolButton>
#include <QHostAddress>

class CompilerOptsWizardPage;

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

	cbScreenRes->clear();
    cbScreenRes->addItem("System Default", (int)0);
	cbScreenRes->addItem("48", (int)48);
	cbScreenRes->addItem("72", (int)72);
	cbScreenRes->addItem("96", (int)96);
	cbScreenRes->addItem("120", (int)120);
	cbScreenRes->addItem("144", (int)144);
    cbScreenRes->addItem("192", (int)192);
    cbScreenRes->addItem("216", (int)216);

	cb_esql_pp_driver_list->addItem("Gix (Internal)", "esql_driver_gix_internal");
	cb_esql_pp_driver_list->addItem("Gix (External)", "esql_driver_gix_external");

	cbDefaultSourceFormat->addItem("Fixed", "fixed");
	cbDefaultSourceFormat->addItem("Free", "free");

	cbDebugEngine->addItem("Standard", (int)DebugDriverType::Standard);
	cbDebugEngine->addItem("Experimental", (int)DebugDriverType::Experimental);

	txtDbgrBindingAddress->setText(DBGR_LOCAL_BINDING_DEFAULT_ADDR);
	txtDbgrBindingPort->setText(QString::number(DBGR_LOCAL_BINDING_DEFAULT_PORT));

	GeneralCfgTab_LoadSettings();
	GnuCobolCfgTab_LoadSettings();
	ESQLCfgTab_LoadSettings();
	EditorCfgTab_LoadSettings();
	DebugCfgTab_LoadSettings();

	this->tabWidget->setCurrentIndex(0);

	// For now, we do not show ESQL options
	this->tabWidget->setTabEnabled(3, false);
	this->setStyleSheet("QTabBar::tab::disabled {width: 0; height: 0; margin: 0; padding: 0; border: none;} ");


	QMenu *menu = new QMenu(this);
	QAction *act1 = new QAction(tr("Add compiler (manually)..."), this);
	connect(act1, &QAction::triggered, this, [this] { addCompilerManually(); });
	menu->addAction(act1);
	btnAddCompiler->setMenu(menu);

    connect(btnAddCompiler, &QToolButton::clicked, this, [this] { addCompiler(); });

	connect(btn_esql_mysql_x64_rt_path, &QPushButton::clicked, this, [this] { choosePath(esql_mysql_x64_rt_path, true); });
	connect(btn_esql_mysql_x86_rt_path, &QPushButton::clicked, this, [this] { choosePath(esql_mysql_x86_rt_path, true); });

	connect(btn_esql_odbc_x64_rt_path, &QPushButton::clicked, this, [this] { choosePath(esql_odbc_x64_rt_path, true); });
	connect(btn_esql_odbc_x86_rt_path, &QPushButton::clicked, this, [this] { choosePath(esql_odbc_x86_rt_path, true); });

	connect(btn_esql_pgsql_x64_rt_path, &QPushButton::clicked, this, [this] { choosePath(esql_pgsql_x64_rt_path, true); });
	connect(btn_esql_pgsql_x86_rt_path, &QPushButton::clicked, this, [this] { choosePath(esql_pgsql_x86_rt_path, true); });
	
	connect(btn_esql_pp_new, &QPushButton::clicked, this, [this] { UiUtils::InfoDialog(tr("Function not implemented")); });

	connect(cbReleaseCompiler, qOverload<int>(&QComboBox::currentIndexChanged), this, [this](int i){ updateCompilerInfo(0, i); });
	connect(cbDebugCompiler, qOverload<int>(&QComboBox::currentIndexChanged), this, [this](int i){ updateCompilerInfo(1, i); });
}

void SettingsDialog::updateCompilerInfo(int t, int idx)
{
	QString id;
	switch (t) {
		case 0:
			id = cbReleaseCompiler->itemData(idx).toString();
			break;
		case 1:
			id = cbDebugCompiler->itemData(idx).toString();
			break;
	}

	if (id.isEmpty())
		return;

	QMap<QString, CompilerDefinition *> compilers = GixGlobals::getCompilerManager()->getCompilers();
	if (!compilers.contains(id))
		return;

	CompilerDefinition *cdef = compilers.value(id);
	switch (t) {
		case 0:
			lblReleaseCompilerBaseDir->setText(cdef->getHomedir());
			lblReleaseCompilerDescription->setText(cdef->getName());
			break;
		case 1:
			lblDebugCompilerBaseDir->setText(cdef->getHomedir());
			lblDebugCompilerDescription->setText(cdef->getName());
			break;
	}
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
	updateCompilerInfo(0, idx);

	idx = cbDebugCompiler->findData(settings.value("DebugCompilerId", ""));
	cbDebugCompiler->setCurrentIndex((idx >= 0) ? idx : 0);
	updateCompilerInfo(1, idx);
}

bool SettingsDialog::GnuCobolCfgTab_CheckSettings()
{
	return true;
}

bool SettingsDialog::GeneralCfgTab_SaveSettings()
{
	QSettings settings;

	bool changed = settingsSetValue("Ide_DebugOutput", cbDebugOutput->isChecked());
	changed |= settingsSetValue("cobc_default_source_format", cbDefaultSourceFormat->currentData().toString());
	changed = settingsSetValue("ide_save_before_build", cbSaveBeforeBuild->isChecked());
	return changed;
}

void SettingsDialog::GeneralCfgTab_LoadSettings()
{
	QSettings settings;
	int idx = 0;

	cbDebugOutput->setChecked(settings.value("Ide_DebugOutput", false).toBool());
	idx = cbDefaultSourceFormat->findData(settings.value("cobc_default_source_format", "fixed"));
	cbDefaultSourceFormat->setCurrentIndex((idx >= 0) ? idx : 0);
	cbSaveBeforeBuild->setChecked(settings.value("ide_save_before_build", true).toBool());
}

bool SettingsDialog::GeneralCfgTab_CheckSettings()
{
	return true;
}

bool SettingsDialog::ESQLCfgTab_SaveSettings()
{
	QSettings settings;
	bool changed = false;
	changed |= settingsSetValue("esql_mysql_x64_rt_path", esql_mysql_x64_rt_path->text());
	changed |= settingsSetValue("esql_mysql_x86_rt_path", esql_mysql_x86_rt_path->text());

	changed |= settingsSetValue("esql_odbc_x64_rt_path", esql_odbc_x64_rt_path->text());
	changed |= settingsSetValue("esql_odbc_x86_rt_path", esql_odbc_x86_rt_path->text());

	changed |= settingsSetValue("esql_pgsql_x64_rt_path", esql_pgsql_x64_rt_path->text());
	changed |= settingsSetValue("esql_pgsql_x86_rt_path", esql_pgsql_x86_rt_path->text());

	changed |= settingsSetValue("esql_preprocessor_id", cb_esql_pp_driver_list->currentData().toString());

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
	cbScreenRes->setCurrentText(settings.value("screen_resolution", 0).toString());
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
	changed |= settingsSetValue("screen_resolution", cbScreenRes->currentData().toInt());
	return changed;
}

void SettingsDialog::DebugCfgTab_LoadSettings()
{
	QSettings settings;
	cbDebugEngine->setCurrentIndex(cbDebugEngine->findData(settings.value("debugger_engine", (int)DebugDriverType::Standard).toInt()));
	txtDbgrBindingAddress->setText(settings.value("debugger_client_addr", DBGR_LOCAL_BINDING_DEFAULT_ADDR).toString());
	txtDbgrBindingPort->setText(settings.value("debugger_client_port", DBGR_LOCAL_BINDING_DEFAULT_PORT).toString());
	cbDbgrOpensWindow->setChecked(settings.value("debugger_host_new_window", false).toBool());
}

bool SettingsDialog::DebugCfgTab_CheckSettings()
{
	bool port_ok, addr_ok;
	QHostAddress addr;
	int port = txtDbgrBindingPort->text().toInt(&port_ok, 10);
	addr_ok = addr.setAddress(txtDbgrBindingAddress->text());
	return (addr_ok && port_ok && port > 0 && port <= 65535);
}

bool SettingsDialog::DebugCfgTab_SaveSettings()
{
	QSettings settings;
	bool changed = settingsSetValue("debugger_engine", cbDebugEngine->currentData().toInt());
	changed |= settingsSetValue("debugger_client_addr", txtDbgrBindingAddress->text());
	changed |= settingsSetValue("debugger_client_port", txtDbgrBindingPort->text().toInt());
	changed |= settingsSetValue("debugger_host_new_window", cbDbgrOpensWindow->isChecked());
	return changed;
}

void SettingsDialog::ESQLCfgTab_LoadSettings()
{
	QSettings settings;

	esql_mysql_x64_rt_path->setText(settings.value("esql_mysql_x64_rt_path", "").toString());
	esql_mysql_x86_rt_path->setText(settings.value("esql_mysql_x86_rt_path", "").toString());

	esql_odbc_x64_rt_path->setText(settings.value("esql_odbc_x64_rt_path", "").toString());
	esql_odbc_x86_rt_path->setText(settings.value("esql_odbc_x86_rt_path", "").toString());

	esql_pgsql_x64_rt_path->setText(settings.value("esql_pgsql_x64_rt_path", "").toString());
	esql_pgsql_x86_rt_path->setText(settings.value("esql_pgsql_x86_rt_path", "").toString());

	QString esql_pp_driver_id = settings.value("esql_preprocessor_id", ESQLConfigurationType::GixInternal).toString();
	if (esql_pp_driver_id == ESQLConfigurationType::GixInternal)
		cb_esql_pp_driver_list->setCurrentIndex(0);
	else
		if (esql_pp_driver_id == ESQLConfigurationType::GixExternal)
			cb_esql_pp_driver_list->setCurrentIndex(1);
		else {
			int idx = cb_esql_pp_driver_list->findData(esql_pp_driver_id);
			if (idx > 1)
				cb_esql_pp_driver_list->setCurrentIndex(idx);
			else
				cb_esql_pp_driver_list->setCurrentIndex(0);

		}
}

bool SettingsDialog::ESQLCfgTab_CheckSettings()
{
	return
		(esql_mysql_x64_rt_path->text().isEmpty() || QDir(esql_mysql_x64_rt_path->text()).exists()) &&
		(esql_mysql_x86_rt_path->text().isEmpty() || QDir(esql_mysql_x86_rt_path->text()).exists()) &&

		(esql_odbc_x64_rt_path->text().isEmpty() || QDir(esql_odbc_x64_rt_path->text()).exists()) &&
		(esql_odbc_x86_rt_path->text().isEmpty() || QDir(esql_odbc_x86_rt_path->text()).exists()) &&

		(esql_pgsql_x64_rt_path->text().isEmpty() || QDir(esql_pgsql_x64_rt_path->text()).exists()) &&
		(esql_pgsql_x86_rt_path->text().isEmpty() || QDir(esql_pgsql_x86_rt_path->text()).exists());
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
	AddCompilerWizard *wizard = new AddCompilerWizard(this);
	wizard->exec();

	if (!wizard->new_compiler_id.isEmpty()) {
		GixGlobals::getCompilerManager()->init();
		initCompilers();
		GnuCobolCfgTab_LoadSettings();
	}
}

void SettingsDialog::addCompilerManually()
{
	std::string basedir;
	bool b;
	AddCompilerDialog *adlg = new AddCompilerDialog(this);

	if (adlg->exec()) {
		GixGlobals::getCompilerManager()->init();
		initCompilers();
		GnuCobolCfgTab_LoadSettings();
	}

}
