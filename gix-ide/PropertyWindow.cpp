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

#include "PropertyWindow.h"
#include "ElidedLabel.h"
#include "ProtectedLineEdit.h"
#include "CustomDialog.h"
#include "PropertyOptionsDialog.h"
#include "SysUtils.h"
#include "UiUtils.h"
#include "IdeTaskManager.h"
#include "NoWheelComboBox.h"

#include <QToolBar>
#include <QLabel>
#include <QLineEdit>
#include <QComboBox>
#include <QHeaderView>
#include <QBoxLayout>
#include <QPushButton>
#include <QFileDialog>
#include <QDataStream>

#include <Ide.h>

PropertyWindow::PropertyWindow(QWidget* parent, MainWindow* mw) : QMainWindow(parent)
{
	this->item = nullptr;
	this->setWindowTitle("Properties");
	this->setMinimumWidth(300);
	this->setWindowFlags(Qt::Widget); // <---------
	QToolBar* toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

	this->propertyTable = new QTableWidget(0, 2, this);
	this->setCentralWidget(propertyTable);

	propertyTable->setRowCount(0);
	QHeaderView* header = propertyTable->horizontalHeader();
	header->setSectionResizeMode(QHeaderView::Stretch);
	header->hide();

	connect(Ide::TaskManager(), &IdeTaskManager::SettingsChanged, this, &PropertyWindow::refreshContent);

	connect(Ide::TaskManager(), &IdeTaskManager::projectCollectionClosed, this, [this]() {
		setContent(nullptr);
	});
}


PropertyWindow::~PropertyWindow()
{}

void PropertyWindow::refreshContent()
{
	setContent(this->item);
}

void PropertyWindow::setContent(ProjectItem* pi)
{
	this->item = pi;

	propertyTable->clear();
	propertyTable->setRowCount(0);

	PropertySource* ps = dynamic_cast<PropertySource*>(pi);
	if (ps == nullptr)
		return;

	PropertyDefinitionCollection prop_defs = ps->PropertyGetDefinitions();
	if (prop_defs.size() == 0)
		return;

	QMap<QString, QVariant>* cur_values = ps->PropertyGetCurrentValues();

	propertyTable->setRowCount(prop_defs.size() + prop_defs.getGroupCount());
	propertyTable->setColumnCount(2);

	QHeaderView* header = propertyTable->horizontalHeader();
	header->setSectionResizeMode(QHeaderView::Stretch);
	header->setSizeAdjustPolicy(QAbstractScrollArea::AdjustToContents);
	header->hide();

	QHeaderView* vheader = propertyTable->verticalHeader();
	vheader->hide();

	vheader->setDefaultSectionSize(UiUtils::computeFontSize(this, 22));

	QString curr_group;

	int table_row = 0;
	auto props = prop_defs.enumerateByGroup();
	for (int i = 0; i < props.size(); i++) {
		PropertyDefinition* pd = props.at(i);
		if (!pi->isPropertyVisible(pd)) {
			propertyTable->setRowCount(propertyTable->rowCount() - 1);
			continue;
		}

		if (pd->Group != curr_group) {
			QTableWidgetItem* th = new QTableWidgetItem();
			th->setText(prop_defs.getPropertyGroupName(pd->Group));
			//QFont f = th->font();
			QFont f = getGridFont();
			f.setBold(true);
			th->setFont(f);
			th->setBackgroundColor(QColor(224, 224, 224));
			propertyTable->setItem(table_row, 0, th);
			propertyTable->setSpan(table_row, 0, 1, 2);
			table_row++;
			curr_group = pd->Group;
		}

		QVariant cur_property_value = cur_values->contains(pd->Name) ? cur_values->value(pd->Name) : pd->DefaultValue;
		QTableWidgetItem* tw = new QTableWidgetItem();
		tw->setText(pd->Description);
		tw->setFlags(tw->flags() & ~Qt::ItemIsEditable);

		QFont gridFont = getGridFont();
		tw->setFont(gridFont);

		QWidget* qw = nullptr;
		switch (pd->Type) {
			case PropertyTypeText:
			{
				if (!pd->ReadOnly) {
					ProtectedLineEdit* qle = new ProtectedLineEdit(propertyTable);
					connect(qle, &ProtectedLineEdit::textChanged, [this, qle, pd, pi] { propertyValueChanged(pd, qle->text(), pi);  });
					qle->setText(pd->serialize(cur_property_value));
					qle->setFont(gridFont);
					qle->setStyleSheet("ProtectedLineEdit { padding-left: 3px }");
					qw = qle;
				}
				else {
					QLabel* qle = new QLabel(propertyTable);
					qle->setText(pd->serialize(cur_property_value));
					qle->setFont(gridFont);
					qle->setStyleSheet("QLabel { padding-left: 3px }");
					qw = qle;
				}
			}
			break;

			case PropertyTypeOption:
				if (pd->Options != nullptr && pd->Options->size() > 0) {
					QComboBox* cb = new NoWheelComboBox(propertyTable);
					cb->setFocusPolicy(Qt::StrongFocus);
					QMap<QString, QVariant>::iterator it;
					for (it = pd->Options->begin(); it != pd->Options->end(); ++it) {
						cb->addItem(it.value().toString(), it.key());
					}
					cb->setCurrentIndex(cb->findData(cur_property_value));
					cb->setMaximumWidth(145);
					cb->setEnabled(!pd->ReadOnly);
					cb->setFont(gridFont);
					cb->setStyleSheet("QComboBox { padding-left: 3px }");
					connect(cb, &QComboBox::currentTextChanged, [this, cb, pd, pi] { propertyValueChanged(pd, cb->currentData(), pi);   });
					qw = cb;
				}
				break;

			case PropertyTypeBoolean:
			{
				QComboBox* cb = new NoWheelComboBox(propertyTable);
				cb->setFocusPolicy(Qt::StrongFocus);
				cb->addItem(tr("Yes"), true);
				cb->addItem(tr("No"), false);
				cb->setCurrentIndex(cb->findData(cur_property_value));
				cb->setMaximumWidth(145);
				cb->setEnabled(!pd->ReadOnly);
				cb->setFont(gridFont);
				cb->setStyleSheet("QComboBox { padding-left: 3px }");
				connect(cb, &QComboBox::currentTextChanged, [this, cb, pd, pi] { propertyValueChanged(pd, cb->currentData(), pi);   });
				qw = cb;
			}
			break;

			case PropertyTypeFilePath:
			{
				QHBoxLayout* layout = new QHBoxLayout(propertyTable);
				layout->setContentsMargins(0, 0, 0, 0);
				layout->setAlignment(Qt::AlignJustify);
				QLabel* qle = new QLabel(propertyTable);
				qle->setText(pd->serialize(cur_property_value));
				qle->setFont(gridFont);
				qle->setStyleSheet("QLabel { padding-left: 3px }");
				QPushButton* qpb = new QPushButton("...", propertyTable);
				connect(qpb, &QPushButton::clicked, this, [this, pd, ps, pi, qle] {
					auto cvs = ps->PropertyGetCurrentValues();
					QVariant v = cvs->contains(pd->Name) ? cvs->value(pd->Name) : pd->DefaultValue;
					filePathEditButtonClicked(pd, v, pi, qle);
				});
				qpb->setFixedSize(QSize(18, 18));
				qpb->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
				qle->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
				qle->setMaximumWidth(120);
				layout->addWidget(qle, 90);
				layout->addWidget(qpb, 10);
				QWidget* qqw = new QWidget(propertyTable);
				qqw->setLayout(layout);
				qw = qqw;
			}
			break;

			case PropertyTypeDirPath:
			{
				QHBoxLayout* layout = new QHBoxLayout(propertyTable);
				layout->setContentsMargins(0, 0, 0, 0);
				layout->setAlignment(Qt::AlignJustify);
				QLabel* qle = new QLabel(propertyTable);
				qle->setText(pd->serialize(cur_property_value));
				qle->setFont(gridFont);
				qle->setStyleSheet("QLabel { padding-left: 3px }");
				QPushButton* qpb = new QPushButton("...", propertyTable);
				connect(qpb, &QPushButton::clicked, this, [this, pd, ps, pi, qle] {
					auto cvs = ps->PropertyGetCurrentValues();
					QVariant v = cvs->contains(pd->Name) ? cvs->value(pd->Name) : pd->DefaultValue;
					dirPathEditButtonClicked(pd, v, pi, qle);
				});
				qpb->setFixedSize(QSize(18, 18));
				qpb->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
				qle->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
				qle->setMaximumWidth(120);
				layout->addWidget(qle, 90);
				layout->addWidget(qpb, 10);
				QWidget* qqw = new QWidget(propertyTable);
				qqw->setLayout(layout);
				qw = qqw;
			}
			break;

			case PropertyTypeDirPathList:
			{
				QHBoxLayout* layout = new QHBoxLayout(propertyTable);
				layout->setContentsMargins(0, 0, 0, 0);
				layout->setAlignment(Qt::AlignJustify);
				QLabel* qle = new QLabel(propertyTable);
				qle->setText(pd->serialize(cur_property_value));
				qle->setFont(gridFont);
				qle->setStyleSheet("QLabel { padding-left: 3px }");
				QPushButton* qpb = new QPushButton("...", propertyTable);
				connect(qpb, &QPushButton::clicked, this, [this, pd, ps, pi, qle] {
					auto cvs = ps->PropertyGetCurrentValues();
					QVariant v = cvs->contains(pd->Name) ? cvs->value(pd->Name) : pd->DefaultValue;
					dirPathListEditButtonClicked(pd, v, pi, qle);
				});
				qpb->setFixedSize(QSize(18, 18));
				qpb->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
				qle->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
				qle->setMaximumWidth(120);
				layout->addWidget(qle, 90);
				layout->addWidget(qpb, 10);
				QWidget* qqw = new QWidget(propertyTable);
				qqw->setLayout(layout);
				qw = qqw;
			}
			break;

			case PropertyTypeEnvVarsList:
			{
				QHBoxLayout* layout = new QHBoxLayout(propertyTable);
				layout->setContentsMargins(0, 0, 0, 0);
				layout->setAlignment(Qt::AlignJustify);
				QLabel* qle = new QLabel(propertyTable);
				qle->setText(pd->serialize(cur_property_value));
				qle->setFont(gridFont);
				qle->setStyleSheet("QLabel { padding-left: 3px }");
				QPushButton* qpb = new QPushButton("...", propertyTable);
				connect(qpb, &QPushButton::clicked, this, [this, ps, pd, pi, qle] {
					auto cvs = ps->PropertyGetCurrentValues();
					QVariant v = cvs->contains(pd->Name) ? cvs->value(pd->Name) : pd->DefaultValue;
					envVarsListEditButtonClicked(pd, v, pi, qle);
				});
				qpb->setFixedSize(QSize(18, 18));
				qpb->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
				qle->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
				qle->setMaximumWidth(120);
				layout->addWidget(qle, 90);
				layout->addWidget(qpb, 10);
				QWidget* qqw = new QWidget(propertyTable);
				qqw->setLayout(layout);
				qw = qqw;
			}
			break;

			case PropertyTypeConditionalPropertySet:
				QHBoxLayout* layout = new QHBoxLayout(propertyTable);
				layout->setContentsMargins(0, 0, 0, 0);
				layout->setAlignment(Qt::AlignJustify);

				QScopedPointer<QVariantMap> sub_props(SysUtils::deserializeMap(cur_property_value.toString()));
				QComboBox* cb = new NoWheelComboBox(propertyTable);
				cb->setFocusPolicy(Qt::StrongFocus);
				QPushButton* qpb = new QPushButton("...", propertyTable);

				cb->addItem(tr("Yes"), true);
				cb->addItem(tr("No"), false);
				cb->setCurrentIndex(cb->findData(sub_props->value("enabled").toBool()));
				cb->setMaximumWidth(145);
				cb->setEnabled(!pd->ReadOnly);
				cb->setFont(gridFont);
				cb->setStyleSheet("QComboBox { padding-left: 3px }");

				connect(cb, &QComboBox::currentTextChanged, [this, cb, pd, pi, ps, qpb, cur_property_value] {
					QScopedPointer<QVariantMap> sub_props(SysUtils::deserializeMap(cur_property_value.toString()));
					sub_props->insert("enabled", cb->currentData());
					ps->PropertySetValue(pd->Name, SysUtils::serializeMap(sub_props.get()));
					qpb->setEnabled(sub_props->value("enabled").toBool());
				});

				connect(qpb, &QPushButton::clicked, this, [this, ps, pd, pi, cur_property_value] {
					QScopedPointer<QVariantMap> sub_props(SysUtils::deserializeMap(cur_property_value.toString()));

					QVariantMap *ptr = sub_props.get();
					QString dialog_type = sub_props->value("dialog_type").toString();
					//QScopedPointer<PropertyOptionsDialog> dlg(PropertyOptionsDialog::get(dialog_type));
					//if (dlg && dlg->show(sub_props.get())) {
					//	ps->PropertySetValue(pd->Name, SysUtils::serializeMap(sub_props.get()));
					//}
					PropertyOptionsDialog* dlg = PropertyOptionsDialog::get(dialog_type, mainWindow);
					if (dlg) {
						bool rc = dlg->show(pi, pd);
					}
				});
				qpb->setFixedSize(QSize(18, 18));
				qpb->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
				qpb->setEnabled(sub_props->value("enabled").toBool());
				layout->addWidget(cb, 90);
				layout->addWidget(qpb, 10);
				QWidget* qqw = new QWidget(propertyTable);
				qqw->setLayout(layout);
				qw = qqw;
				break;
		}

		if (qw != nullptr) {
			propertyTable->setCellWidget(table_row, 1, qw);
		}
		propertyTable->setItem(table_row, 0, tw);
		table_row++;
	}


}

void PropertyWindow::envVarsListEditButtonClicked(PropertyDefinition* pd, QVariant value, ProjectItem* pi, QWidget* prop_visual)
{
	CustomDialog cd(pd->Description);
	QStringList l = value.toStringList();
	l.removeAll(QString(""));
	cd.addEditableStringList("", "", &l);
	if (cd.exec()) {
		QVariant out_value(l);
		propertyValueChanged(pd, out_value, pi);
		QLabel* lbl = dynamic_cast<QLabel*>(prop_visual);
		if (lbl != nullptr) {
			lbl->setText(pd->serialize(out_value));
		}
	}
}

void PropertyWindow::dirPathListEditButtonClicked(PropertyDefinition* pd, QVariant value, ProjectItem* pi, QWidget* prop_visual)
{
	CustomDialog cd(pd->Description);
	QStringList l = value.toStringList();
	l.removeAll(QString(""));
	cd.addEditablePathList("", "", &l);
	if (cd.exec()) {
		QVariant out_value(l);
		propertyValueChanged(pd, out_value, pi);
		QLabel* lbl = dynamic_cast<QLabel*>(prop_visual);
		if (lbl != nullptr) {
			lbl->setText(pd->serialize(out_value));
		}
	}
}

void PropertyWindow::dirPathEditButtonClicked(PropertyDefinition* pd, QVariant value, ProjectItem* pi, QWidget* prop_visual)
{
	QFileDialog dialog;
	dialog.setFileMode(QFileDialog::Directory);
	dialog.setOption(QFileDialog::ShowDirsOnly);
	if (dialog.exec()) {
		QString directory = dialog.selectedFiles()[0];
		propertyValueChanged(pd, directory, pi);
		QLabel* lbl = dynamic_cast<QLabel*>(prop_visual);
		if (lbl != nullptr) {
			lbl->setText(pd->serialize(directory));
		}
	}
}

void PropertyWindow::filePathEditButtonClicked(PropertyDefinition *pd, QVariant value, ProjectItem* pi, QWidget* prop_visual)
{
	QFileDialog dialog;
	dialog.setFileMode(QFileDialog::FileMode::AnyFile);
	dialog.setAcceptMode(QFileDialog::AcceptMode::AcceptSave);
	if (dialog.exec()) {
		QString file = dialog.selectedFiles()[0];
		propertyValueChanged(pd, file, pi);
		QLabel *lbl = dynamic_cast<QLabel *>(prop_visual);
		if (lbl != nullptr) {
			lbl->setText(pd->serialize(file));
		}
	}
}

void PropertyWindow::propertyValueChanged(PropertyDefinition* pd, QVariant value, ProjectItem* pi)
{
	PropertySource* ips = dynamic_cast<PropertySource*>(pi);
	if (!ips)
		return;

	ips->PropertySetValue(pd->Name, value);

	IPersistableProjectItem* ipp = dynamic_cast<IPersistableProjectItem*>(pi);
	ipp->setDirty(true);

	if (ipp != nullptr)
		ipp->save();

	emit notifyPropertyValueChanged(pd, value, pi);
}

QFont PropertyWindow::getGridFont()
{
	QSettings settings;
	QTableWidgetItem* tw = new QTableWidgetItem();
	QString font_name = settings.value("grid_font_name", "").toString();
	int font_size = settings.value("grid_font_size", 0).toInt();
	if (font_name.isEmpty() || font_size == 0) {
		QTableWidgetItem tw;
		font_name = tw.font().family();
		font_size = tw.font().pointSize();
	}

	return QFont(font_name, font_size);
}

