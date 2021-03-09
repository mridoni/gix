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

#include "RestOptionsDialog.h"
#include "Project.h"
#include <SysUtils.h>
#include <UiUtils.h>
#include <QtGlobal>

#include "linq/linq.hpp"

RestOptionsDialog::RestOptionsDialog(QMainWindow* mw) : PropertyOptionsDialog(mw)
{
	setupUi(this);
	this->setAttribute(Qt::WA_DeleteOnClose);
}


RestOptionsDialog::~RestOptionsDialog()
{
}

void RestOptionsDialog::init_itf_fields(const QComboBox& cbcopy, int copyidx, QComboBox &cbfld)
{
	cbfld.clear();
	cbfld.addItem("(" + tr("none") + ")", QVariant());
	if (copyidx > 0) {
		ProjectFile *pf = (ProjectFile *)cbcopy.currentData().value<void *>();
		QStringList itf_candidates = get_itf_candidates_from_copy(pf);
		if (itf_candidates.size())
			cbfld.addItems(itf_candidates);
	}
}

bool RestOptionsDialog::show(ProjectItem* _pi, PropertyDefinition* _pd)
{
	if (!init(_pi, _pd))
		return false;

	ProjectFile* pf = dynamic_cast<ProjectFile*>(_pi);
	Project* prj = pf->getParentProject();
	if (!prj)
		return false;

	auto files = prj->getAllCopyFiles();
	
	cbInputCopy->clear();
	cbOutputCopy->clear();
	cbInputCopy->addItem("(" + tr("none") + ")", QVariant());
	cbOutputCopy->addItem("(" + tr("none") + ")", QVariant());
	for (int i = 0; i < files.size(); i++) {
		ProjectFile* f = files.at(i);
		cbInputCopy->addItem(f->GetDisplayName(), QVariant::fromValue<void *>(f));
		cbOutputCopy->addItem(f->GetDisplayName(), QVariant::fromValue<void*>(f));
	}

	if (sub_properties->contains("interface_in_copy")) {
		int i = cbInputCopy->findText(sub_properties->value("interface_in").toString());
		if (i != -1)
			cbInputCopy->setCurrentIndex(i);
	}

	if (sub_properties->contains("interface_out_copy")) {
		int i = cbOutputCopy->findText(sub_properties->value("interface_out").toString());
		if (i != -1)
			cbOutputCopy->setCurrentIndex(i);
	}

	if (sub_properties->contains("port")) {
		tbPort->setText(sub_properties->value("port").toString());
	}

	if (sub_properties->contains("url")) {
		tbUrl->setText(sub_properties->value("url").toString());
	}

	connect(cbInputCopy, qOverload<int>(&QComboBox::currentIndexChanged), this, [this](int idx) {
		init_itf_fields(*cbInputCopy, idx, *cbFieldIn);

	});

	connect(cbOutputCopy, qOverload<int>(&QComboBox::currentIndexChanged), this, [this](int idx) {
		init_itf_fields(*cbOutputCopy, idx, *cbFieldOut);
	});

	init_itf_fields(*cbInputCopy, cbInputCopy->currentIndex(), *cbFieldIn);
	init_itf_fields(*cbOutputCopy, cbOutputCopy->currentIndex(), *cbFieldOut);

	if (sub_properties->contains("interface_in_field")) {
		int i = cbFieldIn->findText(sub_properties->value("interface_in_field").toString());
		if (i != -1)
			cbFieldIn->setCurrentIndex(i);
	}

	if (sub_properties->contains("interface_out_field")) {
		int i = cbFieldOut->findText(sub_properties->value("interface_out_field").toString());
		if (i != -1)
			cbFieldOut->setCurrentIndex(i);
	}

	this->setModal(true);
	QWidget::show();
	return true;
}

void RestOptionsDialog::accept()
{
	bool ok;

	if (!cbInputCopy->currentData().isNull()) {
		ProjectFile* pf = (ProjectFile*)cbInputCopy->currentData().value<void*>();
		sub_properties->insert("interface_in_copy", pf->GetFileRelativePath());
	}

	if (!cbOutputCopy->currentData().isNull()) {
		ProjectFile* pf = (ProjectFile*)cbOutputCopy->currentData().value<void*>();
		sub_properties->insert("interface_out_copy", pf->GetFileRelativePath());
	}

	if (cbFieldIn->currentIndex() > 0) {
		sub_properties->insert("interface_in_field", cbFieldIn->currentText());
	}

	if (cbFieldOut->currentIndex() > 0) {
		sub_properties->insert("interface_out_field", cbFieldOut->currentText());
	}

	if (!tbPort->text().isEmpty()) {
		int port = tbPort->text().trimmed().toInt(&ok);
		if (ok && port > 0 && port < 65536) {
			sub_properties->insert("port", QString::number(port));
		}
		else {
			UiUtils::ErrorDialog(tr("Invalid value for field \"port\""));
			return;
		}
	}

	if (!tbUrl->text().isEmpty()) {
		sub_properties->insert("url", tbUrl->text().trimmed());
	}

	commitSubProperties();
		
	QDialog::accept();		
}

void RestOptionsDialog::cancel()
{
}

QStringList RestOptionsDialog::get_itf_candidates_from_copy(ProjectFile *pf)
{
	QStringList res;
	if (!pf || !QFile::exists(pf->GetFileFullPath()))
		return res;

	QStringList lines = SysUtils::FileReadAllLines(pf->GetFileFullPath());

	lines = QStringList::fromStdList(cpplinq::from(lines).where([](QString a) { return a.trimmed() != "" && a.length() > 7 && a[6] != '*'; }).to_list());
	for (QString ln : lines) {
		ln = ln.trimmed();
		if (ln.startsWith("01 ") && ln.length() > 3) {
			ln = ln.mid(3).trimmed();
			if (ln.endsWith("."))
				ln = ln.chopped(1).trimmed();

			res.append(ln);
		}
	}
	return res;
}
