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

#include "ErrorWindow.h"
#include "UiUtils.h"
#include "Ide.h"
#include "IdeTaskManager.h"
#include "ide_sink.h"

#include <QHeaderView>
#include <QLabel>
#include <QPixmap>

#define COL_TYPE	0
#define COL_MSG		1
#define COL_FILE	2
#define COL_LINE	3
#define COL_PAR_SEC	4

ErrorWindow::ErrorWindow(QWidget* parent, MainWindow* mw) : QMainWindow(parent)
{
	this->setWindowTitle("Errors/Warnings");
	this->setMinimumHeight(100);
	this->setMinimumWidth(250);
	this->setWindowFlags(Qt::Widget); // <---------
	toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

	err_grid = new QTableWidget(this);
	err_grid->setColumnCount(5);

	

	//err_grid->setHorizontalHeaderLabels(QStringList() << "Type" << "Message" << "Filename" << "Line" << "Paragraph/Section");

	auto c0 = new QTableWidgetItem("Type");
	err_grid->setHorizontalHeaderItem(0, c0);
	err_grid->verticalHeader()->hide();

	QHeaderView* header = err_grid->horizontalHeader();
	header->setSizeAdjustPolicy(QAbstractScrollArea::AdjustIgnored);
	header->setSectionResizeMode(COL_TYPE, QHeaderView::Fixed);
	header->setSectionResizeMode(COL_MSG, QHeaderView::Stretch);
	header->setSectionResizeMode(COL_FILE, QHeaderView::Stretch);
	header->setSectionResizeMode(COL_LINE, QHeaderView::Fixed);
	header->setSectionResizeMode(COL_PAR_SEC, QHeaderView::Stretch);

	err_grid->setColumnWidth(COL_TYPE, 12);
	err_grid->setColumnWidth(COL_MSG, 300);
	err_grid->setColumnWidth(COL_LINE, 20);

	err_grid->setIconSize(QSize(12,12));
	
	err_grid->setSelectionMode(QAbstractItemView::SelectionMode::SingleSelection);
	err_grid->setSelectionBehavior(QAbstractItemView::SelectionBehavior::SelectRows);

	connect(err_grid, &QTableWidget::doubleClicked, this, [this](const QModelIndex& mi) {
		errorListDoubleClicked(mi);
	});

	this->setCentralWidget(err_grid);

}

ErrorWindow::~ErrorWindow()
{
	delete(toolBar);
}
void ErrorWindow::clear()
{
	err_grid->clearContents();
	entries.clear();
}
void ErrorWindow::addEntries(QList<ErrorWarningFilterEntry> _entries)
{
	for (auto e : _entries) {
		entries.append(e);
	}
}

QLabel *ErrorWindow::get_label(QString t, QString tt)
{
	QLabel* qle = new QLabel(err_grid);
	qle->setText(t);
	if (!tt.isEmpty())
		qle->setToolTip(tt);

	QFont gridFont = Ide::getGridFont();
	gridFont.setPointSizeF(8);

	qle->setFont(gridFont);
	qle->setStyleSheet("QLabel { padding: 2px }");

	return qle;
}

void ErrorWindow::updateErrorList()
{
	QString cur_par_or_sec;
	int cur_line = 0;

	QList<ErrorWarningFilterEntry> new_entries;
	err_grid->clear();

	// Process section/paragraph headers
	for (ErrorWarningFilterEntry e : entries) {
		switch (e.type) {

			case ErrorWarningFilterType::SectionHeader:
			case ErrorWarningFilterType::ParagrapHeader:
				cur_par_or_sec = e.section_or_paragraph;
				cur_line = e.line;
				continue;

			default:
				if (cur_line == 0) {
					cur_line = e.line;
				} else {
					if (e.line != cur_line)
						cur_par_or_sec = "";
				}

				e.section_or_paragraph = cur_par_or_sec;
				new_entries.append(e);
		}
	}

	err_grid->setRowCount(new_entries.size());
	for (int i = 0; i < new_entries.size(); i++) {
		auto e = new_entries[i];

		QLabel* icon_item = new QLabel();
		icon_item->setPixmap(QPixmap(e.type == ErrorWarningFilterType::Error ? ":/icons/error.png" : ":/icons/warning.png"));
		icon_item->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
		err_grid->setCellWidget(i, COL_TYPE, icon_item);

		//QTableWidgetItem* t1 = new QTableWidgetItem(e.message);
		//QTableWidgetItem* t2 = new QTableWidgetItem(e.filename);
		//QTableWidgetItem* t3 = new QTableWidgetItem(QString::number(e.line));
		//QTableWidgetItem* t4 = new QTableWidgetItem(e.section_or_paragraph);

		err_grid->setCellWidget(i, COL_MSG, get_label(e.message, e.message));
		err_grid->setCellWidget(i, COL_FILE, get_label(e.filename, e.filename));
		err_grid->setCellWidget(i, COL_LINE, get_label(QString::number(e.line)));
		err_grid->setCellWidget(i, COL_PAR_SEC, get_label(e.section_or_paragraph, e.section_or_paragraph));
	
		//QVariant v;
		//v.setValue<void*>((void*)&e);
		//err_grid->cellWidget(i, COL_TYPE)->setProperty("*", v);

		//th->setBackgroundColor(QColor(224, 224, 224));
		//propertyTable->setItem(table_row, 0, th);
		//propertyTable->setSpan(table_row, 0, 1, 2);
		//table_row++;
		//curr_group = pd->Group;
	}

	entries = new_entries;

}

void ErrorWindow::errorListDoubleClicked(const QModelIndex& mi)
{
	auto e = entries.at(mi.row());
	Ide::TaskManager()->gotoFileLine(e.filename, e.line, true);
}