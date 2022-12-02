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

#include <QTableWidget>

#include "MainWindow.h"
#include "PropertySource.h"

class QWidget;
class MainWindow;

class PropertyWindow : public QMainWindow
{
	Q_OBJECT

	friend class IdeTaskManager;

public:
	PropertyWindow(QWidget *parent, MainWindow *mw);
	~PropertyWindow();

public slots:
	//void setValue(int value);
	void setContent(ProjectItem *);
	void refreshContent();

signals:
	void notifyPropertyValueChanged(PropertyDefinition*, QVariant, ProjectItem* pi);

private:
	ProjectItem  *item;
	MainWindow * mainWindow;
	QTableWidget *propertyTable;
	
	void envVarsListEditButtonClicked(PropertyDefinition *, QVariant, ProjectItem *pi, QWidget *prop_visual);
	void dirPathListEditButtonClicked(PropertyDefinition *, QVariant, ProjectItem *pi, QWidget *prop_visual);
	void dirPathEditButtonClicked(PropertyDefinition *, QVariant, ProjectItem *pi, QWidget *prop_visual);
	void filePathEditButtonClicked(PropertyDefinition *, QVariant, ProjectItem *pi, QWidget *prop_visual);

	void propertyValueChanged(PropertyDefinition *, QVariant, ProjectItem *pi);
};

