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

#include <QMap>
#include <QDialog>
#include <QVariant>
#include <QMainWindow>

#include "ProjectItem.h"

class PropertyOptionsDialog : public QDialog
{
	Q_OBJECT

public:
	PropertyOptionsDialog(QMainWindow *mw = 0);

	virtual bool show(ProjectItem* pi, PropertyDefinition* pd) = 0;
	virtual void accept() = 0;
	virtual void cancel() = 0;

	static PropertyOptionsDialog *get(QString, QMainWindow* mw);

protected:
	QScopedPointer<QVariantMap> sub_properties;

	bool init(ProjectItem* pi, PropertyDefinition* pd);
	bool commitSubProperties();

private:
	ProjectItem* item;
	PropertyDefinition* prop_def;
};

