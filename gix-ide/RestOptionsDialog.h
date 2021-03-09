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

#pragma once
#include "PropertyOptionsDialog.h"

#include "ui_RestOptions.h"

class ProjectFile;

class RestOptionsDialog :
	public PropertyOptionsDialog, private Ui::Ui_RestOptions
{
public:
	RestOptionsDialog(QMainWindow* mw = 0);
	~RestOptionsDialog();

	// Inherited via PropertyOptionsDialog
	virtual bool show(ProjectItem* pi, PropertyDefinition* pd) override;
	virtual void accept() override;
	virtual void cancel() override;

private:
	QStringList get_itf_candidates_from_copy(ProjectFile *pf);
	void init_itf_fields(const QComboBox &cbcopy, int copyidx, QComboBox &cbfld);
};

