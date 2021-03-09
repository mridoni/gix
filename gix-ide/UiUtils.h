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

#include <QString>
#include <QTreeView>

#define DEFAULT_TREEVIEW_FONT_NAME	"Courier New"
#define DEFAULT_TREEVIEW_FONT_SIZE	8

class UiUtils
{
public:
	UiUtils();
	~UiUtils();

	static void ErrorDialog(QString msg);
	static void InfoDialog(QString msg);
	static bool YesNoDialog(QString msg);


	static int OnPlatform(int _win, int _linux, int _osx);
	static QString OnPlatform(QString _win, QString _linux, QString _osx);
	static int computeFontSize(QWidget *w, int v);

	static void setTreeViewFont(QTreeView *tv);

    static void dispatchToMainThread(std::function<void()> callback);

	static bool isOceCompatModeEnabled();
};

