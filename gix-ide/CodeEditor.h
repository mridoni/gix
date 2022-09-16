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

#include "ScintillaEdit.h"
#include "EolMode.h"
#include "EolMode.h"
#include "SourceFileFormat.h"

#include <QEvent>
#include <QPainter>

#define DEFAULT_EDITOR_FONT_NAME	"Courier New"
#define DEFAULT_EDITOR_FONT_SIZE	10

class CodeEditor : public ScintillaEdit
{
	Q_OBJECT

	friend class MainWindow;

public:
	CodeEditor(QWidget* parent = 0, int standard_init = 1);
	~CodeEditor();

	void paintEvent(QPaintEvent *event) override;

	QString toPlainText();
	void setPlainText(QString text);
	void ensureLineVisible(int ln);
	void highlightDebuggedLine(int);	
	QString selectedText();
	void toggleBreakpoint();
	void toggleBookmark();
	void clearAllBookmarks();
	bool isShowingSpecialChars();
	void showSpecialChars(bool b);
	void removeDebugMarkers();
	void highlightSymbol(int line, QString symbol_name);

	void setSourceFormat(SourceFileFormat sfmt);

signals:
	void handleTooltipOn(QString s, int x, int y, sptr_t pos);
	void handleTooltipOff(QString s, int x, int y, sptr_t pos);


private:
	int __maxLineNumberCharLength;
	int __lineNumberWidth;
	SourceFileFormat source_format = SourceFileFormat::Fixed;

	void setFontFromSettings();
	void showBookmarkAtLine(int ln);
	bool isCurrentlyCommented(QString s);


private slots:
	void changed();
	void handleMarginClick(int position, int modifiers, int margin);

	void handleBreakPoint(int position, int modifiers);
	void handleBookmark(int position, int modifiers);
};
