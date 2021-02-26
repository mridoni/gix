#pragma once

#include "ScintillaEdit.h"
#include "EolMode.h"

#include <QEvent>
#include <QPainter>

#define DEFAULT_EDITOR_FONT_NAME	"Courier New"
#define DEFAULT_EDITOR_FONT_SIZE	10

class CodeEditor : public ScintillaEdit
{
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

private:
	int __maxLineNumberCharLength;
	int __lineNumberWidth;

	void setFontFromSettings();
	void showBookmarkAtLine(int ln);

private slots:
	void changed();
	void handleMarginClick(int position, int modifiers, int margin);

	void handleBreakPoint(int position, int modifiers);
	void handleBookmark(int position, int modifiers);
};
