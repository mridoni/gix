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

#include "CodeEditor.h"
#include "SciLexer.h"
#include "ScintillaEdit.h"
#include "IdeTaskManager.h"

#include <PathUtils.h>
#include <Ide.h>
#include <MdiChild.h>
#include <QSettings>
#include <QKeyEvent>
#include <QShortcut>

#ifndef RGB
#define RGB(r,g,b)  (r | (g << 8) | (b << 16))
#endif

#define MARGIN_INDICATORS			0
#define MARGIN_LINE_NUMBERS			1
#define MARGIN_SCRIPT_FOLD_INDEX	2

CodeEditor::CodeEditor(QWidget* parent, int default_initialization) : ScintillaEdit(parent)
{

	__lineNumberWidth = 0;
	__maxLineNumberCharLength = 0;

	if (default_initialization) {
		connect(this, &ScintillaEdit::notifyChange, this, &CodeEditor::changed);
		connect(this, &ScintillaEdit::marginClicked, this, &CodeEditor::handleMarginClick);
		//connect(this, &ScintillaEdit::updateUi, this, [this](int n) {

		setFontFromSettings();

		styleSetFore(SCE_C_IDENTIFIER, RGB(0x80, 0, 0x80));
		styleSetFore(SCE_C_NUMBER, RGB(0x00, 0, 0x80));
		styleSetFore(SCE_C_COMMENTDOC, RGB(0x00, 0x90, 0x80));
		styleSetFore(SCE_C_COMMENTLINE, RGB(0x00, 0x80, 0x00));
		styleSetFore(SCE_C_WORD, RGB(0xff, 0x00, 0x00));

		//styleSetBack(STYLE_DEFAULT, RGB(0xff, 0xf0, 0x00));
		QColor lc = QColor(Qt::yellow).lighter(160);
		setCaretLineBack(RGB(lc.red(), lc.green(), lc.blue()));

		setCaretLineVisible(true);
		setLexer(SCLEX_COBOL);

		setKeyWords(0, "accept access add address advancing after all alphabet alphabetic alphabetic-lower alphabetic-upper alphanumeric alphanumeric-edited also alter alternate and any apply are area areas ascending assign at author basis before beginning binary blank block bottom by call cancel cbl cd cf ch character characters class class-id clock-units close cobol code code-set collating column com-reg comma common communication comp comp-1 comp-2 comp-3 comp-4 comp-5 computational computational-1 computational-2 computational-3 computational-4 computational-5 compute configuration contains content continue control controls converting copy corr corresponding count currency data date-compiled date-written day day-of-week dbcs de debug-contents debug-item debug-line debug-name debug-sub-1 debug-sub-2 debug-sub-3 debugging decimal-point declaratives delete delimited delimiter depending descending destination detail display display-1 divide division down duplicates dynamic egcs egi eject else emi enable end end-add end-call end-compute end-delete end-divide end-evaluate end-if end-invoke end-multiply end-of-page end-perform end-read end-receive end-return end-rewrite end-search end-start end-string end-subtract end-unstring end-write ending enter entry environment eop equal error esi evaluate every exception exit extend external false fd file file-control filler final first footing for from function generate giving global go goback greater group headinhigh-valuhigh-values i-o i-o-control id identification if in index indexed indicate inherits initial initialize initiate input input-output insert inspect installation into invalid invoke is jusjustifie kanjke label last leading left length less limit limits linage linage-counter line line-counter lines linkage local-storage lock low-value low-values memory merge message metaclass method method-id mode modules more-labels move multiple multiply native native_binary negative next no not null nulls number numeric numeric-edited object object-computer occurs of off omitted on open optional or order organization other output overflow override packed-decimal padding page page-counter password perform pf ph pic picture plus pointer position positive printing procedure procedure-pointer procedures proceed processing program program-id purge queuquotquotes random rd read ready receive record recording records recursive redefines reel reference references relative release reload remainder removal renames replace replacing report reporting reports repository rerun reserve reset return return-code returning reversed rewind rewrite rf rh right rounded run same sd search section security segment segment-limit select self send sentence separate sequence sequential service set shift-in shift-out sign size skip1 skip2 skip3 sort sort-control sort-core-size sort-file-size sort-merge sort-message sort-mode-size sort-return source source-computer space spaces special-names standard standard-1 standard-2 start status stop string sub-queue-1 sub-queue-2 sub-queue-3 subtract sum super suppress symbolic sync synchronized table tally tallying tape terminal terminate test text than then through thru time times title to top trace trailing true type unit unstring until up upon usage use using valuvalue varying when when-compiled with words working-storage write write-only zero zeros");

		setMarginTypeN(MARGIN_INDICATORS, SC_MARGIN_COLOUR | SC_MARGIN_SYMBOL);
		setMarginBackN(MARGIN_INDICATORS, RGB(0xc0, 0xc0, 0xc0));
		setMarginWidthN(MARGIN_INDICATORS, 18);
		setMarginSensitiveN(MARGIN_INDICATORS, true);

		setMarginTypeN(MARGIN_LINE_NUMBERS, SC_MARGIN_NUMBER);
		setMarginWidthN(MARGIN_LINE_NUMBERS, 50);
		setMarginSensitiveN(MARGIN_LINE_NUMBERS, true);

		markerDefine(MRKR_DBG_CURLINE, SC_MARK_BACKGROUND);
		markerSetBack(MRKR_DBG_CURLINE, RGB(0x00, 0xc0, 0x00));
		markerSetAlpha(MRKR_DBG_CURLINE, 120);
		markerEnableHighlight(true);

		markerDefine(MRKR_BKP, SC_MARK_CIRCLE);
		markerSetBack(MRKR_BKP, RGB(0x90, 0x00, 0x00));
		markerSetFore(MRKR_BKP, RGB(0x00, 0x00, 0x00));

		markerDefine(MRKR_BOOKMARK, SC_MARK_LEFTRECT);
		markerSetBack(MRKR_BOOKMARK, RGB(0x00, 0x90, 0x00));
		markerSetFore(MRKR_BOOKMARK, RGB(0x90, 0x00, 0x90));

		setMarginMaskN(MARGIN_LINE_NUMBERS, 0);
		setMarginMaskN(MARGIN_INDICATORS, 1 << MRKR_BKP | 1 << MRKR_DBG_CURLINE | 1 << MRKR_BOOKMARK);

		// Tabs
		this->setTabWidth(4);
		this->setUseTabs(false);

		// Folding
		this->setProperty("fold", "1");
		this->setProperty("fold.compact", "0");
		this->setProperty("fold.flags", "17");

		//this->setMarginWidthN(MARGIN_SCRIPT_FOLD_INDEX, 0);
		this->setMarginTypeN(MARGIN_SCRIPT_FOLD_INDEX, SC_MARGIN_SYMBOL);
		this->setMarginMaskN(MARGIN_SCRIPT_FOLD_INDEX, SC_MASK_FOLDERS);
		this->setMarginWidthN(MARGIN_SCRIPT_FOLD_INDEX, 18);
		setMarginSensitiveN(MARGIN_SCRIPT_FOLD_INDEX, true);

		this->markerDefine(SC_MARKNUM_FOLDER, SC_MARK_PLUS);
		this->markerDefine(SC_MARKNUM_FOLDEROPEN, SC_MARK_MINUS);
		this->markerDefine(SC_MARKNUM_FOLDEREND, SC_MARK_EMPTY);
		this->markerDefine(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY);
		this->markerDefine(SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY);
		this->markerDefine(SC_MARKNUM_FOLDERSUB, SC_MARK_EMPTY);
		this->markerDefine(SC_MARKNUM_FOLDERTAIL, SC_MARK_EMPTY);

		setAutomaticFold(4);
		setFoldFlags(16);

		setIndentationGuides(false);
		setTabWidth(4);
		setTabIndents(true);
		setUseTabs(false);
		setTabDrawMode(SCTD_LONGARROW);
		setWhitespaceSize(1);

		showSpecialChars(false);

		// Auto-indent
		connect(this, &CodeEditor::charAdded, this, [this](int ch) {

			int eol_mode = this->eOLMode();
			int eol_ch = eol_mode == SC_EOL_CR ? '\r' : '\n';

			if (ch == eol_ch) {
				char linebuf[1000];
				int curLine = lineFromPosition(this->currentPos());
				int prevLine = curLine -  1;
				int lineLength = this->lineLength(curLine);
				int indent = 0;

				if (curLine > 0) {
					QByteArray linebuf = this->getLine(prevLine);
					if (linebuf.size() > 7) {
						int i = 7;
						while (linebuf[i++] == ' ') {
							indent++;
						}
					}
				}

				if (indent) {
					this->insertText(this->currentPos(), QString(indent + 7, ' ').toUtf8().constData());
					this->gotoPos(this->currentPos() + indent + 7);
				}
			}
		});		


		connect(Ide::TaskManager(), &IdeTaskManager::SettingsChanged, this, &CodeEditor::setFontFromSettings);
#if DEBUG
		QShortcut* qs1 = new QShortcut(QKeySequence(Qt::Key::Key_F12), this);
#else
		QShortcut* qs1 = new QShortcut(QKeySequence(Qt::Key::Key_F4), this);
#endif
		connect(qs1, &QShortcut::activated, this, [this]() {
			sptr_t wstart = this->wordStartPosition(this->currentPos(), true);
			sptr_t wend = this->wordEndPosition(this->currentPos(), true);
			sptr_t ln = this->lineFromPosition(this->currentPos());
			QString word = QString::fromUtf8(this->get_text_range(wstart, wend));
			if (!word.isEmpty()) {
				QString path = "";
				sptr_t eol = this->lineEndPosition(ln);
				QString rest_of_line = this->get_text_range(wend, eol).trimmed();
				auto words = rest_of_line.split(' ');
				QQueue<QString> q;
				QStringList items;
				items.insert(0, word);
				q.append(words);
				while (!q.isEmpty() && q.dequeue() == "OF") {
					items.insert(0, q.dequeue());
				}
				items.insert(0, "*");
				path = items.join(':').replace(".", "");
				Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("Got(1): %1").arg(word), QLogger::LogLevel::Trace);
				Ide::TaskManager()->gotoDefinition(this, path, ln);
			}
		});

#if DEBUG
		QShortcut* qs2 = new QShortcut(QKeySequence("Shift+F12"), this);
#else
		QShortcut* qs2 = new QShortcut(QKeySequence("Shift+F4"), this);
#endif
		connect(qs2, &QShortcut::activated, this, [this]() {
			sptr_t wstart = this->wordStartPosition(this->currentPos(), true);
			sptr_t wend = this->wordEndPosition(this->currentPos(), true);
			sptr_t ln = this->lineFromPosition(this->currentPos());
			QString s = QString::fromUtf8(this->get_text_range(wstart, wend));
			Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("Got(2): %1").arg(s), QLogger::LogLevel::Trace);
		});

		QShortcut *qs3 = new QShortcut(QKeySequence("Ctrl+K"), this);
		connect(qs3, &QShortcut::activated, this, [this]() {
			sptr_t wstart = this->selectionStart();
			sptr_t wend = this->selectionEnd();
			int ln_start = this->lineFromPosition(wstart);
			int ln_end = this->lineFromPosition(wend);

			// we get the current status fom the first line in the selection
			QByteArray qba = this->get_text_range(this->positionFromLine(ln_start), this->lineEndPosition(ln_start));
			QString s = QString::fromUtf8(qba);
			int is_currently_commented = (s.length() >= 7 && s[6] == '*');

			for (int ln = ln_start; ln <= ln_end; ln++) {
				QByteArray qba = this->get_text_range(this->positionFromLine(ln), this->lineEndPosition(ln));
				QString s = QString::fromUtf8(qba);
				int cur_line_len = s.length();

				if (cur_line_len < 7) {
					s = s.leftJustified(7);
					this->setTargetStart(this->positionFromLine(ln));
					this->setTargetEnd(this->positionFromLine(ln) + cur_line_len - 1);
					this->replaceTarget(7, (s.left(6) + (is_currently_commented ? " " : "*")).toUtf8());
				}
				else {
					this->setTargetStart(this->positionFromLine(ln) + 6);
					this->setTargetEnd(this->positionFromLine(ln) + 7);
					this->replaceTarget(1, is_currently_commented ? " " : "*");
				}
			}

			//Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, QString("Got(2): %1").arg(s), QLogger::LogLevel::Trace);
		});

	}
};


CodeEditor::~CodeEditor()
{}

void CodeEditor::paintEvent(QPaintEvent* event)
{
	ScintillaEdit::paintEvent(event);

	QColor areaAcolor(240, 240, 240);
	QColor areaCcolor(240, 240, 255);
	QPainter painter(viewport());

	QString dummyA(7, QChar('9'));
	QString dummyC(72, QChar('9'));
	int __areaAWidth = this->textWidth(STYLE_LINENUMBER, dummyA.toUtf8().constData());
	int __areaCOffset = this->textWidth(STYLE_LINENUMBER, dummyC.toUtf8().constData());

	QRect areaA(18 + 18 + __lineNumberWidth, viewport()->rect().top(), __areaAWidth, viewport()->rect().bottom() + 1);
	QRect areaC(18 + 18 + __lineNumberWidth + __areaCOffset, viewport()->rect().top(), viewport()->rect().width(), viewport()->rect().bottom() + 1);

	painter.setCompositionMode(QPainter::CompositionMode::CompositionMode_Darken);
	painter.fillRect(areaA, areaAcolor);
	painter.fillRect(areaC, areaCcolor);
}

QString CodeEditor::toPlainText()
{
	return QString::fromUtf8(this->getText(this->length()));
}

void CodeEditor::setPlainText(QString text)
{
	this->setText(text.toLocal8Bit().constData());
}

void CodeEditor::ensureLineVisible(int ln)
{
	//this->lineVisible(ln);
	this->gotoLine(--ln);
}

void CodeEditor::setFontFromSettings()
{
	QSettings settings;
	QString font_name = settings.value("editor_font_name", DEFAULT_EDITOR_FONT_NAME).toString();
	styleSetFont(STYLE_DEFAULT, font_name.toUtf8().constData());

	int font_size = settings.value("editor_font_size", DEFAULT_EDITOR_FONT_SIZE).toInt();
	styleSetSize(STYLE_DEFAULT, font_size);
}

void CodeEditor::handleMarginClick(int position, int modifiers, int margin)
{
	//handleBreakPoint(position, modifiers);

	switch (margin) {
		case MARGIN_SCRIPT_FOLD_INDEX:
			{
				int ln = this->lineFromPosition(position);
				this->toggleFold(ln);
			}
			break;

		case MARGIN_INDICATORS:
			//handleBookmark(position, modifiers);
			handleBreakPoint(position, modifiers);
			break;
	}	
}

void CodeEditor::handleBreakPoint(int position, int modifiers)
{
	int ln = this->lineFromPosition(position) + 1;

	MdiChild* c = ((MdiChild*)this);
	//QString module = PathUtils::toModuleName(c->currentFile());

	if (!Ide::TaskManager()->existsBreakpoint(c->currentFile(), ln)) {
		Ide::TaskManager()->addBreakpoint(c->currentFile(), ln);
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, "Set breakpoint at " + c->currentFile() + ":" + QString::number(ln), QLogger::LogLevel::Debug);
		markerAdd(ln - 1, MRKR_BKP);
	}
	else {
		Ide::TaskManager()->removeBreakpoint(c->currentFile(), ln);
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, "Removed breakpoint at " + c->currentFile() + ":" + QString::number(ln), QLogger::LogLevel::Debug);
		markerDelete(ln - 1, MRKR_BKP);
	}
}

void CodeEditor::showBookmarkAtLine(int ln)
{
	markerAdd(ln - 1, MRKR_BOOKMARK);
}

void CodeEditor::handleBookmark(int position, int modifiers)
{
	int ln = this->lineFromPosition(position) + 1;

	MdiChild* c = ((MdiChild*)this);
	QString filename = QDir::cleanPath(c->currentFile());

	if (!Ide::TaskManager()->existsBookmark(filename, ln)) {
		Ide::TaskManager()->addBookmark(filename, ln);
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, "Set bookmark at " + filename + ":" + QString::number(ln), QLogger::LogLevel::Debug);
		markerAdd(ln - 1, MRKR_BOOKMARK);
	}
	else {
		Ide::TaskManager()->removeBookmark(filename, ln);
		Ide::TaskManager()->logMessage(GIX_CONSOLE_LOG, "Removed bookmark at " + filename + ":" + QString::number(ln), QLogger::LogLevel::Debug);
		markerDelete(ln - 1, MRKR_BOOKMARK);
	}
}

void CodeEditor::highlightDebuggedLine(int n)
{
	if (--n < 0)
		return;

	markerDeleteAll(MRKR_DBG_CURLINE);
	markerAdd(n, MRKR_DBG_CURLINE);
}

QString CodeEditor::selectedText()
{
	return QString::fromUtf8(this->getSelText());
}

void CodeEditor::toggleBreakpoint()
{
	handleBreakPoint(this->currentPos(), 0);
}

void CodeEditor::toggleBookmark()
{
	handleBookmark(this->currentPos(), 0);
}

bool CodeEditor::isShowingSpecialChars()
{
	return this->viewEOL();
}

void CodeEditor::showSpecialChars(bool b)
{
	setViewEOL(b);
	setViewWS(b ? SCWS_VISIBLEONLYININDENT : SCWS_INVISIBLE);
}

void CodeEditor::removeDebugMarkers()
{
	markerDeleteAll(MRKR_DBG_CURLINE);
}

void CodeEditor::highlightSymbol(int line, QString symbol_name)
{
	sptr_t ed_ln = (sptr_t)line - 1;
	gotoLine(ed_ln);
	QString ln = QString::fromUtf8(this->getLine(ed_ln));
	sptr_t pos = this->positionFromLine(ed_ln);
	this->setCurrentPos(pos);
	this->setFocus(true);
}

void CodeEditor::clearAllBookmarks()
{
	MdiChild* c = ((MdiChild*)this);
	QString module = PathUtils::toModuleName(c->currentFile());

	markerDeleteAll(MRKR_BOOKMARK);
	Ide::TaskManager()->clearBookmarks(module);
}

void CodeEditor::changed()
{
	int maxLineNumberCharLength = QString::number(this->lineCount()).size();
	if (maxLineNumberCharLength == __maxLineNumberCharLength)
		return;

	const int padding = 2;
	QString dummy(maxLineNumberCharLength, QChar('9'));
	int tw = this->textWidth(STYLE_LINENUMBER, dummy.toUtf8().constData()) + padding;

	this->setMarginWidthN(1, tw);
	__maxLineNumberCharLength = maxLineNumberCharLength;
	__lineNumberWidth = tw;

	//int fl = this->foldLevel(8);
	//setFoldLevel(8, fl + SC_FOLDLEVELHEADERFLAG);
	//setFoldLevel(9, fl + 1);
	//setFoldLevel(10, fl + 1);
	//setFoldLevel(10, fl + 1);
	//setFoldLevel(10, fl + 1);
}
