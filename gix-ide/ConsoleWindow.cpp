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

#include "ConsoleWindow.h"
#include "UiUtils.h"
#include <QSettings>

#if defined(__linux__)
#include <unistd.h>
#endif

ConsoleWindow::ConsoleWindow(QWidget* parent, MainWindow* mw)
{
	this->setWindowTitle("Output");
	this->setMinimumHeight(100);
	this->setMinimumWidth(250);
	this->setWindowFlags(Qt::Widget); // <---------
	toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

#if defined(__linux__)
    this->console = new QTermWidget(0, this);

    connect (this->console, &QTermWidget::sendData, this, [this](const char *s, int len) {
        if (s && len) {
            if (has_echo)
                write(this->console->getPtySlaveFd(), s, strlen(s));

            QByteArray qba = QByteArray::fromRawData(s, len);
            console_buffer += QString::fromLocal8Bit(qba);
            if (console_buffer.endsWith('\n') || console_buffer.endsWith('\r')) {
                emit inputAvailable(console_buffer);
                console_buffer.clear();
            }
        }
    });

    this->console->setColorScheme("GreenOnBlack");
	this->console->setFlowControlEnabled(false);
    this->console->startTerminalTeletype();

#else
    this->console = new ConsoleWidget(this);
#endif
	//this->console->setReadOnly(true);
	
	setConsoleFont();

	this->setCentralWidget(console);
}

void ConsoleWindow::setConsoleFont()
{
	QSettings settings;

	QFont f = this->console->font();

	QString font_name = settings.value("console_font_name", "").toString();
	int font_size = settings.value("console_font_size", 0).toInt();

	if (font_name.isEmpty() || font_size == 0) {
		f.setFamily("Courier New");
		f.setPointSize(UiUtils::computeFontSize(this, 8));
	}
	else {
		f.setFamily(font_name);
		f.setPointSize(font_size);
	}

	this->console->setFont(f);
}

ConsoleWindow::~ConsoleWindow()
{

}

void ConsoleWindow::setEcho(bool b)
{
    has_echo = b;
}

void ConsoleWindow::clear()
{
	console->clear();
	setConsoleFont();
}

void ConsoleWindow::append(QString s, bool is_err)
{
#if defined(__linux__)
    const char *c = s.toLocal8Bit().constData();
    write(this->console->getPtySlaveFd(), c, strlen(c));
#else
    console->putData(s.toUtf8());
#endif
}

void ConsoleWindow::appendOut(QString s)
{
	append(s, false);
}

void ConsoleWindow::appendErr(QString s)
{
	append(s, true);
}

QString ConsoleWindow::getTextContent()
{
#if defined(__linux__)
	return QString();
#elif defined(WIN32)	
	return console->document()->toPlainText();
#else
	return QString();
#endif	
}
