#include "ConsoleWindow.h"
#include "UiUtils.h"

#include <QSettings>

ConsoleWindow::ConsoleWindow(QWidget* parent, MainWindow* mw)
{
	this->setWindowTitle("Output");
	this->setMinimumHeight(100);
	this->setMinimumWidth(500);
	this->setWindowFlags(Qt::Widget); // <---------
	toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

	this->console = new ConsoleWidget(this);
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

void ConsoleWindow::clear()
{
	console->clear();
	setConsoleFont();
}

void ConsoleWindow::append(QString s, bool is_err)
{
	console->putData(s.toUtf8());
}

void ConsoleWindow::appendOut(QString s)
{
	append(s, false);
}

void ConsoleWindow::appendErr(QString s)
{
	append(s, true);
}

