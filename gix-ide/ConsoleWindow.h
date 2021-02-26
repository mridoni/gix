#pragma once

#include <QMainWindow>
#include <QToolBar>

#include "MainWindow.h"
#include "ConsoleWidget.h"

class ConsoleWindow : public QMainWindow
{

public:
	ConsoleWindow(QWidget* parent, MainWindow* mw);
	void setConsoleFont();
	~ConsoleWindow();

	void clear();

	void append(QString s, bool is_err = false);
	void appendOut(QString s);
	void appendErr(QString s);

private:
	ConsoleWidget* console;
	MainWindow* mainWindow;
	QToolBar* toolBar;
};

