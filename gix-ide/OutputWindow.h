#pragma once

#include <QString>
#include <QTextEdit>

#include "MainWindow.h"
#include "LogOutputType.h"
#include "QLogger.h"

class OutputWindow : public QMainWindow
{

public:
	OutputWindow(QWidget *parent, MainWindow *mw);
	~OutputWindow();

	void print(QString msg, QLogger::LogLevel log_level);

public slots:
	void clearAll();
	

private:
	MainWindow * mainWindow;
    QTextEdit *textArea;
	QToolBar* toolBar;
};

