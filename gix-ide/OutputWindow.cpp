#include "OutputWindow.h"
#include "UiUtils.h"
#include "Ide.h"
#include "IdeTaskManager.h"

#include <QToolBar>

OutputWindow::OutputWindow(QWidget *parent, MainWindow *mw) : QMainWindow(parent)
{
	this->setWindowTitle("Output");
	this->setMinimumHeight(100);
	this->setMinimumWidth(500);
	this->setWindowFlags(Qt::Widget); // <---------
	toolBar = new QToolBar(this);
	this->addToolBar(toolBar);
	this->mainWindow = mw;

    this->textArea = new QTextEdit(this);
	this->textArea->setReadOnly(true);
	QFont f = this->textArea->font();
	f.setFamily("Courier New");
	f.setPointSize(UiUtils::computeFontSize(this, 9));
	this->textArea->setFont(f);
	this->setCentralWidget(textArea);

    connect(Ide::TaskManager(), &IdeTaskManager::print, this, &OutputWindow::print, Qt::ConnectionType::QueuedConnection);

	Ide::TaskManager()->flushLog();
}


OutputWindow::~OutputWindow()
{
	delete(toolBar);
}

void OutputWindow::print(QString msg, QLogger::LogLevel log_level)
{
#if defined(_DEBUG) || defined(_DEBUG_LOG_ON)
#if defined(_WIN32) || defined(_WIN64)
    OutputDebugStringA(QString("[" + QLogger::QLoggerManager::levelToText(log_level) + "] ").toUtf8().constData());
	OutputDebugStringA(msg.toUtf8().constData());
    OutputDebugStringA("\n");
#else
    //fprintf(stderr, "%s\n", msg.toUtf8().constData());
#endif
#endif
    
    if (!Ide::TaskManager()->isDebugOutputEnabled() && (log_level == QLogger::LogLevel::Debug || log_level == QLogger::LogLevel::Trace))
        return;

    QColor textColor(Qt::black);

    switch (log_level) {
        case QLogger::LogLevel::Trace:
            textColor = Qt::blue;
            break;

        case QLogger::LogLevel::Debug:
            textColor = Qt::darkCyan;
            break;

        case QLogger::LogLevel::Info:
            textColor = Qt::darkGray;
            break;

        case QLogger::LogLevel::Error:
            textColor = Qt::red;
            break;

        case QLogger::LogLevel::Success:
            textColor = Qt::darkGreen;
            break;
    }
    textArea->setTextColor(textColor);
    textArea->append(msg);
    textArea->ensureCursorVisible();
}

void OutputWindow::clearAll()
{
	textArea->clear();
}
