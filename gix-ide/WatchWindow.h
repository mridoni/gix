#pragma once
#include <QTableWidget>
#include <QStringList>
#include <QPushButton>

#include "MainWindow.h"


class QWidget;
class MainWindow;

class WatchWindow : public QMainWindow
{
	friend class IdeTaskManager;

public:
	WatchWindow(QWidget *parent, MainWindow *mw);
	~WatchWindow();

public slots:
	void addButtonClicked();
	void removeButtonClicked();
	void IdeStatusChanged(IdeStatus);
	void onDebuggerBreak();

private:
	MainWindow * mainWindow;
	QTableWidget *varTable;

	QPushButton *bAdd;
	QPushButton *bRemove;
	QPushButton *bRefresh;

	void refreshContent();
};

