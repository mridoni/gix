#pragma once

#include <QTreeWidget>
#include <QStringList>
#include <QPushButton>
#include <QStandardItemModel>

#include "MainWindow.h"
#include "ProjectFile.h"
#include "ListingFileParser.h"

class QWidget;
class MainWindow;

class NavigationWindow : public QMainWindow
{
	Q_OBJECT

	friend class MainWindow;

public:
	NavigationWindow(QWidget* parent, MainWindow* mw);
	~NavigationWindow();

	void setContent(ProjectFile*);
	bool hasContent();
	void refreshContent();

public slots:
	void IdeStatusChanged(IdeStatus);
	//void onDebuggerBreak();

private slots:
	void dataItemDoubleClicked(QTreeWidgetItem* item, int column);
	//void prepareMenu(const QPoint& pos);

private:
	MainWindow* mainWindow;

	QPushButton* bRefresh;

	void refresh_data_items();
	void append_children(DataEntry* e, QTreeWidgetItem* parent_item);
	void goto_definition(DataEntry* e);

	QTreeWidget* dataWidget = nullptr;

	ProjectFile* cur_file = nullptr; 
};

