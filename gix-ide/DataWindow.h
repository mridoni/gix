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

class DataWindow : public QMainWindow
{
	friend class MainWindow;

public:
	DataWindow(QWidget *parent, MainWindow *mw);
	~DataWindow();

	void setContent(ProjectFile *);
	bool hasContent();
	void refreshContent();

public slots:
	void IdeStatusChanged(IdeStatus);
	void onDebuggerBreak();

private slots:
	void dataItemDoubleClicked(QTreeWidgetItem * item, int column);
	void prepareMenu(const QPoint & pos);

private:
	MainWindow * mainWindow;

	QPushButton *bRefresh;

	void refresh_data_items();
	void append_children(DataEntry *e, QTreeWidgetItem *parent_item);
	void setNodeStatus(QString node_path, QTreeWidgetItem* si);

	QTreeWidget *dataWidget;
	//ListingFileParser *cur_data;
	ProjectFile *cur_file;

};

