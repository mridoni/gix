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

class DependenciesWindow : public QMainWindow
{
	friend class MainWindow;

public:
	DependenciesWindow(QWidget* parent, MainWindow* mw);
	~DependenciesWindow();

public slots:
	void IdeStatusChanged(IdeStatus);
	//void onDebuggerBreak();
	void updateFileDependencies();
	void updateProjectDependencies();

private:
	MainWindow* mainWindow;

	QTabWidget *tab_widget;
	QTreeWidget* file_dataWidget;
	QTreeWidget* prj_dataWidget;
	ListingFileParser* cur_data;
	

	void file_ItemDoubleClicked(QTreeWidgetItem* item, int column);
	void prj_ItemDoubleClicked(QTreeWidgetItem* item, int column);
	void tabChanged(int idx);

	bool get_project_dependencies(QMap<QString, QStringList> &deps);
};

