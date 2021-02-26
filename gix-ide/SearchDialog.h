#pragma once

#ifdef __MINGW32__
typedef unsigned char byte;
#include <cstddef>
#endif

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>

#include "ui_Search.h"
//#include "MainWindow.h"
#include "IdeSearchManager.h"

class QDialogButtonBox;
class QFileInfo;
class QTabWidget;
class SearchDialog;
class ProjectCollection;
class MainWindow;

class SearchDialog : public QDialog, private Ui::Ui_Search
{
	Q_OBJECT

public:
	SearchDialog(MainWindow *mw);
	~SearchDialog();

	void setup(SearchType mode = SearchType::Find, QString search_spec = QString());
	void show();

	virtual bool event(QEvent*);

private:
	MainWindow* main_window;

	void findNext();
	void count();
	void replace();
	void replaceAll();
	void replaceInOpenDocs();

	void setupUiForSearchType(SearchType mode);
	SearchType tabIndexToSearchType(int idx);

private slots:
	void searchTypeTabChanged(int idx);

};
