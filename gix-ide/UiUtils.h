#pragma once

#include <QString>
#include <QTreeView>

#define DEFAULT_TREEVIEW_FONT_NAME	"Courier New"
#define DEFAULT_TREEVIEW_FONT_SIZE	8

class UiUtils
{
public:
	UiUtils();
	~UiUtils();

	static void ErrorDialog(QString msg);
	static void InfoDialog(QString msg);
	static bool YesNoDialog(QString msg);


	static int OnPlatform(int _win, int _linux, int _osx);
	static QString OnPlatform(QString _win, QString _linux, QString _osx);
	static int computeFontSize(QWidget *w, int v);

	static void setTreeViewFont(QTreeView *tv);

    static void dispatchToMainThread(std::function<void()> callback);

	static bool isOceCompatModeEnabled();
};

