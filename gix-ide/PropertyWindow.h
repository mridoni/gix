#pragma once

#include <QTableWidget>

#include "MainWindow.h"
#include "PropertySource.h"

class QWidget;
class MainWindow;

class PropertyWindow : public QMainWindow
{
	Q_OBJECT

public:
	PropertyWindow(QWidget *parent, MainWindow *mw);
	~PropertyWindow();

public slots:
	//void setValue(int value);
	void setContent(ProjectItem *);
	void refreshContent();

signals:
	void notifyPropertyValueChanged(PropertyDefinition*, QVariant, ProjectItem* pi);

private:
	ProjectItem  *item;
	MainWindow * mainWindow;
	QTableWidget *propertyTable;
	
	void envVarsListEditButtonClicked(PropertyDefinition *, QVariant, ProjectItem *pi, QWidget *prop_visual);
	void dirPathListEditButtonClicked(PropertyDefinition *, QVariant, ProjectItem *pi, QWidget *prop_visual);
	void dirPathEditButtonClicked(PropertyDefinition *, QVariant, ProjectItem *pi, QWidget *prop_visual);
	void filePathEditButtonClicked(PropertyDefinition *, QVariant, ProjectItem *pi, QWidget *prop_visual);

	void propertyValueChanged(PropertyDefinition *, QVariant, ProjectItem *pi);

	QFont getGridFont();
};

