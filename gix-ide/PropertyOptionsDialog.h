#pragma once

#include <QMap>
#include <QDialog>
#include <QVariant>
#include <QMainWindow>

#include "ProjectItem.h"

class PropertyOptionsDialog : public QDialog
{
	Q_OBJECT

public:
	PropertyOptionsDialog(QMainWindow *mw = 0);

	virtual bool show(ProjectItem* pi, PropertyDefinition* pd) = 0;
	virtual void accept() = 0;
	virtual void cancel() = 0;

	static PropertyOptionsDialog *get(QString, QMainWindow* mw);

protected:
	QScopedPointer<QVariantMap> sub_properties;

	bool init(ProjectItem* pi, PropertyDefinition* pd);
	bool commitSubProperties();

private:
	ProjectItem* item;
	PropertyDefinition* prop_def;
};

