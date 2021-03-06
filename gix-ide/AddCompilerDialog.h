#pragma once

#ifdef __MINGW32__
#include <cstddef>
#endif

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>

#include "CompilerDefinition.h"
#include "ui_AddCompilerDialog.h"

class QDialogButtonBox;
class QFileInfo;
class QTabWidget;
class NewProjectDialog;
class ProjectCollection;
class Project;


class AddCompilerDialog : public QDialog, private Ui::Ui_AddCompilerDialog
{
	Q_OBJECT

public:
	AddCompilerDialog(QWidget *parent = 0);
	~AddCompilerDialog();

	void show();

	void __accept();

public slots:

private:
	void addPlatform();
	bool validateCompilerDefinition();
	bool saveCompilerDefinition();
	void chooseBaseDir();

};
