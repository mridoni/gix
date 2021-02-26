#pragma once

#ifdef __MINGW32__
#include <cstddef>
#endif

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>

#include "ui_NewProject.h"

class QDialogButtonBox;
class QFileInfo;
class QTabWidget;
class NewProjectDialog;
class ProjectCollection;
class Project;


class NewProjectDialog : public QDialog, private Ui::Ui_NewProject
{
	Q_OBJECT

public:
	NewProjectDialog(ProjectCollection *, QWidget *parent = 0);
	~NewProjectDialog() ;

	void show();

	void accept();

public slots:
	void projectTypeSelectionChanged(bool b);

private:
	ProjectCollection* prj_collection;

	void choosePrjFile();
	bool is_valid_prj_name(QString p);
	bool create_prj_file(Project* prj, int idx);
};
