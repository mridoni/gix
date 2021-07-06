#pragma once

#include <QWizard>
#include <QString>
#include <QStringList>

#include "CompilerDefinition.h"

class CompilerOptsWizardPage;
class CompilerSaveWizardPage;

class AddCompilerWizard : public QWizard
{
public:
	AddCompilerWizard(QWidget *parent);
	~AddCompilerWizard();

	void setInfo(QStringList il);
	QStringList getInfo();

	CompilerDefinition *cdef = nullptr;

	QString new_compiler_id;

private:
	QWizardPage *createIntroPage();
	QWizardPage *createConclusionPage();

	CompilerOptsWizardPage *opts_page = nullptr;
	CompilerSaveWizardPage *save_page = nullptr;

	QStringList info;

};

