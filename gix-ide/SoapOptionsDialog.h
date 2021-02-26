#pragma once

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>

#include "ui_SoapOptions.h"

#include "PropertyOptionsDialog.h"
class SoapOptionsDialog :
	public PropertyOptionsDialog, private Ui::Ui_SoapOptions
{

public:
	SoapOptionsDialog(QMainWindow *mw = 0);
	~SoapOptionsDialog();

	// Inherited via PropertyOptionsDialog
	virtual bool show(ProjectItem* pi, PropertyDefinition* pd) override;
	virtual void accept() override;
	virtual void cancel() override;
};

