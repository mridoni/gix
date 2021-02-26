#include "SoapOptionsDialog.h"

SoapOptionsDialog::SoapOptionsDialog(QMainWindow* mw) : PropertyOptionsDialog(mw)
{
	setupUi(this);
	this->setAttribute(Qt::WA_DeleteOnClose);
}


SoapOptionsDialog::~SoapOptionsDialog()
{
}

bool SoapOptionsDialog::show(ProjectItem* pi, PropertyDefinition* pd)
{
	this->setModal(true);
	QWidget::show();
	return true;
}

void SoapOptionsDialog::accept()
{
}

void SoapOptionsDialog::cancel()
{
}
