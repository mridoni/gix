#include "ProtectedLineEdit.h"



ProtectedLineEdit::ProtectedLineEdit(QWidget *parent) : QLineEdit(parent)
{
	this->setReadOnly(true);
	QPalette palette;
	palette.setColor(QPalette::Base, QColor(240, 240, 240));
	palette.setColor(QPalette::Text, Qt::black);
	this->setPalette(palette);

	activator = new ActivateOnDoubleClick({ this });

}


ProtectedLineEdit::~ProtectedLineEdit()
{
	delete activator;
}

void ProtectedLineEdit::onDoubleClick(QMouseEvent *)
{
}
