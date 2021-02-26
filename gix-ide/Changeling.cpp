#include "Changeling.h"


Changeling::Changeling()
{
	content = "";
}

void Changeling::clear()
{
	content = "";
}

void Changeling::setContent(QString s)
{
	if (s != content) {
		content = s;
		emit contentChanged(s);
	}
}

QString Changeling::getContent()
{
	return content;
}
