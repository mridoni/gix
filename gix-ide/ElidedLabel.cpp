/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#include "ElidedLabel.h"

#include <QDebug>
#include <QPainter>
#include <QResizeEvent>

ElidedLabel::ElidedLabel(QWidget* parent, Qt::WindowFlags f)
	: QLabel(parent, f)
	, elideMode_(Qt::ElideRight)
{
	// setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Preferred);
}

ElidedLabel::ElidedLabel(const QString& txt, QWidget* parent, Qt::WindowFlags f)
	: QLabel(txt, parent, f)
	, elideMode_(Qt::ElideRight)
{
}

ElidedLabel::ElidedLabel(const QString& txt, Qt::TextElideMode elideMode,
	QWidget *parent, Qt::WindowFlags f)
	: QLabel(txt, parent, f)
	, elideMode_(elideMode)
{
}

void ElidedLabel::setText(const QString& txt) {
	QLabel::setText(txt);
	cacheElidedText(geometry().width());
}

void ElidedLabel::cacheElidedText(int w) {
	cachedElidedText = fontMetrics().elidedText(text(), elideMode_, w, Qt::TextShowMnemonic);
}

void ElidedLabel::resizeEvent(QResizeEvent* e) {
	QLabel::resizeEvent(e);
	cacheElidedText(e->size().width());
}

void ElidedLabel::paintEvent(QPaintEvent* e) {
	if (elideMode_ == Qt::ElideNone) {
		QLabel::paintEvent(e);
	}
	else {
		QPainter p(this);
		p.drawText(0, 0,
			geometry().width(),
			geometry().height(),
			alignment(),
			cachedElidedText
		);
	}
}