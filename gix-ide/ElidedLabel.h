/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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

#pragma once

#include <QLabel>

class ElidedLabel : public QLabel
{
	Q_OBJECT
public:
	ElidedLabel(QWidget* parent = 0, Qt::WindowFlags f = 0);
	ElidedLabel(const QString& txt, QWidget* parent = 0, Qt::WindowFlags f = 0);
	ElidedLabel(const QString& txt,
		Qt::TextElideMode elideMode = Qt::ElideRight,
		QWidget* parent = 0,
		Qt::WindowFlags f = 0
	);
	// Set the elide mode used for displaying text.
	void setElideMode(Qt::TextElideMode elideMode) {
		elideMode_ = elideMode;
		updateGeometry();
	}
	// Get the elide mode currently used to display text.
	Qt::TextElideMode elideMode() const { return elideMode_; }
	// QLabel overrides
	void setText(const QString &);

protected: // QLabel overrides
	void paintEvent(QPaintEvent*);
	void resizeEvent(QResizeEvent*);
	// Cache the elided text so as to not recompute it every paint event
	void cacheElidedText(int w);

private:
	Qt::TextElideMode elideMode_;
	QString cachedElidedText;
};
