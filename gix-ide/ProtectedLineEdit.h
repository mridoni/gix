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

#pragma once

#include<QMouseEvent>
#include<QLineEdit>
#include<QWidget>

class ActivateOnDoubleClick;

class ProtectedLineEdit : public QLineEdit
{
public:
	ProtectedLineEdit(QWidget *);
	~ProtectedLineEdit();

private:
	void onDoubleClick(QMouseEvent *);
	ActivateOnDoubleClick *activator;
};

class ActivateOnDoubleClick : public QObject {

	bool is_accept_event(QEvent *event)
	{
		if (event->type() == QEvent::FocusOut)
			return true;

		if (event->type() == QEvent::KeyRelease) {
			QKeyEvent *ke = (QKeyEvent *)event;
			int keyValue = ke->key();
			if (keyValue == Qt::Key::Key_Enter || keyValue == Qt::Key::Key_Return)
				return true;
		}

		return false;
	}

	bool eventFilter(QObject *watched, QEvent *event) {
		if (event->type() == QEvent::MouseButtonDblClick) {
			ProtectedLineEdit *ple = dynamic_cast<ProtectedLineEdit *>(watched);
			if (ple != nullptr) {
				QPalette palette;
				palette.setColor(QPalette::Base, QColor(255, 255, 255));
				palette.setColor(QPalette::Text, Qt::black);
				ple->setPalette(palette);
				ple->selectAll();
				ple->setReadOnly(false);
			}
		}

		if (is_accept_event(event)) {
		//if (event->type() == QEvent::FocusOut) {
			ProtectedLineEdit *ple = dynamic_cast<ProtectedLineEdit *>(watched);
			if (ple != nullptr) {
				QPalette palette;
				palette.setColor(QPalette::Base, QColor(240, 240, 240));
				palette.setColor(QPalette::Text, Qt::black);
				ple->setPalette(palette);
				ple->setReadOnly(true);
				ple->deselect();
			}
		}

		return QObject::eventFilter(watched, event);
	}
public:
	explicit ActivateOnDoubleClick(QObject * parent = nullptr) : QObject{ parent } {
		addTo(parent);
	}
	void addTo(QObject * obj) {
		if (obj) obj->installEventFilter(this);
	}
};