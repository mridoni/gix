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