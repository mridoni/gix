#include "DragDropTableWidget.h"
#include "DragDropTreeWidget.h"
#include <QDragEnterEvent>
#include <QTreeWidgetItem>

DragDropTableWidget::DragDropTableWidget(int rows, int cols, bool _enable_drag, bool _enable_drop, QWidget *parent) : QTableWidget(rows, cols, parent)
{
	enable_drag = _enable_drag;
	enable_drop = _enable_drop;

	if (enable_drop) {
		this->setAcceptDrops(true);
		this->setDropIndicatorShown(true);
	}

	if (enable_drag) {
		this->setDragEnabled(true);
	}

	if (enable_drag && enable_drop)
		this->setDragDropMode(QAbstractItemView::DragDropMode::DragDrop);
	else
		if (!enable_drag && enable_drop)
			this->setDragDropMode(QAbstractItemView::DragDropMode::DropOnly);
		else
			if (enable_drag && !enable_drop)
				this->setDragDropMode(QAbstractItemView::DragDropMode::DragOnly);
			else
				if (!enable_drag && !enable_drop)
					this->setDragDropMode(QAbstractItemView::DragDropMode::NoDragDrop);
}

void DragDropTableWidget::dropEvent(QDropEvent *event)
{
	if (!enable_drop)
		return;

	const QMimeData *md = event->mimeData();
	DragDropTreeWidget *src = dynamic_cast<DragDropTreeWidget *>(event->source());
	if (src) {
		QVariant v = src->draggedData();
		//QTreeWidgetItem *item = dynamic_cast<QTreeWidgetItem *>(src->itemAt(event->pos()));
		//if (item) {
		//	QVariant v = item->data(0, Qt::UserRole);
			if (v.isValid())
				emit itemDropped(v, md);
		//}

	}
	event->acceptProposedAction();
}

void DragDropTableWidget::dragMoveEvent(QDragMoveEvent *event)
{
	event->acceptProposedAction();
}

void DragDropTableWidget::dragEnterEvent(QDragEnterEvent *event)
{
	event->acceptProposedAction();
}
