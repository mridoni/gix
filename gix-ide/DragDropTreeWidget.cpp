#include "DragDropTreeWidget.h"

#include <QCoreApplication>
#include <QDragEnterEvent>

#include "ProjectItem.h"
#include "UiUtils.h"

DragDropTreeWidget::DragDropTreeWidget(bool _enable_drag, bool _enable_drop, QWidget *parent)
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

void DragDropTreeWidget::dropEvent(QDropEvent *event)
{
	if (!enable_drop)
		return;

	const QMimeData *md = event->mimeData();

	auto item = itemAt(event->pos());
	if (dynamic_cast<QTreeWidgetItem *>(item)) {
		QTreeWidgetItem *qwi = (QTreeWidgetItem *)item;
		emit itemDropped(qwi, md);
	}

	reset_drop_target_item();
	event->acceptProposedAction();
}

void DragDropTreeWidget::dragMoveEvent(QDragMoveEvent *event)
{

	auto item = itemAt(event->pos());

	if (item != cur_dragdrop_item) {
		reset_drop_target_item();
	}

	if (item != cur_dragdrop_item && dynamic_cast<QTreeWidgetItem *>(item)) {
		cur_dragdrop_item = item;
		cur_bold_status = item->font(0).bold();
		highlight_drop_target_item();
	}

	event->acceptProposedAction();
}

void DragDropTreeWidget::dragEnterEvent(QDragEnterEvent *event)
{
	event->acceptProposedAction();
}

void DragDropTreeWidget::dragLeaveEvent(QDragLeaveEvent *event)
{
	event->accept();
}

void DragDropTreeWidget::mousePressEvent(QMouseEvent *event)
{
	if (enable_drag) {
		if (event->button() == Qt::MouseButton::LeftButton) {
			is_pressed = true;
			auto item = itemAt(event->pos());
			if (dynamic_cast<QTreeWidgetItem *>(item)) {
				QTreeWidgetItem *qwi = (QTreeWidgetItem *)item;
				dragged_data = qwi->data(0, Qt::UserRole);
			}
		}
	}
	QTreeWidget::mousePressEvent(event);
}

void DragDropTreeWidget::mouseReleaseEvent(QMouseEvent * event)
{
	is_pressed = true;
	dragged_data.clear();

	QTreeWidget::mouseReleaseEvent(event);
}

QVariant DragDropTreeWidget::draggedData()
{
	return dragged_data;
}

void DragDropTreeWidget::highlight_drop_target_item()
{
	if (cur_dragdrop_item) {
		cur_dragdrop_item->setTextColor(0, QColor(0x66, 0, 0xcc));
		UiUtils::setTreeWidgetItemFontBold(cur_dragdrop_item, true);
	}
}

void DragDropTreeWidget::reset_drop_target_item()
{
	if (cur_dragdrop_item) {
		cur_dragdrop_item->setTextColor(0, QColor(0, 0, 0));
		UiUtils::setTreeWidgetItemFontBold(cur_dragdrop_item, cur_bold_status);
		cur_bold_status = false;
	}
}
