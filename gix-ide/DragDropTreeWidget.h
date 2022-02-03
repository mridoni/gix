#pragma once

#include <QTreeWidget>
#include <QMimeData>

class DragDropTreeWidget : public QTreeWidget
{
	Q_OBJECT

public:

	DragDropTreeWidget(bool _enable_drag, bool _enable_drop, QWidget *parent = nullptr);

	void dropEvent(QDropEvent *event) override;
	void dragMoveEvent(QDragMoveEvent *event) override;
	void dragEnterEvent(QDragEnterEvent *event) override;
	void dragLeaveEvent(QDragLeaveEvent *event) override;

	void mousePressEvent(QMouseEvent *event) override;
	void mouseReleaseEvent(QMouseEvent *event) override;

	QVariant draggedData();

signals:
	void itemDropped(QTreeWidgetItem *tvi, const QMimeData *md);

private:
		bool enable_drop; 
		bool enable_drag;

		QTreeWidgetItem *cur_dragdrop_item = nullptr;
		bool cur_bold_status = false;

		void highlight_drop_target_item();
		void reset_drop_target_item();

		bool is_pressed = false;
		QVariant dragged_data;
};

