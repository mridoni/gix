#pragma once

#include <QTableWidget>
#include <QTreeWidget>
#include <QMimeData>

class DragDropTableWidget : public QTableWidget
{
	Q_OBJECT

public:

	DragDropTableWidget(int rows, int cols, bool _enable_drag, bool _enable_drop, QWidget *parent = nullptr);

	void dropEvent(QDropEvent *event) override;
	void dragMoveEvent(QDragMoveEvent *event) override;
	void dragEnterEvent(QDragEnterEvent *event) override;

signals:
	void itemDropped(QVariant data, const QMimeData *md);

private:
	bool enable_drop;
	bool enable_drag;

};

