#pragma once

#include <QObject>
#include <QString>

class Changeling : public QObject {

	Q_OBJECT

public:
	Changeling();
	void clear();
	void setContent(QString);
	QString getContent();

signals:
	void contentChanged(QString s);

private:
	QString content;
};