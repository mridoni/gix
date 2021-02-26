#pragma once

#include <QString>
#include <QVariant>
#include <QMap>
#include <QProcessEnvironment>

#include "PropertySource.h"
#include "gixcommon_global.h"

class GIXCOMMON_EXPORT MacroManager
{
public:
	MacroManager();
	MacroManager(const QMap<QString, QVariant>&);
	~MacroManager();

	void add(PropertySource *);
	void add(QProcessEnvironment&);
	void add(QMap<QString, QString>);
	void add(QMap<QString, QVariant>);
	void add(const QString &k, const QString &v);

	QString translate(QString);

	QVariant getItem(const QString &k) const;
	QVariantMap getItems();

private:
	QMap<QString, QVariant> items;
};

