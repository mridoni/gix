#pragma once

#include <QString>
#include <QList>

#define BY_REFERENCE	true
#define BY_VALUE		false

class ESqlCallParameter
{
public:
	QString value;
	bool by_reference;
};

class ESQLCall
{
public:
	ESQLCall(bool _is_static);
	ESQLCall(const QString _call_name, bool _is_static);

	void addParameter(QString value, bool by_reference);
	void addParameter(int value, bool by_reference);

	QStringList format() const;

private:
	bool is_static;
	QString call_name;
	QList<ESqlCallParameter> params;
};

