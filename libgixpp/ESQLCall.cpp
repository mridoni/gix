#include "ESQLCall.h"

#define LINE_PREFIX			 "GIXSQL     "

ESQLCall::ESQLCall(const QString _call_name, bool _is_static)
{
	call_name = _call_name;
	is_static = _is_static;
}

ESQLCall::ESQLCall(bool _is_static)
{
	is_static = _is_static;
}

void ESQLCall::addParameter(QString value, bool by_reference)
{
	params.append({ value, by_reference });
}

void ESQLCall::addParameter(int value, bool by_reference)
{
	params.append({ QString::number(value), by_reference });
}

QStringList ESQLCall::format() const
{
	QStringList res;
	const char *lp = LINE_PREFIX;

	res.append(lp + QString("CALL %1\"%2\"").arg(is_static ? "STATIC " : "", call_name) + (params.size() ? " USING" : ""));
	
	for (auto p : params) {
		res.append(lp + QString("    BY %1 %2").arg(p.by_reference ? "REFERENCE" : "VALUE", p.value));
	}

	res.append(lp + QString("END-CALL"));

	return res;
}

