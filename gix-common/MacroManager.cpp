#include "MacroManager.h"

#include <QRegularExpression>

MacroManager::MacroManager()
{
}

MacroManager::MacroManager(const QMap<QString, QVariant>& props)
{
	add(props);
}


MacroManager::~MacroManager()
{
}

void MacroManager::add(PropertySource *ps)
{
	add(*ps->PropertyGetCurrentValues());
}

void MacroManager::add(QProcessEnvironment &env)
{
	QStringList keys = env.keys();
	for (int i = 0; i < keys.size(); i++) {
		this->items[keys[i]] = env.value(keys[0]);
	}
}

void MacroManager::add(const QString &k, const QString &v)
{
	this->items[k] = v;
}

void MacroManager::add(QMap<QString, QString> map)
{
	QMap<QString, QString>::iterator it;
	for (it = map.begin(); it != map.end(); ++it) {
		this->items[it.key()] = it.value();
	}
}

void MacroManager::add(QMap<QString, QVariant> map)
{
	QMap<QString, QVariant>::iterator it;
	for (it = map.begin(); it != map.end(); ++it) {
		this->items[it.key()] = it.value().toString();
	}
}


QString MacroManager::translate(QString s)
{
	bool f_exit = false;
	QRegularExpression rx("\\$\\{([A-Za-z0-9_\\-\\.]+)\\}", QRegularExpression::PatternOption::CaseInsensitiveOption);
	QRegularExpressionMatchIterator i = rx.globalMatch(s);
	QStringList vars;
	while (i.hasNext()) {
		QRegularExpressionMatch match = i.next();
		vars.append(match.captured(1));
	}

	if (!vars.size())
		return s;

	for (int i = 0; i < vars.size(); i++) {
		QString var = vars.at(i);
		if (items.contains(var)) {
			s = s.replace("${" + var + "}", items[var].toString());
		}
		else
			f_exit = true;
	}

	return f_exit ? s : translate(s);
}

QVariant MacroManager::getItem(const QString &k) const
{
	if (items.contains(k))
		return items[k];
	else
		return QVariant();
}

QVariantMap MacroManager::getItems()
{
	return items;
}
