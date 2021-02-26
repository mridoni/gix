#include "PathUtils.h"
#include <QRegularExpression>

QString PathUtils::combine(QString p1, QString p2)
{
	QString s = p1.isEmpty() || (p1.endsWith("/") || p1.endsWith("\\")) ? p1 : p1 + QDir::separator();
	return QDir::cleanPath(s + p2);
}

QString PathUtils::combine(std::initializer_list<QString> a_args)
{
	QString res = "";

	for (auto s : a_args)
		res = PathUtils::combine(res, s);

	return QDir::cleanPath(res);
}

QString PathUtils::getFilename(QString p)
{
	return QFileInfo(p).fileName();
}

QString PathUtils::getAbsoluteDirectory(QString p)
{
	return QFileInfo(p).absoluteDir().absolutePath();
}

QString PathUtils::getDirectory(QString p)
{
	if (p.contains("/") || p.contains("\\")) {
		int n = p.lastIndexOf(QRegularExpression("[\\/\\\\]"));
		return p.mid(0, n);
	}
	else {
		return p;
	}
}

bool PathUtils::isValidDirectoryName(QString s)
{
	QString::iterator it;
	QString illegalChars = "\\/:?\"<>|";
	for (it = s.begin(); it < s.end(); ++it) {
		if (illegalChars.contains(*it))
			return false;
	}
	return true;
}

bool PathUtils::isValidFileName(QString s)
{
	QString::iterator it;
	QString illegalChars = "\\/:?\"<>|";
	for (it = s.begin(); it < s.end(); ++it) {
		if (illegalChars.contains(*it))
			return false;
	}
	return true;
}

QString PathUtils::toModuleName(QString s)
{
	QFileInfo f(s);
	return f.baseName().toUpper();
}

QString PathUtils::quote(QString s)
{
	return "\"" + s + "\"";
}

QString PathUtils::changeExtension(QString s, QString newext)
{
	QFileInfo fi(s);
	QString ext = fi.suffix();
	if (ext == "")
		return s + newext;

	return s.left((s.length() - ext.length())-1) + newext;
}

QString PathUtils::rebasePath(QString child, QString parent)
{
	QString c = QDir::cleanPath(child);
	QString p = QDir::cleanPath(parent);

	if (c.startsWith(p)) {
		return c.mid(parent.length() + 1);
	}
	else {
		return c;
	}
}



