/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#pragma once

#include <QSysInfo>
#include <QChar>
#include <QDir>
#include <QSettings>
#include <QProcessEnvironment>
#include <QCoreApplication>
#include <QBuffer>
#include <QDataStream>
#include <QVariantMap>
#include <QTextStream>

#include "PathUtils.h"

#include <utility>

class SysUtils {
public:
	static bool isWindows();
	static bool isMacOs();
	static bool isLinux();
	static void mergeEnvironmentVariable(QProcessEnvironment&, QString, QString, QChar sep = QDir::listSeparator());
	static QString mergeEnvironmentVariable(QString name, QString value, QChar  sep = QDir::listSeparator());
	static QString mergeEnvironmentVariableValue(QString cur_value, QString value, QChar sep = QDir::listSeparator());
	static QString FileReadAllText(QString filename);
	static QStringList FileReadAllLines(QString filename);
	static QStringList FileReadAllLines(QTextStream *s);
	static QStringList FileReadLines(const QString& filename, int max_lines);
	static bool FileWriteAllLines(QString filename, QStringList lines);
	static QByteArray FileReadAll(QString filename);
	static QString RegistryGetValue(QString keypath, QString value, QString default_value = QString());
	static void mergeMaps(QMap<QString, QVariant>& m1, const QMap<QString, QVariant> m2);
	static QString getSysCopyDir();
	static QString serializeMap(QVariantMap *);
	static QMap<QString, QVariant> * deserializeMap(QString);
	static QVariant getSubProperty(QString serialized_sub_props, QString sub_prop_id);
	static QString getjavaBinPath();
	static QString getCbl2XmlPath();
	static QString randomString(int len);
	static bool isWritableFile(const QString path);
	static bool isWritableDir(const QString path);
	static bool existsIntersection(const QStringList &l1, const QStringList &l2);
	
	template<typename T> static const char* enum_val_to_str(int v);
};

inline QString SysUtils::getSysCopyDir()
{
	QString v = QProcessEnvironment::systemEnvironment().value("GIX_COPY_DIR", QString());
	if (!v.isEmpty())
		return v;

	QDir appDir(QCoreApplication::applicationDirPath());
	appDir.cdUp();
	if (appDir.cd("copy"))
		return appDir.currentPath();

	return QString();
}

inline QString SysUtils::serializeMap(QVariantMap * m)
{
	QByteArray mapData;
	QDataStream outStream(&mapData, QIODevice::WriteOnly);
	outStream << (*m);
	return QString::fromLocal8Bit(mapData.toBase64());
}

inline QMap<QString, QVariant>* SysUtils::deserializeMap(QString s)
{
	QVariantMap *inMap = new QVariantMap();
	QByteArray mapData = QByteArray::fromBase64(s.toLocal8Bit());

	QDataStream inStream(&mapData, QIODevice::ReadOnly);
	inStream >> *inMap;
	return inMap;
}

inline QString SysUtils::getjavaBinPath()
{
	QSettings settings;

	QString jre_path = settings.value("JAVA_JrePath").toString();
	if (jre_path.isEmpty()) {
		jre_path = QProcessEnvironment::systemEnvironment().value("JAVA_HOME", QString());
	}

	if (jre_path.isEmpty() || !QDir(jre_path).exists())
		return QString();
		
	QString java_path = PathUtils::combine({ jre_path, "bin", "java" });
	if (SysUtils::isWindows())
		java_path = PathUtils::changeExtension(java_path, ".exe");

	if (QFile::exists(java_path) && QFileInfo(java_path).isExecutable())
		return java_path;

	return QString();
}

inline QString SysUtils::getCbl2XmlPath()
{
	QSettings settings;

	QString cbl2xml_path = settings.value("JAVA_Cb2XmlPath").toString();

	if (cbl2xml_path.isEmpty() || !QFile(cbl2xml_path).exists())
		return QString();

	return cbl2xml_path;
}

inline bool SysUtils::isWindows()
{
#if defined(Q_OS_WIN)
	return true;
#else
	return false;
#endif
}

inline bool SysUtils::isMacOs()
{
#if defined(Q_OS_MAC)
	return true;
#else
	return false;
#endif
}

inline bool SysUtils::isLinux()
{
#if defined(Q_OS_LINUX)
	return true;
#else
	return false;
#endif
}

inline void SysUtils::mergeEnvironmentVariable(QProcessEnvironment& env, QString name, QString value, QChar sep)
{
	QString cur_value = env.value(name, "");
	QStringList components = cur_value.split(sep);
	components.append(value);
	components.removeAll("");
	env.insert(name, components.join(sep));
}

inline QString SysUtils::mergeEnvironmentVariable(QString name, QString value, QChar sep)
{
	QString cur_value(qgetenv(name.toUtf8().constData()));
	QStringList components = cur_value.split(sep);
	components.append(value);
	components.removeAll("");
	return components.join(sep);
}

inline QString SysUtils::mergeEnvironmentVariableValue(QString cur_value, QString value, QChar sep)
{
	QStringList components = cur_value.split(sep);
	components.append(value);
	components.removeAll("");
	return components.join(sep);
}

inline QString SysUtils::FileReadAllText(QString filename)
{
	QString res;
	QFile f(filename);
	if (f.exists()) {
		if (f.open(QIODevice::OpenModeFlag::ReadOnly)) {
			res = QString::fromUtf8(f.readAll());
			f.close();
		}
	}
	return res;
}

inline QByteArray SysUtils::FileReadAll(QString filename)
{
	QByteArray res;
	QFile f(filename);
	if (f.exists()) {
		if (f.open(QIODevice::OpenModeFlag::ReadOnly)) {
			res = f.readAll();
			f.close();
		}
	}
	return res;
}


inline QStringList SysUtils::FileReadAllLines(QString filename)
{
	QString content = FileReadAllText(filename);
	return content.split(QRegExp("\n|\r\n|\r"));
}

inline QStringList SysUtils::FileReadAllLines(QTextStream *s)
{
	QString content = s->readAll();
	return content.split(QRegExp("\n|\r\n|\r"));
}

inline QStringList SysUtils::FileReadLines(const QString &filename, int max_lines)
{
	QStringList res;

	QFile f(filename);
	if (f.open(QIODevice::ReadOnly | QIODevice::Text)) {

		QTextStream ts(&f);
		int nread = 0;

		while (!ts.atEnd() && nread <= max_lines) {
			res.append(ts.readLine());
			nread++;
		}

		f.close();
	}
	return res;
}

inline bool SysUtils::FileWriteAllLines(QString filename, QStringList lines)
{
	QFile f(filename);
	if (!f.open(QIODevice::OpenModeFlag::Text | QIODevice::OpenModeFlag::WriteOnly))
		return false;

	QTextStream s(&f);
	for (QString ln : lines)
		s << ln << "\n";

	f.close();

	return true;
}

inline QString SysUtils::RegistryGetValue(QString keypath, QString valuekey, QString default_value)
{
	QSettings settings(keypath, QSettings::NativeFormat);
	return settings.value(valuekey, default_value).toString();
}

inline void SysUtils::mergeMaps(QMap<QString, QVariant>& m1, const QMap<QString, QVariant> m2)
{
	for (auto i = m2.constBegin(); i != m2.constEnd(); ++i) {
		m1.insert(i.key(), i.value());
	}
}

inline QString SysUtils::randomString(int len)
{
	QString str;
	str.resize(len);
	for (int s = 0; s < len; ++s)
		str[s] = QChar('A' + char(qrand() % ('Z' - 'A')));

	return str;
}


inline QVariant SysUtils::getSubProperty(QString serialized_sub_props, QString sub_prop_id)
{
	auto sub_props = SysUtils::deserializeMap(serialized_sub_props);
	if (sub_props && sub_props->contains(sub_prop_id) && !sub_props->value(sub_prop_id).isNull()) {
		return sub_props->value(sub_prop_id);
	}
	return QVariant();
}

/*

namespace coalesce_impl
{
	template<typename LHS, typename RHS>
	auto coalesce(LHS lhs, RHS rhs) ->
		typename std::remove_reference<decltype(lhs())>::type&&
	{
		auto&& initialValue = lhs();
		if (initialValue)
			return std::move(initialValue);
		else
			return std::move(rhs());
	}

	template<typename LHS, typename RHS, typename ...RHSs>
	auto coalesce(LHS lhs, RHS rhs, RHSs ...rhss) ->
		typename std::remove_reference<decltype(lhs())>::type&&
	{
		auto&& initialValue = lhs();
		if (initialValue)
			return std::move(initialValue);
		else
			return std::move(coalesce(rhs, rhss...));
	}
}

#define COALESCE(x) (::coalesce_impl::coalesce([&](){ return ( x ); }))
#define OR_ELSE     ); }, [&](){ return (

*/

class QStringFuncCoalescer : public std::vector<std::function<QString()>> {

public:
	QString get();
};

inline QString QStringFuncCoalescer::get()
{
	for (int i = 0; i < this->size(); i++) {
		std::function<QString()> fcn = this->at(i);
		if (fcn == nullptr)
			continue;

		QString s = fcn();
		if (!s.isEmpty())
			return s;
	}
	return QString();

}


inline bool SysUtils::isWritableFile(const QString path)
{
	QFileInfo f(path);
	return isWritableDir(f.absoluteDir().absolutePath());
}

inline bool SysUtils::isWritableDir(const QString path)
{
	QFileInfo my_dir(path);

	return (my_dir.isDir() && my_dir.isWritable());
}

inline bool SysUtils::existsIntersection(const QStringList &l1, const QStringList &l2)
{
	for (const QString &s : l1) {
		if (l2.contains(s))
			return true;
	}
	return false;
}