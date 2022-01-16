/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
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
#include <QMap>
#include <QDir>
#include <QSettings>
#include <QProcessEnvironment>
#include <QCoreApplication>
#include <QBuffer>
#include <QDataStream>
#include <QVariantMap>
#include <QTextStream>

#include <map>
#include <vector>
#include <string>

#include "PathUtils.h"

#include <utility>

#if _DEBUG
#if defined(_WIN32)
#define _DBG_OUT(format, ...) { char bfr[2048];	sprintf(bfr, format, ##__VA_ARGS__); OutputDebugStringA(bfr); }
#else
#define _DBG_OUT(format, ...) fprintf(stderr, format, ##__VA_ARGS__)
#endif
#else
#define _DBG_OUT(format, ...)
#endif

class SysUtils {
public:
	static bool isWindows();
	static bool isMacOs();
	static bool isLinux();
	static QString getGixBuildPlatform();
	static void mergeEnvironmentVariable(QProcessEnvironment&, QString, QString, bool prepend = false, QChar sep = QDir::listSeparator());
	static QString mergeEnvironmentVariable(QString name, QString value, bool prepend = false, QChar sep = QDir::listSeparator());
	static QString mergeEnvironmentVariableValue(QString cur_value, QString value, QChar sep = QDir::listSeparator());
	static QString FileReadAllText(QString filename);
	static QStringList FileReadAllLines(QString filename);
	static QStringList FileReadAllLines(QTextStream *s);
	static QStringList FileReadLines(const QString& filename, int max_lines);
	static bool FileWriteAllLines(QString filename, QStringList lines);
	static QByteArray FileReadAll(QString filename);
	static QString RegistryGetValue(QString keypath, QString value, QString default_value = QString());
	static void RegistrySetValue(QString keypath, QString valuekey, QString value);
	static void mergeMaps(QMap<QString, QVariant>& m1, const QMap<QString, QVariant> m2);
	static QString serializeMap(QVariantMap *);
	static QMap<QString, QVariant> * deserializeMap(QString);
	static QString getjavaBinPath();
	static QString getCbl2XmlPath();
	static QString randomString(int len);
	static bool isWritableFile(const QString path);
	static bool isWritableDir(const QString path);
	static bool existsIntersection(const QStringList &l1, const QStringList &l2);
    static QString toHexString(uint32_t i32);
    static QString toHexString(uint64_t i64);

	template<typename T> static const char* enum_val_to_str(int v);

	template<typename KT, typename VT> static QMap<KT, VT>map_to_qmap(const std::map<KT, VT> &map)
	{
		QMap <KT, VT> outmap;

		for (auto const &mi: map) {
			outmap.insert(mi.first, mi.second);
		}

		return outmap;
	}

	static std::vector<std::string> to_std_string_vector(const QStringList &l);
	static QStringList to_qstringlist(const std::vector<std::string> &l);

	// Specialized
	static QMap<QString, int> to_qmap_qstring_int(const std::map<std::string, int> &map);
	static QMap<int, QString> to_qmap_int_qstring(const std::map<int, std::string> &map);
	static QMap<QString, QStringList> to_qmap_qstring_qstringlist(const std::map<std::string, std::vector<std::string>> &map);

	template<typename KV> static QMap<QString, KV> to_qmap_qstring_usertype(const std::map <std::string, KV> &map)
	{
		QMap<QString, KV> outmap;

		for (auto const &item : map) {
			outmap.insert(QString::fromStdString(item.first), item.second);
		}

		return outmap;
	}
};

inline QString SysUtils::getGixBuildPlatform()
{
	QString a = QSysInfo::buildCpuArchitecture();
	if (a == "x86_64")
		return "x64";

	if (a == "i386")
		return "x86";

	return a;
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
	if (s.isEmpty())
		return nullptr;

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

inline void SysUtils::mergeEnvironmentVariable(QProcessEnvironment& env, QString name, QString value, bool prepend, QChar sep)
{
	QString cur_value = env.value(name, "");
	QStringList components = cur_value.split(sep);
	if (prepend)
		components.insert(0, value);
	else
		components.append(value);

	components.removeAll("");
	env.insert(name, components.join(sep));
}

inline QString SysUtils::mergeEnvironmentVariable(QString var_name, QString value, bool prepend, QChar sep)
{
    QString cur_value(qgetenv(var_name.toUtf8().constData()));
	QStringList components = cur_value.split(sep);
	if (prepend)
		components.insert(0, value);
	else
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

inline void SysUtils::RegistrySetValue(QString keypath, QString valuekey, QString value)
{
	QSettings settings(keypath, QSettings::NativeFormat);
	settings.setValue(valuekey, value);
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

inline QString SysUtils::toHexString(uint32_t i32)
{
    return QString("0x%1").arg(i32, 8, 16, QLatin1Char( '0' ));
}

inline QString SysUtils::toHexString(uint64_t i64)
{
    return QString("0x%1").arg(i64, 16, 16, QLatin1Char( '0' ));
}

inline std::vector<std::string> SysUtils::to_std_string_vector(const QStringList &l)
{
	std::vector<std::string> outlist;

	for (QString s : l)
		outlist.push_back(s.toStdString());

	return outlist;
}

inline QStringList SysUtils::to_qstringlist(const std::vector<std::string> &l)
{
	QStringList outlist;

	for (std::string s : l)
		outlist.append(QString::fromStdString(s));

	return outlist;
}

inline QMap<QString, int> SysUtils::to_qmap_qstring_int(const std::map<std::string, int> &map)
{
	QMap<QString, int> outmap;

	for (auto const &item : map) {
		outmap.insert(QString::fromStdString(item.first), item.second);
	}

	return outmap;
}

inline QMap<int, QString> SysUtils::to_qmap_int_qstring(const std::map<int, std::string> &map)
{
	QMap<int, QString> outmap;

	for (auto const &item : map) {
		outmap.insert(item.first, QString::fromStdString(item.second));
	}

	return outmap;
}

inline QMap<QString, QStringList> SysUtils::to_qmap_qstring_qstringlist(const std::map<std::string, std::vector<std::string>> &map)
{
	QMap<QString, QStringList> outmap;

	for (auto const &item : map) {
		outmap.insert(QString::fromStdString(item.first), to_qstringlist(item.second));
	}

	return outmap;
}
