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

#include <QString>
#include <QMap>
#include <QDomNode>

#include "gixcommon_global.h"

/*
  <id>gnucobol-2.2-dbg-vs-all</id>
  <name>GnuCOBOL 2.2 Debugger (VS) x86/x64</name>
  <version>2.2</version>
  <homedir>${gixhome}/compilers/vs</homedir>
  <host_platform>x64</host_platform>
  <target_platforms>x86,x64</target_platforms>
  <is_vs_based>true</is_vs_based>

  <platform id="x86">
	<bin_dir_path>${homedir}/bin</bin_dir_path>
	<lib_dir_path>${homedir}/lib</lib_dir_path>
	<include_dir_path>${homedir}/include</include_dir_path>
	<config_dir_path>${homedir}/config</config_dir_path>
	<copy_dir_path>${homedir}/copy</copy_dir_path>
  </platform>

  <platform id="x64">
	<bin_dir_path>${homedir}/bin_x64</bin_dir_path>
	<lib_dir_path>${homedir}/lib_x64</lib_dir_path>
	<include_dir_path>${homedir}/include</include_dir_path>
	<config_dir_path>${homedir}/config</config_dir_path>
	<copy_dir_path>${homedir}/copy</copy_dir_path>
  </platform>

*/

class GIXCOMMON_EXPORT CompilerPlatformDefinition
{
	friend GIXCOMMON_EXPORT class CompilerDefinition;

public:

	QString getBinDirPath() {
		return bin_dir_path;
	}

	void setBinDirPath(QString _binDirPath) {
		bin_dir_path = _binDirPath;
	}

	QString getLibDirPath() {
		return lib_dir_path;
	}

	void setLibDirPath(QString _libDirPath) {
		lib_dir_path = _libDirPath;
	}

	QString getIncludeDirPath() {
		return include_dir_path;
	}

	void setIncludeDirPath(QString _includeDirPath) {
		include_dir_path = _includeDirPath;
	}

	QString getConfigDirPath() {
		return config_dir_path;
	}

	void setConfigDirPath(QString _configDirPath) {
		config_dir_path = _configDirPath;
	}

	QString getCopyDirPath() {
		return copy_dir_path;
	}

	void setCopyDirPath(QString _copyDirPath) {
		copy_dir_path = _copyDirPath;
	}
		


private:
	QString bin_dir_path;
	QString lib_dir_path;
	QString include_dir_path;
	QString config_dir_path;
	QString copy_dir_path;
};

class GIXCOMMON_EXPORT CompilerDefinition
{
public:
	static CompilerDefinition* load(QString def_path);
	static bool testConfiguration(QString home_dir, QStringList &info, CompilerDefinition **cdef);

	CompilerDefinition();
	~CompilerDefinition();

	void setDefinitionFile(QString f) { def_file = f; }

	bool save();

	bool testConfiguration(QStringList &info);

	QString getId() {
		return id;
	}

	void setId(QString _id) {
		id = _id;
	}

	QString getName() {
		return name;
	}

	void setName(QString _name) {
		name = _name;
	}

	QString getVersion() {
		return version;
	}

	void setVersion(QString _version) {
		version = _version;
	}

	QString getHomedir() {
		return homedir;
	}

	void setHomedir(QString _homedir) {
		homedir = _homedir;
	}

	QString getHostPlatform() {
		return host_platform;
	}

	void setHostPlatform(QString _host_platform) {
		host_platform = _host_platform;
	}

	bool isVsBased() {
		return is_vs_based;
	}

	void setIsVsBased(bool _is_vs_based) {
		is_vs_based = _is_vs_based;
	}

	const QMap<QString, CompilerPlatformDefinition*> getTargetPlatforms()
	{
		return target_platforms;
	}

	CompilerPlatformDefinition* getPlatform(QString platform_id);

	void addPlatform(QString platform_id, CompilerPlatformDefinition *cd);

private:
	QString def_file;

	QMap<QString, CompilerPlatformDefinition*> target_platforms;

	QString id;
	QString name;
	QString version;
	QString homedir;
	QString host_platform;
	bool is_vs_based;

private:
	static CompilerPlatformDefinition* parsePlatform(CompilerDefinition* def, QDomNode& xn);

	bool validate();
};

