#pragma once

#include <QString>
#include <QMap>
#include <QDomNode>

/*
  <id>gnucobol-2.2-dbg-vs-all</id>
  <name>GnuCOBOL 2.2 Debugger (VS) x86/x64</name>
  <version>2.2</version>
  <homedir>${gixhome}/compilers/vs</homedir>
  <host_platform>x64</host_platform>
  <target_platforms>x86,x64</target_platforms>
  <is_vs_based>true</is_vs_based>
  <supports_anim>true</supports_anim>

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

class CompilerPlatformDefinition
{
	friend class CompilerDefinition;

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

class CompilerDefinition
{
public:
	static CompilerDefinition* load(QString def_path);

	CompilerDefinition();
	~CompilerDefinition();

	QString getId() {
		return id;
	}

	void setId(QString id) {
		id = id;
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

	bool hasAnimSupport() {
		return supports_anim;
	}

	void setHasAnimSupport(bool _supports_anim) {
		supports_anim = _supports_anim;
	}

	const QMap<QString, CompilerPlatformDefinition*> getTargetPlatforms()
	{
		return target_platforms;
	}

	CompilerPlatformDefinition* getPlatform(QString platform_id);

private:
	QString def_file;

	QMap<QString, CompilerPlatformDefinition*> target_platforms;

	QString id;
	QString name;
	QString version;
	QString homedir;
	QString host_platform;
	bool is_vs_based;
	bool supports_anim;

private:
	static CompilerPlatformDefinition *parsePlatform(QDomNode& xn, QString homedir);

	bool validate();
};

