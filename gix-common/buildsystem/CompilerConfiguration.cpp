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

#include "CompilerConfiguration.h"
#include "PathUtils.h"
#include "SysUtils.h"
//#include "Ide.h"
#include "CompilerManager.h"
#include "QLogger.h"
#include "GixGlobals.h"

#include <QSettings>

#if !defined(__MINGW32__) && (defined(_WIN32) || defined(_WIN64))

#define _ATL_CSTRING_EXPLICIT_CONSTRUCTORS      // some CString constructors will be explicit

#include <atlbase.h>
#include <atlstr.h>
#include <atlsafe.h>
#include <wtypes.h>
#include <iostream>
#include <fstream>
#include <comutil.h>

#include "Setup.Configuration.h"

using namespace ATL;
#endif
#include <CompilerDefinition.h>

CompilerConfiguration * CompilerConfiguration::getCompilerById(QString compiler_id, QString target_platform)
{
	QSettings settings;
	CompilerConfiguration *cfg = nullptr;
	if (!compiler_id.isEmpty()) {
		CompilerDefinition* cd = GixGlobals::getCompilerManager()->getCompilers().take(compiler_id);
		if (!cd)
			return nullptr;

		//QString homedir = settings.value(QString("GnuCOBOL_C%1_HomeDir").arg(compiler_id), "").toString();
		//QString arch = target_platform == "x64" ? "_x64" : "";
		CompilerPlatformDefinition* cpd = cd->getPlatform(target_platform);
		if (!cpd)
			return nullptr;

		QString bindir = cpd->getBinDirPath();

		QString ext = QSysInfo::productType() == "windows" ? ".exe" : "";
		QString compiler_path = QDir::cleanPath(PathUtils::combine(bindir, QString("/cobc%1").arg(ext)));
		if (QFile(compiler_path).exists()) {
			cfg = new CompilerConfiguration();
			cfg->executablePath = compiler_path;
			cfg->homeDir = cd->getHomedir();
			cfg->binDirPath = cpd->getBinDirPath();
			cfg->libDirPath = cpd->getLibDirPath();
			cfg->configDirPath = cpd->getConfigDirPath();
			cfg->copyDirPath = cpd->getConfigDirPath();
			cfg->includeDirPath = cpd->getIncludeDirPath();
			cfg->runnerPath = QDir::cleanPath(PathUtils::combine(bindir, QString("/cobcrun%1").arg(ext)));
			cfg->host_platform = target_platform; // GnuCOBOL does NOT cross-compile
			cfg->target_platform = target_platform;
			cfg->executablePath = compiler_path;
			cfg->isVsBased = (!SysUtils::isWindows()) ? false : cd->isVsBased();
		}
	}
	return cfg;
}

CompilerConfiguration * CompilerConfiguration::get(QString build_configuration, QString target_platform)
{
	QSettings settings;
	QString path;
	QString id = (build_configuration == "release") ? settings.value("ReleaseCompilerId", "").toString() : settings.value("DebugCompilerId", "").toString();
	if (id.isEmpty())
		return nullptr;

	return getCompilerById(id, target_platform);
}

QProcessEnvironment CompilerConfiguration::getEnvironment(BuildDriver *builder)
{
	QProcessEnvironment env = QProcessEnvironment::systemEnvironment();

	QString host = this->target_platform;	// GnuCOBOL does NOT cross-compile
	QString target = this->target_platform;
	//QString bin_lib_suffix = target.toLower() == "x64" ? "_x64" : "";

	env.insert("COB_MAIN_DIR", homeDir);
	env.insert("COB_CONFIG_DIR", this->configDirPath);
	env.insert("COB_COPY_DIR", this->copyDirPath);
	
	if (!isVsBased) {
		QString mingw_basedir = homeDir.replace("([A-Za-z])\\:", "\\/\\1\\/");
		env.insert("COB_CONFIG_DIR", this->configDirPath);
		env.insert("COB_COPY_DIR", this->copyDirPath);
	}
	//psi.Environment.Add("COB_LIBRARY_PATH", PathUtils::combine(gchome, "extras"));

	SysUtils::mergeEnvironmentVariable(env, "PATH", this->binDirPath);

	if (isVsBased) {
		env.insert("COB_CFLAGS", "/I \"" + this->includeDirPath + "\"");
		env.insert("COB_LIB_PATHS", "/LIBPATH:\"" + this->libDirPath + "\"");
#if !defined(__MINGW32__) && (defined(_WIN32) || defined(_WIN64))
		if (!add_vs_environment(builder, env)) {
			builder->log_build_message("Cannot setup environment for Visual Studio", QLogger::LogLevel::Error);
		}
#endif
	}
	else {
		env.insert("COB_CFLAGS", "-I \"" + this->includeDirPath + "\"");
		env.insert("COB_LIB_PATHS", "-L \"" + this->libDirPath + "\"");
	}

	if (SysUtils::isLinux()) {
		SysUtils::mergeEnvironmentVariable(env, "LD_LIBRARY_PATH", this->libDirPath);
	}

	return env;
}

#if !defined(__MINGW32__) && (defined(_WIN32) || defined(_WIN64))
int option = 0;
bool vcToolsFound = false;
CComBSTR vcInstance;

bool GetInstanceData(BuildDriver *builder, CComPtr<ISetupInstance2>& instance2, CComPtr<ISetupHelper>& setupHelper, std::string& out_version, std::string& out_installation_path);

bool CompilerConfiguration::add_vs_environment(BuildDriver *builder, QProcessEnvironment& env)
{
	CoInitializeEx(NULL, COINIT_MULTITHREADED);

	CComPtr<ISetupConfiguration> setupConfig = nullptr;
	auto hresult = setupConfig.CoCreateInstance(__uuidof(SetupConfiguration), nullptr, CLSCTX_INPROC_SERVER);

	if (!SUCCEEDED(hresult) || setupConfig == nullptr)
	{
		builder->log_build_message("Visual Studio '15' may not be installed (Component creation failed)", QLogger::LogLevel::Error);
		return false;
	}

	CComQIPtr<ISetupConfiguration2> setupConfig2(setupConfig);
	CComQIPtr<ISetupHelper> setupHelper(setupConfig);
	if (!setupConfig2 || !setupHelper)
	{
		builder->log_build_message("Unsupported version of Visual Studio '15' may be installed (ISetupConfiguration2 or ISetupHelper unavailable)", QLogger::LogLevel::Error);
		return false;
	}

	CComPtr<IEnumSetupInstances> enumInstances;
	setupConfig2->EnumAllInstances(&enumInstances);
	if (!enumInstances)
	{
		builder->log_build_message("No VS '15' version is installed (EnumAllInstances returned null)", QLogger::LogLevel::Error);
		return false;
	}

	CComPtr<ISetupInstance> instance;

	if (!SUCCEEDED(enumInstances->Next(1, &instance, nullptr)) && instance)
		return false;

	CComQIPtr<ISetupInstance2> instance2(instance);
	if (!instance2) {
		builder->log_build_message("Error querying instance (ISetupInstance2 unavailable)", QLogger::LogLevel::Error);
		return false;
	}

	std::string vs_version;
	std::string vs_install_path;

	if (!GetInstanceData(builder, instance2, setupHelper, vs_version, vs_install_path)) {
		instance = nullptr;
		return false;
	}

	instance = nullptr;

	QString host = (host_platform == "x64") ? "x64" : "x86";
	QString target = (target_platform == "x64") ? "x64" : "x86";

	bool res = true;

	res = res && add_bin(env, host, target, QString::fromStdString(vs_install_path), QString::fromStdString(vs_version), builder);
	res = res && add_include(env, host, target, QString::fromStdString(vs_install_path), QString::fromStdString(vs_version), builder);
	res = res && add_lib(env, host, target, QString::fromStdString(vs_install_path), QString::fromStdString(vs_version), builder);
	res = res && add_libpath(env, host, target, QString::fromStdString(vs_install_path), QString::fromStdString(vs_version), builder);

	builder->log_build_message("PATH   : " + env.value("PATH"), QLogger::LogLevel::Debug);
	builder->log_build_message("INCLUDE: " + env.value("INCLUDE"), QLogger::LogLevel::Debug);
	builder->log_build_message("LIB    : " + env.value("LIB"), QLogger::LogLevel::Debug);
	builder->log_build_message("LIBPATH: " + env.value("LIBPATH"), QLogger::LogLevel::Debug);

	return res;
}


bool CompilerConfiguration::add_libpath(QProcessEnvironment& env, QString host, QString target, QString install_path, QString version, BuildDriver *builder)
{
	SysUtils::mergeEnvironmentVariable(env, "LIBPATH", "C:\\Program Files(x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\ATLMFC\\lib\\x64");
	SysUtils::mergeEnvironmentVariable(env, "LIBPATH", "C:\\Program Files(x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\lib\\x64");
	SysUtils::mergeEnvironmentVariable(env, "LIBPATH", "C:\\Program Files(x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\lib\\x86\\store\\references");
	SysUtils::mergeEnvironmentVariable(env, "LIBPATH", "C:\\Program Files(x86)\\Windows Kits\\10\\UnionMetadata\\10.0.16299.0");
	SysUtils::mergeEnvironmentVariable(env, "LIBPATH", "C:\\Program Files(x86)\\Windows Kits\\10\\References\\10.0.16299.0; C:\\WINDOWS\\Microsoft.NET\\Framework64\\v4.0.30319");
	return true;
}

bool CompilerConfiguration::add_lib(QProcessEnvironment& env, QString host, QString target, QString install_path, QString version, BuildDriver *builder)
{
	// MSVC
	QString msvc_home = PathUtils::combine(install_path, "VC");
	QString vctools_version_file = PathUtils::combine(msvc_home, "Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt");
	QString vctools_version = SysUtils::FileReadAllText(vctools_version_file).trimmed();
	QString vctools_dir = PathUtils::combine(msvc_home, "Tools\\MSVC\\" + vctools_version);
	SysUtils::mergeEnvironmentVariable(env, "LIB", PathUtils::combine({ vctools_dir, "ATLMFC", "lib", target }));
	SysUtils::mergeEnvironmentVariable(env, "LIB", PathUtils::combine({ vctools_dir, "lib", target }));

	// Windows SDK
	QString sdk_version;
	QStringFuncCoalescer c;
	c.push_back([&] { return get_win_sdk_path("Microsoft\\Microsoft SDKs\\Windows\\v10.0", sdk_version); });
	c.push_back([&] { return get_win_sdk_path("Microsoft\\Microsoft SDKs\\Windows\\v8.1", sdk_version); });
	QString sdk_path = c.get();

	if (!sdk_path.isEmpty()) {
		sdk_version += ".0";
		SysUtils::mergeEnvironmentVariable(env, "LIB", PathUtils::combine({ sdk_path, "lib", sdk_version, "ucrt", target }));
		SysUtils::mergeEnvironmentVariable(env, "LIB", PathUtils::combine({ sdk_path, "lib", sdk_version, "um", target }));
		return true;
	}

	return false;
}

bool CompilerConfiguration::add_include(QProcessEnvironment& env, QString host, QString target, QString install_path, QString version, BuildDriver *builder)
{
	// MSVC
	QString msvc_home = PathUtils::combine(install_path, "VC");
	QString vctools_version_file = PathUtils::combine(msvc_home, "Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt");
	QString vctools_version = SysUtils::FileReadAllText(vctools_version_file).trimmed();
	QString vctools_dir = PathUtils::combine(msvc_home, "Tools\\MSVC\\" + vctools_version);
	SysUtils::mergeEnvironmentVariable(env, "INCLUDE", PathUtils::combine({ vctools_dir, "ATLMFC", "include" }));
	SysUtils::mergeEnvironmentVariable(env, "INCLUDE", PathUtils::combine({ vctools_dir, "include" }));

	// Windows SDK
	QString sdk_version;

	//COALESCE(f() OR_ELSE g() OR_ELSE h());

	QStringFuncCoalescer c;
	c.push_back([&] { return get_win_sdk_path("Microsoft\\Microsoft SDKs\\Windows\\v10.0", sdk_version); });
	c.push_back([&] { return get_win_sdk_path("Microsoft\\Microsoft SDKs\\Windows\\v8.1", sdk_version); });
	QString sdk_path = c.get();

	if (!sdk_path.isEmpty()) {
		sdk_version += ".0";
		SysUtils::mergeEnvironmentVariable(env, "INCLUDE", PathUtils::combine({ sdk_path, "include", sdk_version, "ucrt" }));
		SysUtils::mergeEnvironmentVariable(env, "INCLUDE", PathUtils::combine({ sdk_path, "include", sdk_version, "shared" }));
		SysUtils::mergeEnvironmentVariable(env, "INCLUDE", PathUtils::combine({ sdk_path, "include", sdk_version, "um" }));
		SysUtils::mergeEnvironmentVariable(env, "INCLUDE", PathUtils::combine({ sdk_path, "include", sdk_version, "winrt" }));
		SysUtils::mergeEnvironmentVariable(env, "INCLUDE", PathUtils::combine({ sdk_path, "include", sdk_version, "cppwinrt" }));

		return true;
	}
	return false;
}

bool CompilerConfiguration::add_bin(QProcessEnvironment& env, QString host, QString target, QString install_path, QString version, BuildDriver *builder)
{
	QString major_version = version.left(version.indexOf("."));

	// MSBuild
	QString msbuild_bin_path = PathUtils::combine(install_path, "MSBuild\\" + major_version + ".0\\bin");
	SysUtils::mergeEnvironmentVariable(env, "PATH", msbuild_bin_path);

	// MSVC

	QString msvc_target_platform = (target == "x64") ? "x64" : "x86";
	QString msvc_home = PathUtils::combine(install_path, "VC");
	QString vctools_version_file = PathUtils::combine(msvc_home, "Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt");
	QString vctools_version = SysUtils::FileReadAllText(vctools_version_file).trimmed();
	QString vctools_bin_dir = PathUtils::combine(msvc_home, "Tools\\MSVC\\" + vctools_version + "\\bin\\Host" + msvc_target_platform + "\\" + msvc_target_platform);

	if (!QDir(vctools_bin_dir).exists()) {
		builder->log_build_message("No such directory: " + vctools_bin_dir, QLogger::LogLevel::Error);
		return false;
	}

	if (!QFile(PathUtils::combine(vctools_bin_dir, "cl.exe")).exists()) {
		builder->log_build_message("Cannot find cl.exe", QLogger::LogLevel::Error);
		return false;
	}

	SysUtils::mergeEnvironmentVariable(env, "PATH", vctools_bin_dir);


	// Windows SDK
	QString sdk_version;
	QStringFuncCoalescer c;
	c.push_back([&] { return get_win_sdk_path("Microsoft\\Microsoft SDKs\\Windows\\v10.0", sdk_version); });
	c.push_back([&] { return get_win_sdk_path("Microsoft\\Microsoft SDKs\\Windows\\v8.1", sdk_version); });
	QString sdk_path = c.get();

	if (!sdk_path.isEmpty()) {
		sdk_version += ".0";
		//TaskUtils.LogMessage(_build_engine, "Windows SDK Path: " + sdk_path);
		env.insert("WindowsSdkDir", sdk_path);
		QString sdk_bin_path = PathUtils::combine({ sdk_path, "bin", sdk_version, target });
		env.insert("WindowsSdkBinPath", sdk_bin_path);
		SysUtils::mergeEnvironmentVariable(env, "PATH", sdk_bin_path);

		return true;
	}
	return false;
}


QString CompilerConfiguration::get_win_sdk_path(QString p, QString& version)
{
	QStringFuncCoalescer c;
	c.push_back([&] { return SysUtils::RegistryGetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\" + p, "InstallationFolder"); });
	c.push_back([&] { return SysUtils::RegistryGetValue("HKEY_CURRENT_USER\\SOFTWARE\\Wow6432Node\\" + p, "InstallationFolder"); });
	c.push_back([&] { return SysUtils::RegistryGetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\" + p, "InstallationFolder"); });
	c.push_back([&] { return SysUtils::RegistryGetValue("HKEY_CURRENT_USER\\SOFTWARE\\" + p, "InstallationFolder"); });

	QString sdk_path = c.get();

	c.clear();
	c.push_back([&] { return SysUtils::RegistryGetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\" + p, "ProductVersion"); });
	c.push_back([&] { return SysUtils::RegistryGetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\" + p, "ProductVersion"); });
	c.push_back([&] { return SysUtils::RegistryGetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\" + p, "ProductVersion"); });
	c.push_back([&] { return SysUtils::RegistryGetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\" + p, "ProductVersion"); });

	version = c.get();

	return sdk_path;
}


std::string bstr_to_str(BSTR source) {
	//source = L"lol2inside";
	_bstr_t wrapped_bstr = _bstr_t(source);
	int length = wrapped_bstr.length();
	char* char_array = new char[length];
	strcpy_s(char_array, length + 1, wrapped_bstr);
	return char_array;
}

bool GetInstanceData(BuildDriver *builder, CComPtr<ISetupInstance2>& instance2, CComPtr<ISetupHelper>& setupHelper, std::string& out_version, std::string& out_installation_path)
{
	USES_CONVERSION;
	vcToolsFound = false;
	CComBSTR	bstrId;

	if (FAILED(instance2->GetInstanceId(&bstrId))) {
		builder->log_build_message("Error reading instance id", QLogger::LogLevel::Error);
		return false;
	}
	vcInstance = bstrId.Copy();
	InstanceState state;
	if (FAILED(instance2->GetState(&state))) {
		builder->log_build_message("Error reading instance state", QLogger::LogLevel::Error);
		return false;
	}

	CComBSTR	bstrVersion;
	if (FAILED(instance2->GetInstallationVersion(&bstrVersion))) {
		builder->log_build_message("Error reading version", QLogger::LogLevel::Error);
		return false;
	}
	else
	{
		out_version = bstr_to_str(bstrVersion);
		builder->log_build_message("Visual Studio version: " + QString(out_version.c_str()), QLogger::LogLevel::Trace);
	}

	// Reboot may have been required before the installation path was created.
	if ((eLocal & state) == eLocal)
	{
		CComBSTR bstrInstallationPath;
		if (FAILED(instance2->GetInstallationPath(&bstrInstallationPath))) {
			builder->log_build_message("Error getting installation path", QLogger::LogLevel::Error);
			return false;
		}

		out_installation_path = bstr_to_str(bstrInstallationPath);
		builder->log_build_message("Visual Studio installation path: " + QString(out_installation_path.c_str()), QLogger::LogLevel::Trace);
		return true;
	}

	return false;
}

#endif // defined(_WIN32) || defined(_WIN64)
