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

#include "CompilerDefinition.h"

#include "linq/linq.hpp"

#include <QDomDocument>
#include <QFile>
#include <QVariant>
#include <QXmlQuery>
#include <PathUtils.h>
#include <QBuffer>
#include <QtCore>

#include "IGixLogManager.h"
#include "GixGlobals.h"
#include "SysUtils.h"
#include "CompilerConfiguration.h"


#if _WIN32
#include <Windows.h>
#define COBC_EXE_NAME		"cobc.exe"
#define COBCRUN_EXE_NAME	"cobcrun.exe"
#else
#define COBC_EXE_NAME		"cobc"
#define COBCRUN_EXE_NAME	"cobcrun"
#endif

CompilerDefinition::CompilerDefinition()
{
	is_vs_based = false;
}

CompilerDefinition::~CompilerDefinition()
{
	if (target_platforms.size() == 0)
		return;

	QMap<QString, CompilerPlatformDefinition *>::iterator i;
	for (i = target_platforms.begin(); i != target_platforms.end(); ++i) {
		delete i.value();
	}

	target_platforms.clear();
}

CompilerPlatformDefinition *CompilerDefinition::parsePlatform(QDomNode &xn, QString homedir)
{
	IGixLogManager *logger = GixGlobals::getLogManager();

	CompilerPlatformDefinition *cpd = new CompilerPlatformDefinition();
	cpd->bin_dir_path = QDir::fromNativeSeparators(xn.firstChildElement("bin_dir_path").text().replace("${homedir}", homedir));
	cpd->lib_dir_path = QDir::fromNativeSeparators(xn.firstChildElement("lib_dir_path").text().replace("${homedir}", homedir));
	cpd->include_dir_path = QDir::fromNativeSeparators(xn.firstChildElement("include_dir_path").text().replace("${homedir}", homedir));
	cpd->config_dir_path = QDir::fromNativeSeparators(xn.firstChildElement("config_dir_path").text().replace("${homedir}", homedir));
	cpd->copy_dir_path = QDir::fromNativeSeparators(xn.firstChildElement("copy_dir_path").text().replace("${homedir}", homedir));

	logger->logMessage(GIX_CONSOLE_LOG, "Validating plaform " + xn.attributes().namedItem("id").nodeValue(), QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " bin_dir_path    :" + cpd->bin_dir_path, QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " lib_dir_path    :" + cpd->lib_dir_path, QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " include_dir_path:" + cpd->include_dir_path, QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " config_dir_path :" + cpd->config_dir_path, QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " copy_dir_path   :" + cpd->copy_dir_path, QLogger::LogLevel::Debug);

	if (!QDir(cpd->bin_dir_path).exists() || !QDir(cpd->lib_dir_path).exists() || !QDir(cpd->include_dir_path).exists() ||
		!QDir(cpd->config_dir_path).exists() || !QDir(cpd->copy_dir_path).exists()) {

		delete cpd;
		return nullptr;
	}

	return cpd;
}

bool CompilerDefinition::validate()
{
	IGixLogManager *logger = GixGlobals::getLogManager();

	logger->logMessage(GIX_CONSOLE_LOG, "CompilerManager: validating " + this->def_file + "", QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " id           : " + this->id, QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " name         : " + this->name, QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " version      : " + this->version, QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " host_platform: " + this->host_platform, QLogger::LogLevel::Debug);
	logger->logMessage(GIX_CONSOLE_LOG, " homedir      : " + this->homedir, QLogger::LogLevel::Debug);
	return !id.isEmpty() && !name.isEmpty() && !version.isEmpty() && !host_platform.isEmpty() && QDir(homedir).exists();
}

CompilerDefinition *CompilerDefinition::load(QString def_path)
{
	IGixLogManager *logger = GixGlobals::getLogManager();
	CompilerDefinition *def = new CompilerDefinition();
	def->def_file = def_path;

	QDomDocument doc;
	QFile prj_file(def_path);
	if (!prj_file.open(QIODevice::ReadOnly) || !doc.setContent(&prj_file)) {
		logger->logMessage(GIX_CONSOLE_LOG, "Compiler definition is invalid or cannot be found (" + def_path + ")", QLogger::LogLevel::Error);
		return nullptr;
	}

	QDomNode root = doc.elementsByTagName("gix-ide-compiler-def").item(0);
	QString gix_home = GixGlobals::getGixHomeDir();
	QString gix_data = GixGlobals::getGixDataDir();
	QString homedir;

	homedir = root.firstChildElement("homedir").text().replace("${gixhome}", gix_home);
	homedir = root.firstChildElement("homedir").text().replace("${gixdata}", gix_data);

	def->id = root.firstChildElement("id").text();
	def->name = root.firstChildElement("name").text();
	def->version = root.firstChildElement("version").text();
	def->homedir = homedir;
	def->host_platform = root.firstChildElement("host_platform").text();
	def->is_vs_based = QVariant(root.firstChildElement("is_vs_based").text()).toBool();

	QStringList target_platforms = root.firstChildElement("target_platforms").text().split(",");

	auto xns = root.childNodes();
	for (int i = 0; i < xns.size(); i++) {
		auto xn = xns.at(i);
		QString s = xn.nodeName();
		if (xn.nodeName() != "platform")
			continue;

		if (xn.attributes().contains("id")) {
			QString platform = xn.attributes().namedItem("id").nodeValue();
			if (!target_platforms.contains(platform)) {
				logger->logMessage(GIX_CONSOLE_LOG, "Invalid platform entry in " + def_path, QLogger::LogLevel::Error);
				continue;
			}

			CompilerPlatformDefinition *cpd = parsePlatform(xn, homedir);
			if (cpd == nullptr) {
				logger->logMessage(GIX_CONSOLE_LOG, "Cannot parse platform " + platform + " (" + def_path + ")", QLogger::LogLevel::Error);
				continue;
			}

			def->target_platforms[platform] = cpd;
			logger->logMessage(GIX_CONSOLE_LOG, "Adding compiler " + def->name, QLogger::LogLevel::Info);
		}
	}

	if (!def->validate()) {
		logger->logMessage(GIX_CONSOLE_LOG, "Validation failed (" + def_path + ")", QLogger::LogLevel::Error);
		delete def;
		return nullptr;
	}

	return def;
}

void CompilerDefinition::addPlatform(QString platform_id, CompilerPlatformDefinition *cd)
{
	if (platform_id.isEmpty() || !cd)
		return;

	target_platforms[platform_id] = cd;
}

void appendElement(QDomDocument &doc, QDomElement &parent, QString name, QString content)
{
	QDomElement xid = doc.createElement(name);
	xid.appendChild(doc.createTextNode(content));
	parent.appendChild(xid);

}

bool CompilerDefinition::save()
{
	QDomDocument doc;

	QDomElement xdef = doc.createElement("gix-ide-compiler-def");
	doc.appendChild(xdef);

	appendElement(doc, xdef, "id", this->id);
	appendElement(doc, xdef, "name", this->name);
	appendElement(doc, xdef, "version", this->version);
	appendElement(doc, xdef, "homedir", this->homedir);
	appendElement(doc, xdef, "host_platform", this->host_platform);
	appendElement(doc, xdef, "target_platforms", this->target_platforms.keys().join(','));
	appendElement(doc, xdef, "is_vs_based", this->is_vs_based ? "true" : "false");

	for (QString cpd_id : target_platforms.keys()) {
		CompilerPlatformDefinition *cpd = target_platforms.value(cpd_id);

		QDomElement xcpd = doc.createElement("platform");
		xdef.appendChild(xcpd);

		xcpd.setAttribute("id", cpd_id);
		appendElement(doc, xcpd, "bin_dir_path", cpd->bin_dir_path);
		appendElement(doc, xcpd, "lib_dir_path", cpd->lib_dir_path);
		appendElement(doc, xcpd, "include_dir_path", cpd->include_dir_path);
		appendElement(doc, xcpd, "config_dir_path", cpd->config_dir_path);
		appendElement(doc, xcpd, "copy_dir_path", cpd->copy_dir_path);
	}

	QFile file(def_file);

	if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
		//qDebug("Failed to open file for writing: " + filepath);
		return false;
	}

	QTextStream stream(&file);
	stream << doc.toString();
	file.close();

	return true;
}



bool CompilerDefinition::testConfiguration(QStringList &info)
{
	
	QString prj_basename = SysUtils::randomString(8);
	QTemporaryDir prj_dir(PathUtils::combine(QDir::tempPath(), prj_basename));

	QString prj_path = PathUtils::combine(prj_dir.path(), prj_basename + ".gix");

	ProjectCollection *ppj = ProjectCollection::newProjectCollection(ProjectType::SingleBinary, prj_path, QVariantMap());
	if (!ppj) {
		info.append(QCoreApplication::translate("gix", "Cannot create temporary project (1)"));
		return false;
	}

	if (!ppj->save()) {
		info.append(QCoreApplication::translate("gix", "Cannot create temporary project (2)"));
		return false;
	}

	if (!ppj->GetChildren()->size()) {
		info.append(QCoreApplication::translate("gix", "Cannot create temporary project (3)"));
		return false;
	}

	Project *prj = (Project *)ppj->GetChildren()->at(0);
	if (!prj->GetChildren()->size()) {
		info.append(QCoreApplication::translate("gix", "Cannot create temporary project (4)"));
		return false;
	}

	prj->PropertySetValue("preprocess_esql", false);
	prj->PropertySetValue("build_type", "dll");
	prj->save();

	ProjectFile *pf = (ProjectFile *)prj->GetChildren()->at(0);
	QString filepath = pf->GetFileFullPath();
	QFile prj_file(filepath);
	if (prj_file.exists()) {
		info.append(QCoreApplication::translate("gix", "Cannot create temporary project (5)"));
		return false;
	}

	if (!pf->writeSourceTemplate(prj, prj_file, ProjectFileType::Source)) {
		info.append(QCoreApplication::translate("gix", "Cannot create temporary project (6)"));
		return false;
	}

	delete ppj;
	ppj = new ProjectCollection();

	if (!ppj->load(nullptr, prj_path)) {
		info.append(QCoreApplication::translate("gix", "Cannot create temporary project (7)"));
		return false;
	}

	if (!this->getTargetPlatforms().size()) {
		info.append(QCoreApplication::translate("gix", "No platforms found"));
		return false;
	}

	bool res = true;


	for (QString platform_id : this->getTargetPlatforms().keys()) {
		QScopedPointer<CompilerConfiguration> compiler_cfg(CompilerConfiguration::get(this, platform_id));
		if (compiler_cfg.isNull()) {
			info.append(QCoreApplication::translate("gix", "Cannot create compiler instance"));
			return false;
		}

		int build_res = 0;
		QList<QPair<QString, QString>> tl;

		QVariantMap props;
		props.insert("sys.objext", SysUtils::isWindows() && compiler_cfg->isVsBased ? ".obj" : ".o");
		props.insert("sys.dllext", SysUtils::isWindows() ? ".dll" : ".so");
		props.insert("sys.exeext", SysUtils::isWindows() ? ".exe" : "");
		props.insert("prj.build_dir", "${prj.basedir}/bin/${configuration}/${platform}");
		props.insert("prj.output_path", "${output_path}");
		props.insert("configuration", "test-compiler-config");
		props.insert("platform", platform_id);
		props.insert("__compiler_test_cfg", QVariant::fromValue<void *>(this));

		QScopedPointer<BuildTarget> build_target(ppj->getBuildTarget(props, nullptr));
		if (!build_target) {
			QString err_msg = QString("Invalid build target");
			info.append(err_msg);
			return false;
		}

		//this->logMessage(GIX_CONSOLE_LOG, build_target->toString(), QLogger::LogLevel::Trace);

		BuildDriver builder;
		//connect(&builder, &BuildDriver::log_output, output_window, &OutputWindow::print);
		//connect(&builder, &BuildDriver::log_clear, output_window, &OutputWindow::clearAll);

		builder.setBuildEnvironment(props);

		builder.execute(build_target.data(), BuildOperation::Build);

		build_res += builder.getBuildResult().getStatus();

		res &= (builder.getBuildResult().getStatus() == 0);

#if _DEBUG
		QString build_log_file = PathUtils::combine(QDir::tempPath(), SysUtils::randomString(8) + "_" + platform_id + "_build.log");
		SysUtils::FileWriteAllLines(build_log_file, builder.getBuildResult().buildlog());
#endif

		if (!res) {
			info.append("Cannot build test program<br />Build log:</br><pre>" + builder.getBuildResult().buildlog().join("<br />"));
			return false;
		}
	}

	info.clear();

	QString pid = this->getTargetPlatforms().keys().at(0);
	QScopedPointer<CompilerConfiguration> info_cfg(CompilerConfiguration::get(this, pid));
	if (!info_cfg.data()->getInfo(info)) {
		info.append(QCoreApplication::translate("gix", "Cannot get compiler info"));
		return false;
	}

	return res;
}

CompilerPlatformDefinition *CompilerDefinition::getPlatform(QString platform_id)
{
	return target_platforms[platform_id];
}

QString findIncludeDir(const QString &b)
{
	QString s("libcob.h");
    return PathUtils::findFile(b, s);
}

QString findCopyDir(const QString &b)
{
	QString s("screenio.cpy");
    return PathUtils::findFile(b, s);
}

QString findConfigDir(const QString &b)
{
	QString s("default.conf");
    return PathUtils::findFile(b, s);
}

bool CompilerDefinition::testConfiguration(QString home_dir, QStringList &info, CompilerDefinition **cdef)
{
	QString p1_type;
	CompilerDefinition *test_cfg = new CompilerDefinition();
	test_cfg->setHomedir(home_dir);

	CompilerManager cmgr;
	for (auto cdef : cmgr.getCompilers().values()) {
		if (cdef->getHomedir() == home_dir) {
			info.append(QCoreApplication::translate("gix", "This compiler install is already registered"));
			return false;
		}
	}

	QString include_dir = findIncludeDir(home_dir);
	if (include_dir.isEmpty()) {
		info.append(QCoreApplication::translate("gix", "The \"include\" directory for this compiler install could not be located"));
		return false;
	}

	QString copy_dir = findCopyDir(home_dir);
	if (include_dir.isEmpty()) {
		info.append(QCoreApplication::translate("gix", "The \"copy\" directory for this compiler install could not be located"));
		return false;
	}

	QString config_dir = findConfigDir(home_dir);
    if (config_dir.isEmpty()) {
		info.append(QCoreApplication::translate("gix", "The \"config\" directory for this compiler install could not be located"));
		return false;
	}

	// we look for two possible platforms: x86 and x64

	CompilerPlatformDefinition *p1 = nullptr;
	CompilerPlatformDefinition *p2 = nullptr;

	// Multi-platform compiler (VS-based compilers have two bin/lib directories : either bin/bin_x64 or bin_x86/bin/x64
	if ((QDir(PathUtils::combine(home_dir, "bin")).exists() && QDir(PathUtils::combine(home_dir, "bin_x64")).exists()) ||
		(QDir(PathUtils::combine(home_dir, "bin_x86")).exists() && QDir(PathUtils::combine(home_dir, "bin_x64")).exists())) {
		// we have two platforms;

		QString p1_suffix = QDir(PathUtils::combine(home_dir, "bin_x86")).exists() ? "_x86" : "";
		QString p2_suffix = "_x64";

		p1 = new CompilerPlatformDefinition();
		p1->setBinDirPath(PathUtils::combine(home_dir, "bin" + p1_suffix));
		p1->setLibDirPath(PathUtils::combine(home_dir, "lib" + p1_suffix));
		p1->setIncludeDirPath(include_dir);
		p1->setConfigDirPath(config_dir);
		p1->setCopyDirPath(copy_dir);

		p2 = new CompilerPlatformDefinition();
		p2->setBinDirPath(PathUtils::combine(home_dir, "bin" + p2_suffix));
		p2->setLibDirPath(PathUtils::combine(home_dir, "lib" + p2_suffix));
		p2->setIncludeDirPath(include_dir);
		p2->setConfigDirPath(config_dir);
		p2->setCopyDirPath(copy_dir);
	}
	else {
		// Single platform, we expect at least a bin directory
		if (!QDir(PathUtils::combine(home_dir, "bin")).exists()) {
			info.append(QCoreApplication::translate("gix", "The \"bin\" directory for this compiler install could not be located"));
			return false;
		}

		if (!QDir(PathUtils::combine(home_dir, "lib")).exists()) {
			info.append(QCoreApplication::translate("gix", "The \"bin\" directory for this compiler install could not be located"));
			return false;
		}

		// Beware: p1 here could be either x86 or x64, it should correspond to 
		// the host platform (at least until we validate cross-platform compilation, including x64->x86)
		p1 = new CompilerPlatformDefinition();
		p1->setBinDirPath(PathUtils::combine(home_dir, "bin"));
		p1->setLibDirPath(PathUtils::combine(home_dir, "lib"));
		p1->setIncludeDirPath(include_dir);
		p1->setConfigDirPath(config_dir);
		p1->setCopyDirPath(copy_dir);
	}

	if (!p1 && !p2) {
		info.append(QCoreApplication::translate("gix", "A valid platform for this compiler install could not be located"));
		return false;
	}

	bool platform_validation = true;

	if (p1) {
		if (!QFile::exists(PathUtils::combine(p1->getBinDirPath(), COBC_EXE_NAME))) {
			info.append(QString(QCoreApplication::translate("gix", "The compiler executable \"%1\" for this compiler install could not be located in %2")).arg(COBC_EXE_NAME).arg(p1->getBinDirPath()));
			delete p1;
			platform_validation = false;
		}
	}

	if (p2) {
		if (!QFile::exists(PathUtils::combine(p2->getBinDirPath(), COBC_EXE_NAME))) {
			info.append(QString(QCoreApplication::translate("gix", "The compiler executable \"%1\" for this compiler install could not be located")).arg(COBC_EXE_NAME).arg(p2->getBinDirPath()));
			delete p2;
			platform_validation = false;
		}
	}

	if (!platform_validation)
		return false;


	// if we have only one platform, we have to check whether we are x86 or x64
	if (p1 && !p2) {
		QString exe_path = PathUtils::combine(p1->getBinDirPath(), COBC_EXE_NAME);

#if _WIN32
		DWORD res = 0;
		if (!GetBinaryType(exe_path.toLocal8Bit().data(), &res)) {
			info.append(QCoreApplication::translate("gix", "Cannot determine platform for this compiler install"));
			return false;
		}

		QString platform_id = (res == SCS_64BIT_BINARY) ? "x64" : "x86";

		// We assume single-platform compiler to be MinGW-based
		test_cfg->setIsVsBased(false);
		test_cfg->setId("*");
		test_cfg->setName("*");
		test_cfg->setHostPlatform(SysUtils::getGixBuildPlatform());

		test_cfg->addPlatform(platform_id, p1);

#elif defined(__linux__)
        // For Linux, we assume build/host platform == compiler platform (for now)
        QString platform_id = SysUtils::getGixBuildPlatform();

        test_cfg->setIsVsBased(false);
        test_cfg->setId("*");
        test_cfg->setName("*");
        test_cfg->setHostPlatform(SysUtils::getGixBuildPlatform());

        test_cfg->addPlatform(platform_id, p1);

#elif defined(__APPLE__)

#endif
	}
	else {
		// if we only have one platform, it's always p1. If we are here, p1 (x86) and p2 (x64) are both valid
		// and we assume it's a VS-based compiler, and report x64 as host platform
		test_cfg->setIsVsBased(true);
		test_cfg->setId("*");
		test_cfg->setName("*");
		test_cfg->setHostPlatform("x64");

		test_cfg->addPlatform("x86", p1);
		test_cfg->addPlatform("x64", p2);
	}

	bool res = test_cfg->testConfiguration(info);
	if (res) {
		*cdef = test_cfg;
	}

	return res;
}
