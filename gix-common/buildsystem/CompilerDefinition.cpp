#include "CompilerDefinition.h"

//#include "IdeTaskManager.h"

#include <QDomDocument>
#include <QFile>
#include <QVariant>
#include <QXmlQuery>
#include <PathUtils.h>
#include <QBuffer>
#include <QtCore>

#include "IGixLogManager.h"
#include "GixGlobals.h"


CompilerDefinition::CompilerDefinition()
{
	is_vs_based = false;
	supports_anim = false;
}

CompilerDefinition::~CompilerDefinition()
{
	if (target_platforms.size() == 0)
		return;

	QMap<QString, CompilerPlatformDefinition*>::iterator i;
	for (i = target_platforms.begin(); i != target_platforms.end(); ++i) {
		delete i.value();
	}

	target_platforms.clear();
}

CompilerPlatformDefinition* CompilerDefinition::parsePlatform(QDomNode& xn, QString homedir)
{
	IGixLogManager *logger = GixGlobals::getLogManager();

	CompilerPlatformDefinition* cpd = new CompilerPlatformDefinition();
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

CompilerDefinition* CompilerDefinition::load(QString def_path)
{
	IGixLogManager *logger = GixGlobals::getLogManager();
	CompilerDefinition* def = new CompilerDefinition();
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
	def->supports_anim = QVariant(root.firstChildElement("supports_anim").text()).toBool();

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

				CompilerPlatformDefinition* cpd = parsePlatform(xn, homedir);
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

CompilerPlatformDefinition* CompilerDefinition::getPlatform(QString platform_id)
{
	return target_platforms[platform_id];
}
