#include "BuildActionMakeListingHandler.h"
#include "BuildDriver.h"
#include "BuildConsts.h"
#include "linq/linq.hpp"
#include <PathUtils.h>

BuildActionMakeListingHandler::BuildActionMakeListingHandler()
{}

BuildActionMakeListingHandler::~BuildActionMakeListingHandler()
{}

bool BuildActionMakeListingHandler::startBuild()
{
	QString configuration = this->environment.value("configuration").toString();
	QString platform = this->environment.value("platform").toString();

	importProjectEnvironment();

	QScopedPointer<CompilerConfiguration> ccfg(CompilerConfiguration::get(configuration, platform));
	CompilerConfiguration *compiler_cfg = ccfg.data();
	if (compiler_cfg == nullptr) {
		return false;
	}
	
	QString build_dir = getBuildDirectory();

	bool is_esql_prj = this->environment.value("preprocess_esql").toBool();

	QString src_file = target->getDependency(is_esql_prj ? BuildConsts::TYPE_CBSQL : BuildConsts::TYPE_COBOL)->filename();


	//QMap<QString, QVariant> build_env;
	//build_env["configuration"] = configuration;
	//build_env["platform"] = platform;
	//build_d.setBuildEnvironment(build_env);

	QProcessEnvironment env = compiler_cfg->getEnvironment(build_driver);

	QStringList cobc_opts;
	QString cobc = compiler_cfg->executablePath;

	if (environment.contains("compiler_dialect")) {
		cobc_opts.append("-std");
		cobc_opts.append(environment.value("compiler_dialect").toString());
	}

	QStringList copy_dirs = (build_driver->getCopyResolver()->getCopyDirs());
	if (copy_dirs.size() > 0) {
		for (QString copy_dir : copy_dirs) {
			cobc_opts.append("-I");
			cobc_opts.append(copy_dir);
		}
	}

	auto ekeys = QStringList(environment.keys());
	auto comp_opts = cpplinq::from(ekeys).where([](QString key) { return key.startsWith("cobc_");  }).to_vector();
	for (QString opt : comp_opts) {
		QString v = environment.value(opt).toString();
		if (!v.isEmpty())
			cobc_opts.append(v);
	}

	cobc_opts.append("-t");
	cobc_opts.append(target->filename());
	cobc_opts.append("--tlines");
	cobc_opts.append("0");
	cobc_opts.append("--tsymbols");
	cobc_opts.append("-X");
	cobc_opts.append("-P");

	cobc_opts.append("-C");

	cobc_opts.append(src_file);

	QProcess p;

	p.setTextModeEnabled(false);

	p.setProgram(cobc);

	p.setArguments(cobc_opts);
	p.setProcessEnvironment(env);

	p.setWorkingDirectory(build_dir);

	p.start();
	if (!p.waitForStarted()) {
		build_driver->log_build_message("ERROR: " + p.errorString(), QLogger::LogLevel::Error);
		return false;
	}

	if (!p.waitForFinished()) {
		build_driver->log_build_message("ERROR: " + p.errorString(), QLogger::LogLevel::Error);
		return false;
	}

	return QFile(target->filename()).exists();
}