#include "BuildActionCompileHandler.h"

#include <QEventLoop>

#include "ProjectFile.h"
#include "PathUtils.h"
#include "SysUtils.h"
#include "BuildConsts.h"
#include "PropertyConsts.h"
#include "MacroManager.h"
#include "linq/linq.hpp"

using namespace cpplinq;

BuildActionCompileHandler::BuildActionCompileHandler()
{}


BuildActionCompileHandler::~BuildActionCompileHandler()
{}

bool BuildActionCompileHandler::startBuild()
{
	QString target_final_path;

	importProjectEnvironment();
	importFileEnvironment();

	QString build_dir = getBuildDirectory();
	QDir(".").mkpath(build_dir);

	auto compilable_deps = from(*target->dependencies()).where([](BuildTarget *a) {
		return (a->providesOneOf({ BuildConsts::TYPE_COBOL, BuildConsts::TYPE_CBSQL, BuildConsts::TYPE_CBCICS }));
	}).to_vector();

	if (compilable_deps.size() != 1)
		return false;

	QString input_file = compilable_deps.at(0)->filename();
	QString input_file_dir = PathUtils::getDirectory(input_file);

	build_driver->log_build_message(tr("Building") + input_file, QLogger::LogLevel::Info);

	QString build_configuration = build_driver->getBuildEnvironment()["configuration"].toString();
	QString target_platform = build_driver->getBuildEnvironment()["platform"].toString();
	QString target_type = build_configuration + "/" + target_platform;
	bool is_web_project = environment["__project_type_id"].toInt() == (int)ProjectType::Web;

	QScopedPointer<CompilerConfiguration> ccfg(CompilerConfiguration::get(build_configuration, target_platform));
	CompilerConfiguration *compiler_cfg = ccfg.data();
	if (compiler_cfg == nullptr) {
		build_driver->log_build_message(QString(tr("Invalid compiler configuration for target ")).arg(target_type), QLogger::LogLevel::Error, 1);
		return false;
	}

	QProcessEnvironment env = compiler_cfg->getEnvironment(build_driver);

	QStringList cobc_opts;
	QString cobc = compiler_cfg->executablePath;
	build_driver->log_build_message(QString(tr("Using compiler %1")).arg(cobc), QLogger::LogLevel::Trace);

	if (environment.contains("compiler_dialect")) {
		cobc_opts.append("-std");
		cobc_opts.append(environment["compiler_dialect"].toString());
	}

	if (build_configuration == "debug") {
		cobc_opts.append("-g");
		cobc_opts.append("-debug");
		cobc_opts.append("-O0");
		cobc_opts.append("--fgen-c-line-directives");
		cobc_opts.append("--fgen-c-labels");

		if (compiler_cfg->isVsBased) {
			cobc_opts.append("-A");
			cobc_opts.append("/DEBUG:FULL");
		}
		else {
			cobc_opts.append("-A");
			cobc_opts.append("-O0");
		}
	}

	MacroManager mm(environment);

	QStringList copy_dirs = retrieve_copy_dirs();
	if (copy_dirs.size() > 0) {
		for (QString copy_dir : copy_dirs) {
			copy_dir = mm.translate(copy_dir);
			cobc_opts.append("-I");
			// VS/GC Bug(?) if include path is passed without a trailing slash
			QString cd = copy_dir;
			
			if (compiler_cfg->isVsBased && !copy_dir.endsWith("/"))
				cd += "/";

			cobc_opts.append(cd); 
		}
	}

	if (!QDir::isAbsolutePath(target->filename()))
		target_final_path = PathUtils::combine(build_dir, target->filename());
	else
		target_final_path = target->filename();

	QString ext = SysUtils::isWindows() && compiler_cfg->isVsBased ? ".obj" : ".o";
	target_final_path = PathUtils::changeExtension(target_final_path, ext);

	if (build_configuration != "debug" && is_web_project) {
		cobc_opts.append("-save-temps");
	}

	auto ekeys = QStringList(environment.keys());
	auto comp_opts = from(ekeys).where([](QString key) { return key.startsWith("cobc_");  }).to_vector();
	for (QString opt : comp_opts) {
		QString v = environment.value(opt).toString();
		if (!v.isEmpty())
			cobc_opts.append(v);
	}

	ProjectType prj_type = (ProjectType)environment["__project_type_id"].toInt();
	switch (prj_type) {
		case ProjectType::SingleBinary:
			if (environment.value(PropertyConsts::IsStartupItem, false).toBool())
				cobc_opts.append("-x");
			break;

		case ProjectType::MultipleBinaries:
			QString default_build_type = environment.value("build_type").toString();
			QString file_build_type = environment.value(PropertyConsts::CustomBuildType, default_build_type).toString();
			if (file_build_type == BuildConsts::MODULE_EXECUTABLE)
				cobc_opts.append("-x");
			break;
	}

	//cobc_opts.append("-v");

	cobc_opts.append("-c");

	cobc_opts.append("-o");
	cobc_opts.append(target_final_path);

	cobc_opts.append(input_file);

	build_driver->log_build_message(cobc + " " + cobc_opts.join(" "), QLogger::LogLevel::Info);

	QProcess p;

	p.setTextModeEnabled(false);

	p.setProgram(cobc);

	p.setArguments(cobc_opts);
	p.setProcessEnvironment(env);
	p.setWorkingDirectory(build_dir);

	//dump_environment(env, build_driver);

	QEventLoop loop;
	bool ps_err = false;
	connect(&p, &QProcess::readyReadStandardError, this, [this, &p] { readStdErr(&p); });
	connect(&p, &QProcess::readyReadStandardOutput, this, [this, &p] { readStdOut(&p); });
	connect(&p, &QProcess::errorOccurred, this, [this, &p, ps_err](QProcess::ProcessError _err) mutable { 
		build_driver->log_build_message(p.errorString(), QLogger::LogLevel::Info); ps_err = true; 
	});

	connect(&p, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),
		[this, &loop, &p](int exitCode, QProcess::ExitStatus exitStatus) { readStdErr(&p); readStdOut(&p); loop.quit(); });

	p.start();
	if (!p.waitForStarted()) {
		build_driver->log_build_message("ERROR: " + p.errorString(), QLogger::LogLevel::Error);
		return false;
	}

	loop.exec();

	int rc = p.exitCode();
	bool res = ((!rc) && (!ps_err));
	if (res) {
		build_driver->log_build_message(tr("Build successful") + ": " + target_final_path, QLogger::LogLevel::Success);
	}
	else {
		build_driver->log_build_message(tr("Build error"), QLogger::LogLevel::Error);
		build_driver->log_build_message("Exit code: " + QString::number(rc), QLogger::LogLevel::Trace);
		return false;
	}

	return true;
}

QStringList BuildActionCompileHandler::retrieve_copy_dirs()
{
	QStringList pv = environment["copy_include_path"].toStringList();

	if (environment.contains("preprocess_esql") && environment["preprocess_esql"].toBool()) {
		QString copy_dir = SysUtils::getSysCopyDir();
		if (!copy_dir.isEmpty() && !pv.contains(copy_dir))
			pv.append(copy_dir);
	}

	return pv;

}
