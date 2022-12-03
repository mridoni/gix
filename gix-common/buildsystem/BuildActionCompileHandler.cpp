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

#include "BuildActionCompileHandler.h"

#include <QEventLoop>
#include <QSet>

#include "ProjectFile.h"
#include "PathUtils.h"
#include "SysUtils.h"
#include "BuildConsts.h"
#include "GixGlobals.h"
#include "PropertyConsts.h"
#include "MacroManager.h"
#include "ESQLConfiguration.h"
#include "linq/linq.hpp"

using namespace cpplinq;

BuildActionCompileHandler::BuildActionCompileHandler()
{}


BuildActionCompileHandler::~BuildActionCompileHandler()
{}

bool BuildActionCompileHandler::startBuild()
{
	QSettings settings;
	QString target_final_path;

	QSet<QString> cobc_special_ops({ "cobc_source_format" });	// cobc_* options that will be handled separately

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

	build_driver->log_build_message(tr("Building") + input_file, spdlog::level::info);

	QString build_configuration = build_driver->getBuildEnvironment()["configuration"].toString();
	QString target_platform = build_driver->getBuildEnvironment()["platform"].toString();
	QString target_type = build_configuration + "/" + target_platform;
	bool is_web_project = environment["__project_type_id"].toInt() == (int)ProjectType::Web;

	QScopedPointer<CompilerConfiguration> ccfg(CompilerConfiguration::get(build_configuration, target_platform, environment));
	CompilerConfiguration *compiler_cfg = ccfg.data();
	if (compiler_cfg == nullptr) {
		build_driver->log_build_message(QString(tr("Invalid compiler configuration for target ")).arg(target_type), spdlog::level::err, 1);
		return false;
	}

	QString esql_cfg_id = settings.value("esql_preprocessor_id", ESQLConfigurationType::GixInternal).toString();
	CompilerEnvironment esql_cfg_env = compiler_cfg->getCompilerEnvironment();
	QScopedPointer<ESQLConfiguration> esql_cfg(ESQLConfiguration::get(esql_cfg_id, esql_cfg_env, build_configuration, target_platform));
	if (esql_cfg.isNull()) {
		build_driver->log_build_message(QString(tr("Invalid ESQL precompiler configuration for target ")).arg(target_type), spdlog::level::err, 1);
		return false;
	}

	QProcessEnvironment env = compiler_cfg->getEnvironment(build_driver);

	QStringList cobc_opts;
	QString cobc = compiler_cfg->executablePath;
	build_driver->log_build_message(QString(tr("Using compiler %1")).arg(cobc), spdlog::level::trace);

	if (environment.contains("compiler_dialect")) {
		cobc_opts.append("-std");
		cobc_opts.append(environment["compiler_dialect"].toString());
	}

	if (environment.contains("cobc_source_format")) {
		QString src_format = environment["cobc_source_format"].toString();
		if (src_format.isEmpty() || (src_format != "free" && src_format != "fixed"))
			src_format = settings.value("cobc_default_source_format").toString();

		if (src_format == "fixed")
			cobc_opts.append("-fixed");

		if (src_format == "free")
			cobc_opts.append("-free");
	}

	if (build_configuration == "debug") {
		cobc_opts.append("-g");
		cobc_opts.append("-debug");

        if (compiler_cfg->isVersionGreaterThanOrEqualTo(3, 1, 0)) {
            cobc_opts.append("-O0");
            cobc_opts.append("--fgen-c-line-directives");
            cobc_opts.append("--fgen-c-labels");
        }

		if (compiler_cfg->isVsBased) {
			cobc_opts.append("-A");
			cobc_opts.append("/DEBUG:FULL");
		}
		else {
			cobc_opts.append("-A");
			cobc_opts.append("-O0 -gdwarf-4");
		}
	}

#if _DEBUG
	//cobc_opts.append("-v");
#endif

	MacroManager mm(environment);

	QStringList copy_dirs = retrieve_copy_dirs(esql_cfg.get());
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
	auto comp_opts = from(ekeys).where([cobc_special_ops](QString key) { return key.startsWith("cobc_") && !cobc_special_ops.contains(key);  }).to_vector();
	for (QString opt : comp_opts) {
		QString v = environment.value(opt).toString();
		if (!v.isEmpty())
			cobc_opts.append(v);
	}

	ProjectType prj_type = (ProjectType)environment["__project_type_id"].toInt();
	QString default_build_type = environment.value("build_type").toString();
	QString file_build_type = environment.value(PropertyConsts::CustomBuildType, default_build_type).toString();
	switch (prj_type) {	// TODO: check the semantics of this operation, for the moment they are one and the same
		case ProjectType::SingleBinary:
			if (file_build_type == BuildConsts::MODULE_EXECUTABLE)
				cobc_opts.append("-x");
			break;

		case ProjectType::MultipleBinaries:
			if (file_build_type == BuildConsts::MODULE_EXECUTABLE)
				cobc_opts.append("-x");
			break;
	}

	//cobc_opts.append("-v");

	cobc_opts.append("-c");

	cobc_opts.append("-o");
	cobc_opts.append(target_final_path);

	cobc_opts.append(input_file);

	build_driver->log_build_message(cobc + " " + cobc_opts.join(" "), spdlog::level::info);

	QProcess p;

	p.setTextModeEnabled(false);

	p.setProgram(cobc);
    
    if (environment["cobc_c_comp_opts"].isValid() && !environment["cobc_c_comp_opts"].toString().isEmpty()) {
		cobc_opts.append(environment["cobc_c_comp_opts"].toString());
	} 

	p.setArguments(cobc_opts);
	p.setProcessEnvironment(env);
	p.setWorkingDirectory(build_dir);

	//dump_environment(env, build_driver);

	QEventLoop loop;
	bool ps_err = false;
	connect(&p, &QProcess::readyReadStandardError, this, [this, &p] { readStdErr(&p); });
	connect(&p, &QProcess::readyReadStandardOutput, this, [this, &p] { readStdOut(&p); });
	connect(&p, &QProcess::errorOccurred, this, [this, &p, ps_err](QProcess::ProcessError _err) mutable { 
		build_driver->log_build_message(p.errorString(), spdlog::level::info); ps_err = true; 
	});

	connect(&p, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),
		[this, &loop, &p](int exitCode, QProcess::ExitStatus exitStatus) { readStdErr(&p); readStdOut(&p); loop.quit(); });

	p.start();
	if (!p.waitForStarted()) {
		build_driver->log_build_message("ERROR: " + p.errorString(), spdlog::level::err);
		return false;
	}

	loop.exec();

	int rc = p.exitCode();
	bool res = ((!rc) && (!ps_err));
	if (res) {
		build_driver->log_build_message(tr("Build successful") + ": " + target_final_path, spdlog::level::info);
	}
	else {
		build_driver->log_build_message(tr("Build error"), spdlog::level::err);
		build_driver->log_build_message("Exit code: " + QString::number(rc), spdlog::level::trace);
		return false;
	}

	return true;
}

QStringList BuildActionCompileHandler::retrieve_copy_dirs(ESQLConfiguration *esql_cfg)
{
	QStringList pv = environment["copy_include_path"].toStringList();

	// TODO: user libs

	if (environment.value("preprocess_esql", false).toBool()) {
		pv.append(esql_cfg->getCopyPathList());
	}

	return pv;
}
