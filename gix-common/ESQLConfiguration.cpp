#include "ESQLConfiguration.h"
#include "GixPreProcessor.h"
#include "TPESQLProcessing.h"
#include "BuildDriver.h"
#include "PathUtils.h"
#include "GixGlobals.h"
#include "SysUtils.h"
#include "ErrorWarningFilter.h"

#include <QFile>
#include <QSettings>

ESQLConfiguration *ESQLConfiguration::get(QString id, CompilerEnvironment e, QString c, QString p)
{
	ESQLConfiguration *cfg = new ESQLConfiguration();
	cfg->id = id;
	cfg->env = e;
	cfg->configuration = c;
	cfg->platform = p;

	if (id == ESQLConfigurationType::GixInternal)
		return cfg;
	else {
		if (id == ESQLConfigurationType::GixExternal) {
			// fill in the fields
			/*
			Usage: gixpp [options]

			Options:
			  -h, --help                                              Displays help on
																	  commandline options.
			  -I <copypath>                                           COPY file path list
			  -i <infile>                                             input file
			  -o <outfile>                                            output file
			  -s <symfile>                                            output symbol file
			  -e, --esql                                              preprocess for ESQL
																	  (single file mode
																	  takes precedence)
			  -p, --esql-preprocess-copy                              ESQL: preprocess copy
																	  files outside EXEC SQL
																	  INCLUDE statements
			  -E <ESQL: copy files extension list (comma-separated)>  esql-copy-exts
			  -a, --esql-anon-params                                  ESQL: use anonymous
																	  (not numbered)
																	  parameters
			  -S, --esql-static-calls                                 ESQL: emit static
																	  calls
			  -g, --debug-info                                        generate debug info
			  -c                                                      consolidate source to
																	  single-file (CP)
			  -k                                                      keep temporary files
			  -v                                                      Verbose
			  -d                                                      Verbose (debug)
			*/
			cfg->bin_path = GixGlobals::getGixBinDir();
			cfg->pp_exe = SysUtils::isWindows() ? "gixpp.exe" : "gixpp";
			cfg->pp_cmdline = "-i ${input_file} -o ${output_file} -I ${copy_path_list} -e -E \"${copy_ext_list}\" -g";
			return cfg;
		}
		else {
			// fill in the fields from settings
			return cfg;
		}
	}

	return cfg;
}

bool ESQLConfiguration::run(BuildDriver *build_driver, QString input_file, QString output_file, QMap<QString, QVariant> opts)
{
	if (id == ESQLConfigurationType::GixInternal)
		return runGixSqlInternal(build_driver, input_file, output_file, opts);

	QString target_type = configuration + "/" + platform;
	QScopedPointer<CompilerConfiguration> ccfg(CompilerConfiguration::get(configuration, platform, QVariantMap()));
	CompilerConfiguration *compiler_cfg = ccfg.data();
	if (compiler_cfg == nullptr) {
		build_driver->log_build_message(QString(QCoreApplication::translate("gix", "Invalid compiler configuration for target %1")).arg(target_type), spdlog::level::err, 1);
		return false;
	}

	QProcessEnvironment env = compiler_cfg->getEnvironment(build_driver);

	MacroManager mm(opts);

	mm.add("input_file", input_file);
	mm.add("output_file", output_file);

	QStringList copy_path_list = getCopyPathList();
	copy_path_list.append(opts.value("copy_include_path", QStringList()).toStringList());
	mm.add("copy_path_list", copy_path_list.join(QDir::listSeparator()));

	QString cmd_line = mm.translate(pp_cmdline);

	QStringList esql_opts = cmd_line.split(' ');

	if (id == ESQLConfigurationType::GixExternal) {

		if (opts.value("esql_preprocess_copy_files", false).toBool())
			esql_opts.append("-p");

		if (opts.value("esql_anon_params", false).toBool())
			esql_opts.append("-a");

		if (opts.value("esql_static_calls", false).toBool())
			esql_opts.append("-S");
	}

	for (auto it = pp_env.begin(); it != pp_env.end(); ++it) {
		SysUtils::mergeEnvironmentVariable(env, it.key(), it.value());
	}

	build_driver->log_build_message(QString(QCoreApplication::translate("gix", "ESQL: running %1")).arg(cmd_line), spdlog::level::info);

	QProcess p;

	p.setTextModeEnabled(false);

	p.setProgram(PathUtils::combine(bin_path, pp_exe));

	p.setArguments(esql_opts);
	p.setProcessEnvironment(env);
	p.setWorkingDirectory(PathUtils::getDirectory(input_file));

	//dump_environment(env, build_driver);

	QEventLoop loop;
	bool ps_err = false;
	connect(&p, &QProcess::readyReadStandardError, this, [this, &p, build_driver] { readStdErr(build_driver, &p); });
	connect(&p, &QProcess::readyReadStandardOutput, this, [this, &p, build_driver] { readStdOut(build_driver, &p); });
	connect(&p, &QProcess::errorOccurred, this, [this, &p, ps_err, build_driver](QProcess::ProcessError _err) mutable {
		build_driver->log_build_message(p.errorString(), spdlog::level::info); ps_err = true;
	});

	connect(&p, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),
		[this, &loop, &p, build_driver](int exitCode, QProcess::ExitStatus exitStatus) { readStdErr(build_driver, &p); readStdOut(build_driver, &p); loop.quit(); });

	p.start();
	if (!p.waitForStarted()) {
		build_driver->log_build_message("ERROR: " + p.errorString(), spdlog::level::err);
		return false;
	}

	loop.exec();

	int rc = p.exitCode();
	bool res = ((!rc) && (!ps_err) && QFile::exists(output_file));
	if (res) {
		build_driver->log_build_message(tr("ESQL build successful") + ": " + output_file, spdlog::level::info);
	}
	else {
		build_driver->log_build_message(tr("ESQL build error"), spdlog::level::err);
		build_driver->log_build_message("Exit code: " + QString::number(rc), spdlog::level::trace);
		return false;
	}

	return true;
}

QString ESQLConfiguration::getBinPath()
{
	return bin_path;
}

QString ESQLConfiguration::getExe()
{
	return pp_exe;
}

QString ESQLConfiguration::getCmdLine()
{
	return QString();
}

QMap<QString, QString> ESQLConfiguration::getEnvironment(QString esql_driver_type)
{
	return QMap<QString, QString>();
}

QStringList ESQLConfiguration::getCopyPathList()
{
	if (id == ESQLConfigurationType::GixInternal || id == ESQLConfigurationType::GixExternal) {
		return (QStringList() << GixGlobals::getGixCopyDir());
	}
	return QStringList();
}

QStringList ESQLConfiguration::getLinkLibPathList()
{
	if (id == ESQLConfigurationType::GixInternal || id == ESQLConfigurationType::GixExternal) {
		QString gix_rt_lib_dir = GixGlobals::getGixRuntimeLibDir(env, platform);
		return (QStringList() << gix_rt_lib_dir);
	}

	return QStringList();
}

QStringList ESQLConfiguration::getLinkLibNameList()
{
	if (id == ESQLConfigurationType::GixInternal || id == ESQLConfigurationType::GixExternal) {
		QStringList res = QStringList() << (env == CompilerEnvironment::VisualStudio ? "libgixsql" : "gixsql");
		return res;
	}
	return QStringList();
}

QStringList ESQLConfiguration::getRuntimeLibPathList(QString driver_type)
{
	QSettings settings;
	QStringList res;

	if (id == ESQLConfigurationType::GixInternal || id == ESQLConfigurationType::GixExternal) {

		QString driver_path = GixGlobals::getGixRuntimeLibDir(this->env, this->platform);
		if (!driver_path.isEmpty())
			res.append(driver_path);
	}
	return res;
}

bool ESQLConfiguration::isVerbose()
{
	return verbose;
}

bool ESQLConfiguration::isVerboseDebug()
{
	return verbose_debug;
}

void ESQLConfiguration::setVerbose(bool b)
{
	verbose = b;
}

void ESQLConfiguration::setVerboseDebug(bool b)
{
	verbose_debug = b;
}

struct AnyGet
{
	std::string operator()(bool value) { return value ? "true" : "false"; }
	std::string operator()(char value) { return std::string(1, value); }
	std::string operator()(int value) { return std::to_string(value); }
	std::string operator()(double value) { return std::to_string(value); }
	std::string operator()(const std::string &value) { return value; }
};

static std::string variant_to_string(const variant &input)
{
	return std::visit(AnyGet{}, input);
}

bool ESQLConfiguration::runGixSqlInternal(BuildDriver *build_driver, QString input_file, QString output_file, QMap<QString, QVariant> opts)
{
	GixPreProcessor gp;
	bool opt_esql_preprocess_copy_files = opts["esql_preprocess_copy_files"].toBool();
	bool opt_anonymous_params = opts["esql_anon_params"].toBool();

	gp.verbose = isVerbose();
	gp.verbose_debug = isVerboseDebug();

	CopyResolver cr(*build_driver->getCopyResolver());
	cr.addCopyDir(GixGlobals::getGixCopyDir().toStdString());

	gp.setCopyResolver(&cr);

	gp.setOpt("emit_debug_info", true);
	gp.setOpt("emit_static_calls", true);

	gp.setOpt("anonymous_params", opt_anonymous_params);
	gp.setOpt("consolidated_map", true);	// we need this to generate a map against the full program listing
	gp.setOpt("preprocess_copy_files", opt_esql_preprocess_copy_files);

	gp.addStep(new TPESQLProcessing(&gp));

	gp.setInputFile(input_file.toStdString());
	gp.setOutputFile(output_file.toStdString());

	GixGlobals::getLogManager()->debug(LOG_BUILD, "ESQL: Input file: {}", input_file);
	GixGlobals::getLogManager()->debug(LOG_BUILD, "ESQL: Output file: {}", output_file);
	for (std::string cd : cr.getCopyDirs()) {
		GixGlobals::getLogManager()->debug(LOG_BUILD, "ESQL: Copy dir: {}", cd);
	}
	for (std::string ce : cr.getExtensions()) {
		GixGlobals::getLogManager()->debug(LOG_BUILD, "ESQL: Copy extension: {}", ce);
	}
	for (auto it = gp.getOpts().begin(); it != gp.getOpts().end(); ++it) {
		QString k = QString::fromStdString(it->first);
		QString v = QString::fromStdString(variant_to_string(it->second));
		GixGlobals::getLogManager()->debug(LOG_BUILD, "ESQL: Option [{}] : [{}]", k, v);
	}

	bool b = gp.process();

	if (!b || !QFile(output_file).exists()) {
		for (std::string m : gp.err_data.err_messages) {
			QString qm = QString::fromStdString(m);
			if (ErrorWarningFilter::isWarning(qm))
				build_driver->log_build_message(qm, spdlog::level::warn);
			else
				build_driver->log_build_message(qm, spdlog::level::err);
		}
		return false;
	}

	return true;

}

void ESQLConfiguration::readStdOut(BuildDriver *build_driver, QProcess *p)
{
	p->setReadChannel(QProcess::ProcessChannel::StandardOutput);
	QByteArray qba = p->readAll();
	QString s(qba);
	if (!s.isEmpty())
		build_driver->log_build_message(s, spdlog::level::info);
}

void ESQLConfiguration::readStdErr(BuildDriver *build_driver, QProcess *p)
{
	p->setReadChannel(QProcess::ProcessChannel::StandardError);
	QByteArray qba = p->readAll();
	QString s(qba);
	if (!s.isEmpty())
		build_driver->log_build_message(s, spdlog::level::err);
}