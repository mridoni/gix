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

You should have received a bfr of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#if defined(_WIN32) && defined(USE_CPP_HTTPLIB)
// This avoids a nasty compilation problem with winsock on Windows
#include "httplib.h"
#endif

#include "DebugManager.h"
#include "CompilerConfiguration.h"
#include "BuildDriver.h"
#include "SysUtils.h"
#include "PathUtils.h"
#include "CobolUtils.h"
#include "UiUtils.h"
#include "Ide.h"
#include "MetadataManager.h"
#include "StdStreamRedirect.h"
#include "OutputWindow.h"
#include "GixDebugger.h"
#include "TPESQLProcessing.h"
#include "ESQLConfiguration.h"
#include "utils.h"
#include "PropertyConsts.h"
#include "linq/linq.hpp"

#if defined(_WIN32) || defined(_WIN64)
#include "windows.h"
#endif

#include <QThread>
#include <QCoreApplication>
#include <QRegularExpression>
#include <QGuiApplication>
#include <QMouseEvent>
#include <QMetaEnum>
#include <ServerConfig.h>

#define GET_VARS_LEN_SIZE	5
#define ASCII_ZERO 0x30

#if defined(unix) || defined(__unix__) || defined(__unix) || defined(__linux__)
#include <byteswap.h>
#define COB_BSWAP_16(val) (bswap_16 (val))
#define COB_BSWAP_32(val) (bswap_32(val))
#define COB_BSWAP_64(val) (bswap_64 (val))
#elif defined(__APPLE__)
#include <libkern/OSByteOrder.h>
#define COB_BSWAP_16(val) (OSSwapInt16(val))
#define COB_BSWAP_32(val) (OSSwapInt32(val))
#define COB_BSWAP_64(val) (OSSwapInt64(val))
#else
#define COB_BSWAP_16(val) (_byteswap_ushort (val))
#define COB_BSWAP_32(val) (_byteswap_ulong (val))
#define COB_BSWAP_64(val) (_byteswap_uint64 (val))
#endif

DebugManager::DebugManager(IdeTaskManager *_ide_task_manager)
{
	debug_driver = nullptr;
	ide_task_manager = _ide_task_manager;
	is_debugging_enabled = true;
	is_user_initiated_stop = false;
	cur_line = 0;
	debugged_prj = nullptr;
}


DebugManager::~DebugManager()
{
	ide_task_manager->logMessage(GIX_CONSOLE_LOG, "Stopping debug driver", QLogger::LogLevel::Debug);
	if (debug_driver) {
		debug_driver->stop();
	}

	if (debug_driver_thread) {
		debug_driver_thread->quit();
		debug_driver_thread->wait();
	}

	for (ModuleDebugInfo *mdi : modules) {
		delete mdi;
	}

	if (!tmp_cfg_path.isEmpty() && QFile::exists(tmp_cfg_path))
		QFile::remove(tmp_cfg_path);

	saveDebugManagerState();
}

void DebugManager::setDebuggingEnabled(bool f)
{
	is_debugging_enabled = f;
}

bool DebugManager::start(Project *prj, QString _build_configuration, QString _target_platform)
{
	QSettings settings;

#if defined (__linux__)
    if (prj->getType() == ProjectType::Web) {
        QString msg = QString(tr("Debugging of Web Projects under Linux is not currently supported"));
        ide_task_manager->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Error);
        UiUtils::ErrorDialog(msg);
        return false;
    }
#endif

	build_configuration = _build_configuration;
	target_platform = _target_platform;

	debugged_prj = prj;

	bool dbg_stop_at_first_line = prj->PropertyGetValue("dbg_stop_at_first_line").toBool();
	bool dbg_separate_console = prj->PropertyGetValue("dbg_separate_console").toBool();

	QString gix_build_platform = SysUtils::getGixBuildPlatform();
	if (is_debugging_enabled && target_platform != gix_build_platform) {
		QString msg = QString(tr("Architecture mismatch. Only projects for the following platforms can be debugged: %1")).arg(gix_build_platform);
		ide_task_manager->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Error);
		UiUtils::ErrorDialog(msg);
		return false;
	}

	CompilerConfiguration *compiler_cfg = CompilerConfiguration::get(build_configuration, target_platform, QVariantMap());
	if (compiler_cfg == nullptr) {
		QString msg(tr("Invalid compiler configuration"));
		ide_task_manager->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Error);
		UiUtils::ErrorDialog(msg);
		return false;
	}

	QMap<QString, QVariant> build_env;
	build_env.insert("configuration", build_configuration);
	build_env.insert("platform", target_platform);
	build_env.insert("sys.objext", SysUtils::isWindows() && compiler_cfg->isVsBased ? ".obj" : ".o");
	build_env.insert("sys.dllext", SysUtils::isWindows() ? ".dll" : ".so");
	build_env.insert("sys.exeext", SysUtils::isWindows() ? ".exe" : "");
	build_env.insert("prj.build_dir", "${prj.basedir}/bin/${configuration}/${platform}");

	for (auto it = prj->getRuntimeProperties().begin(); it != prj->getRuntimeProperties().end(); ++it) {
		build_env.insert(it.key(), it.value());
	}

	for (auto it = prj->PropertyGetCurrentValues()->begin(); it != prj->PropertyGetCurrentValues()->end(); ++it) {
		build_env.insert(it.key(), it.value());
	}

	MacroManager mm(build_env);

#if _DEBUG
	BuildTarget *bt = prj->getBuildTarget(build_env, nullptr);
#else
	QScopedPointer<BuildTarget> bt(prj->getBuildTarget(build_env, nullptr));
#endif
	QString target = bt->filename();
	QString module_name = PathUtils::toModuleName(target);

	QString startup_item_name = prj->getStartupItemName();
	if (!startup_item_name.isEmpty()) {
		module_name = startup_item_name;
	}
	else {
		if (prj->getType() == ProjectType::MultipleBinaries) {
			QString msg = tr("Multi-binaries projects need a startup item to be set in order to be run or debugged");
			ide_task_manager->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Error);
			UiUtils::ErrorDialog(msg);
			return false;
		}
	}

	bool dbg_merge_env = prj->PropertyGetValue("dbg_merge_env").toBool();

	QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
	SysUtils::mergeEnvironmentVariable(env, "PATH", compiler_cfg->binDirPath);
	SysUtils::mergeEnvironmentVariable(env, "PATH", compiler_cfg->libDirPath);
	SysUtils::mergeEnvironmentVariable(env, "PATH", GixGlobals::getGixRuntimeLibDir(compiler_cfg->getCompilerEnvironment(), target_platform), true);

	env.insert("COB_EXIT_WAIT", "Y");

	if (prj->isEsql()) {

		QString esql_cfg_id = settings.value("esql_preprocessor_id", ESQLConfigurationType::GixInternal).toString();
		CompilerEnvironment esql_cfg_env = compiler_cfg->getCompilerEnvironment();
		QScopedPointer<ESQLConfiguration> esql_cfg(ESQLConfiguration::get(esql_cfg_id, esql_cfg_env, build_configuration, target_platform));

		ide_task_manager->logMessage(GIX_CONSOLE_LOG, QString(tr("Using ESQL driver: %1")).arg(esql_cfg_id), QLogger::LogLevel::Trace);

		QString esql_driver_type = "*";	// placeholder
		QMap<QString, QString> esql_env = esql_cfg->getEnvironment("*");

		for (auto it = esql_env.begin(); it != esql_env.end(); ++it) {
			env.insert(it.key(), it.value());
		}

		for (QString v : esql_cfg->getRuntimeLibPathList(esql_driver_type)) {
			ide_task_manager->logMessage(GIX_CONSOLE_LOG, QString(tr("Loading ESQL runtime libraries from %1")).arg(v), QLogger::LogLevel::Debug);
#if defined(_WIN32)
			SysUtils::mergeEnvironmentVariable(env, "PATH", v);
#else
			SysUtils::mergeEnvironmentVariable(env, "LD_LIBRARY_PATH", v);
#endif
		}

		if (esql_cfg_id == ESQLConfigurationType::GixInternal || esql_cfg_id == ESQLConfigurationType::GixExternal) {
			// Not needed anymore
			// ide_task_manager->logMessage(GIX_CONSOLE_LOG, QString(tr("Setting ESQL driver in GIXSQL_DB_MODE: %1")).arg(esql_env.value("GIXSQL_DB_MODE")), QLogger::LogLevel::Debug);

			bool esql_debug_log_on = prj->PropertyGetValue("esql_debug_log_on").toBool();
			if (esql_debug_log_on) {
				ide_task_manager->logMessage(GIX_CONSOLE_LOG, QString(tr("Enabling GixSQL debug log")), QLogger::LogLevel::Debug);
				env.insert("GIXSQL_DEBUG_LOG_ON", "1");
				QString esql_debug_log_file = prj->PropertyGetValue("esql_debug_log_file").toString();
				if (!esql_debug_log_file.isEmpty()) {
					ide_task_manager->logMessage(GIX_CONSOLE_LOG, QString(tr("Setting GixSQL debug log file to %1").arg(esql_debug_log_file)), QLogger::LogLevel::Debug);
					env.insert("GIXSQL_DEBUG_LOG", esql_debug_log_file);
				}
			}
			else {
				ide_task_manager->logMessage(GIX_CONSOLE_LOG, QString(tr("GixSQL debug log is disabled")), QLogger::LogLevel::Debug);
			}

			QString esql_error_log_file = prj->PropertyGetValue("esql_error_log_file").toString();
			if (!esql_error_log_file.isEmpty()) {
				ide_task_manager->logMessage(GIX_CONSOLE_LOG, QString(tr("Setting GixSQL error log file to %1").arg(esql_error_log_file)), QLogger::LogLevel::Debug);
				env.insert("GIXSQL_ERR_LOG", esql_error_log_file);
			}
		}
	}

	build_dir = prj->getBuildDirectory(build_configuration, target_platform);
	working_dir = (prj->PropertyGetValue("dbg_working_dir").toString().isEmpty()) ? build_dir : prj->PropertyGetValue("dbg_working_dir").toString();

	QString output_path = prj->PropertyGetValue("output_path").toString();
	if (output_path.isEmpty())
		output_path = build_dir;

	mm.add("prj.output_path", output_path);

	QString target_full_path = mm.translate(target);

	if (!QFile::exists(target_full_path)) {
		QString msg = QString(tr("Invalid target path (%1), please (re)build your project before running or debugging")).arg(target_full_path);
		ide_task_manager->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Error);
		UiUtils::ErrorDialog(msg);
		return false;
	}

	SysUtils::mergeEnvironmentVariable(env, "COB_LIBRARY_PATH", mm.translate(output_path));

#if defined (__linux__)
	SysUtils::mergeEnvironmentVariable(env, "LD_LIBRARY_PATH", compiler_cfg->libDirPath);
#endif

	QStringList dbg_run_env_vars = prj->PropertyGetValue("dbg_run_env_vars").toStringList();
	for (QString var : dbg_run_env_vars) {
		if (!var.contains("="))
			continue;

		QString name = var.left(var.indexOf("="));
		QString value = var.mid(var.indexOf("=") + 1);
		if (dbg_merge_env)
			SysUtils::mergeEnvironmentVariable(env, name, value);
		else
			env.insert(name, value);
	}

	// ******************************************************

	QString cmd;
	QStringList cobcrun_opts;
	bool uses_external_cmd = !(prj->PropertyGetValue("dbg_cmd").toString().isEmpty());

	QString build_type = prj->PropertyGetValue("build_type").toString();

	// No need to manually delete these, the ServerConfig they're eventually attached to 
	// will do it for us when it goes out of scope
	ServiceConfig *svc_rest = nullptr, *svc_soap = nullptr;

	ProjectFile *main_module_obj = prj->getStartupItem();

	if (main_module_obj && main_module_obj->isHttpService()) {
		cmd = PathUtils::combine(GixGlobals::getGixBinDir(), "gix-http");
		tmp_cfg_path = PathUtils::combine(QDir::tempPath(), "gix_http_" + SysUtils::randomString(6) + ".config");

		QString program_id = CobolUtils::extractProgramId(main_module_obj->GetFileFullPath());
		if (program_id.isEmpty()) {
			QString msg = tr("Invalid project configuration");
			ide_task_manager->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Error);
			UiUtils::ErrorDialog(msg);
			return false;
		}

		//int http_port = SysUtils::getSubProperty(main_module_obj->PropertyGetValue("is_rest_ws").toString(), "port").toInt();
		int http_port = main_module_obj->getSubProperty("is_rest_ws", "port").toInt();
		if (!http_port)
			http_port = 9090;

		ServerConfig svr;
		svr.setAddress("127.0.0.1");
		svr.setPort(http_port);
		svr.setRuntimePath(compiler_cfg->getLibCobDir());	// we point directly to the location of libcob.dll/.so
		svr.setDebugEnabled(is_debugging_enabled);
		svr.setLog(PathUtils::combine(PathUtils::getDirectory(target_full_path),
			svr.getAddressString().replace(".", "_") + "__" + QString::number(svr.getPort()) + ".log"));
		svr.setLogLevel("debug");
		svr.setLogConsoleEchoEnabled(true);
		svr.setSearchPath(mm.translate(output_path));

		if (main_module_obj->isRestService()) {
			svc_rest = ServiceConfig::ofType("rest");
			svc_rest->setName(startup_item_name);
			svc_rest->setProgram(startup_item_name);
			svc_rest->setDescription(QString("Service (REST) %1 at %2").arg(startup_item_name).arg(svr.getServerId()));
			svc_rest->setEnabled(true);
			svc_rest->setUrl("/" + main_module_obj->getSubProperty("is_rest_ws", PropertyConsts::WebProjectUrl).toString());

			QString itf_in_fld = main_module_obj->getSubProperty("is_rest_ws", "interface_in_field").toString();
			if (itf_in_fld.isEmpty()) {
				ide_task_manager->logMessage(GIX_CONSOLE_LOG, "Invalid input field", QLogger::LogLevel::Error);
				return false;
			}
			svc_rest->setInterfaceInFieldName(itf_in_fld);

			QString itf_out_fld = main_module_obj->getSubProperty("is_rest_ws", "interface_out_field").toString();
			if (itf_out_fld.isEmpty()) {
				ide_task_manager->logMessage(GIX_CONSOLE_LOG, "Invalid output field", QLogger::LogLevel::Error);
				return false;
			}
			svc_rest->setInterfaceOutFieldName(itf_out_fld);

			svc_rest->setLog(PathUtils::combine(build_dir, svc_rest->getName() + ".log"));
			svc_rest->setLogLevel("debug");
			svr.addService(startup_item_name, svc_rest);
		}

		if (main_module_obj->isSoapService()) {
			ide_task_manager->logMessage(GIX_CONSOLE_LOG, "Unsupported function", QLogger::LogLevel::Error);
			return false;
			//svc_soap = ServiceConfig::ofType("soap");
			//svc_soap->setName(startup_item_name);
			//svc_soap->setProgram(startup_item_name);
			//svc_soap->setDescription(QString("Service (SOAP) %1 at %2").arg(startup_item_name).arg(svr.getServerId()));
			//svc_soap->setEnabled(true);
			//svc_soap->setUrl("/" + startup_item_name);

			//svc_soap->setInterfaceIn("P:\\gix-ide\\test\\wstest\\WSINTF.cpy");
			//svc_soap->setInterfaceOut("P:\\gix-ide\\test\\wstest\\WSINTF.cpy");
			//svc_soap->setLog(PathUtils::combine(PathUtils::getDirectory(target), svc_soap->getName() + ".log"));
			//svc_soap->setLogLevel("debug");
			//svr.addService(startup_item_name, svc_soap);
		}

		if (!svr.write(tmp_cfg_path)) {
			QLogger::QLog_Error(GIX_CONSOLE_LOG, "Cannot write configuration file " + tmp_cfg_path);
			return false;
		}


		cobcrun_opts.append(tmp_cfg_path);
	}
	else {
		if (!uses_external_cmd) {
			if (build_type == "dll") {
				cmd = compiler_cfg->runnerPath;
				//cobcrun_opts.append("-M");
				//cobcrun_opts.append(target_full_path);
				cobcrun_opts.append(module_name);
			}
			else
				cmd = target_full_path;
		}
		else {
			cmd = prj->PropertyGetValue("dbg_cmd").toString();
		}
	}

	QStringList cmd_args = parseArguments(prj->PropertyGetValue("dbg_args").toString());
	cobcrun_opts.append(cmd_args);

	GixDebugger *gd = GixDebugger::get();
	if (!gd) {
		QString msg = tr("Cannot create a debugger instance. Unsupported platform?");
		QLogger::QLog_Error(GIX_CONSOLE_LOG, msg);
		UiUtils::ErrorDialog(msg);
		return false;
	}
	gd->setVerbose(ide_task_manager->isDebugOutputEnabled());

	QMap<QString, QString> dbg_env;

	for (auto k : env.keys()) {
		QString v = env.value(k);
		dbg_env[k] = v;
#if _DEBUG
		//ide_task_manager->logMessage(GIX_CONSOLE_LOG, k + "=" + v, QLogger::LogLevel::Trace);
#endif
	}

	gd->setEnvironment(dbg_env);
	gd->setProperty("symformat", compiler_cfg->isVsBased ? "pdb" : "dwarf");
	gd->setWorkingDirectory(working_dir);
	gd->setModuleDirectory(QFileInfo(target_full_path).path());
	gd->setProcess(cmd);
	gd->setCommandLine(cobcrun_opts.join(" "));
	gd->setDebuggedModuleType(build_type == "dll" ? DebuggedModuleType::Shared : DebuggedModuleType::Executable);
	gd->setDebuggingEnabled(is_debugging_enabled);
	gd->setUseExternalConsole(dbg_separate_console);

	QString stdin_file = prj->PropertyGetValue("dbg_stdin_file").toString();
	if (!stdin_file.isEmpty()) {
		if (!QFile::exists(stdin_file)) {
			QString msg = tr("The file specified as stdin input does not exist or is not accessible");
			QLogger::QLog_Error(GIX_CONSOLE_LOG, msg);
			UiUtils::ErrorDialog(msg);
			return false;
		}
		gd->setStdInFile(stdin_file);
	}


	debug_driver = new DebugDriver(this);

	debug_driver->setDebuggerInstance(gd);

	QObject::connect(debug_driver, &DebugDriver::DebuggerProcessFinished, this, [this](QString m, int l) { debuggedProcessFinished(l, m); }, Qt::ConnectionType::QueuedConnection);
	QObject::connect(debug_driver, &DebugDriver::DebuggerProcessStarted, this, [this](QString m) { debuggedProcessStarted(); }, Qt::ConnectionType::QueuedConnection);
	QObject::connect(debug_driver, &DebugDriver::DebuggerProcessError, this, [this](QString m, int l) {
		debuggedProcessError(l, m);
		QGuiApplication::restoreOverrideCursor();
	}, Qt::ConnectionType::QueuedConnection);

	QObject::connect(debug_driver, &DebugDriver::DebuggerReady, debug_driver, [this](QString msg) {
		QGuiApplication::restoreOverrideCursor();
	}, Qt::ConnectionType::DirectConnection);   // This is direct to avoid a race condition

	QObject::connect(debug_driver, &DebugDriver::DebuggerStdOutAvailable, this, [this](QString m) { ide_task_manager->consoleWriteStdOut(m); }, Qt::ConnectionType::QueuedConnection);
	QObject::connect(debug_driver, &DebugDriver::DebuggerStdErrAvailable, this, [this](QString m) { ide_task_manager->consoleWriteStdErr(m); }, Qt::ConnectionType::QueuedConnection);

	if (is_debugging_enabled) {
		loadDebugManagerState();

		QObject::connect(debug_driver, &DebugDriver::DebuggerBreak, this, [this](QString m, QString s, int l) { debug_break(m, s, l); }, Qt::ConnectionType::QueuedConnection);
		QObject::connect(debug_driver, &DebugDriver::DebuggerModuleChanged, this, [this](QString m, int l) { debug_module_changed(m, l); }, Qt::ConnectionType::QueuedConnection);
		QObject::connect(debug_driver, &DebugDriver::DebuggerModuleExit, this, [this](QString m, int l) { debug_program_exit(m, l); }, Qt::ConnectionType::QueuedConnection);
	}

	bool md_status = GixGlobals::getMetadataManager()->rebuildMetadata();

	ide_task_manager->consoleClear();

	QGuiApplication::setOverrideCursor(QCursor(Qt::WaitCursor));

	debug_driver_thread = new QThread(this);

	debug_driver->moveToThread(debug_driver_thread);

	connect(this, &DebugManager::startDriver, debug_driver, &DebugDriver::startDriver);
	connect(debug_driver_thread, &QThread::finished, debug_driver, &QObject::deleteLater);

	debug_driver_thread->start();

	emit startDriver();

	return true;
}


void DebugManager::readStdErr()
{
	QProcess *p = (QProcess *)sender();
	p->setReadChannel(QProcess::ProcessChannel::StandardError);
	QString s = p->readAll();
	ide_task_manager->logMessage(GIX_CONSOLE_LOG, s, QLogger::LogLevel::Error);
}

void DebugManager::readStdOut()
{
	QProcess *p = (QProcess *)sender();
	p->setReadChannel(QProcess::ProcessChannel::StandardOutput);
	QString s = p->readAll();
	ide_task_manager->logMessage(GIX_CONSOLE_LOG, s, QLogger::LogLevel::Info);
}

void DebugManager::debuggedProcessFinished(int rc, QString s)
{
	QString hexCode;
	hexCode.setNum((unsigned int)rc, 16);
	hexCode = "0x" + hexCode.rightJustified(8, '0');
	QString msg = QString(tr("Process finished with exit code %1 (%2)")).arg(rc).arg(hexCode);
	ide_task_manager->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Info);
	if (rc) {
		UiUtils::ErrorDialog(msg);
	}

	emit debugStopped();

}

void DebugManager::debuggedProcessError(int errcode, QString errmsg)
{

	QString msg = QString(tr("The process stopped with an error (%1): %2")).arg(errcode).arg(errmsg);
	ide_task_manager->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Error);
	//ide_task_manager->logMessage(GIX_CONSOLE_LOG, launched_process->errorString(), QLogger::LogLevel::Info);
	if (!is_user_initiated_stop) {
		UiUtils::ErrorDialog(msg);
	}

	//   __TRACE("DebugManager: emitting debugStopped");
	emit debugError();
}

void DebugManager::debuggedProcessStarted()
{
	//	__TRACE("DebugManager: emitting debugStarted");
	emit debugStarted();
}

void DebugManager::debug_module_changed(QString module, int ln)
{
	ide_task_manager->logMessage(GIX_CONSOLE_LOG, "DBG: MODULE CHANGED @" + module + ":" + QString::number(ln), QLogger::LogLevel::Debug);
}

void DebugManager::debug_program_exit(QString m, int l)
{
	//	__TRACE("DebugManager: emitting debugProgramExit");
	emit debugProgramExit();
}

void DebugManager::debug_break(QString module_name, QString src_file, int ln)
{
	//fprintf(stderr, "DBG BREAK\n");
	ide_task_manager->logMessage(GIX_CONSOLE_LOG, "DBG: BREAK @" + src_file + ":" + QString::number(ln), QLogger::LogLevel::Debug);

	bool bkp_located = false;

	//CobolModuleMetadata *cmm = dbg_metadata_by_module.contains(module_name) ? dbg_metadata_by_module[module_name] : nullptr;
	CobolModuleMetadata *cmm = GixGlobals::getMetadataManager()->getModuleMetadata(module_name);
	if (cmm) {
		if (!cmm->isPreprocessedESQL()) {	// Type 1
			cur_src_file = src_file;
			cur_line = ln;
			bkp_located = true;
		}
		else {
			bkp_located = translateBreakpointReverse(cmm, src_file, ln, cur_src_file, &cur_line);
		}

		if (bkp_located) {
			ide_task_manager->logMessage(GIX_CONSOLE_LOG, "Located line " + QString::number(ln) + " for file " + src_file, QLogger::LogLevel::Trace);
			ide_task_manager->setStatus(IdeStatus::DebuggingOnBreak);

			emit ide_task_manager->IdeDebuggerBreak();
			emit ide_task_manager->IdeEditorChangedPosition(cur_src_file, cur_line);
		}
		else {
			ide_task_manager->logMessage(GIX_CONSOLE_LOG, "Cannot locate line " + QString::number(ln) + " for file " + src_file, QLogger::LogLevel::Error);

		}
	}
	else {
		ide_task_manager->logMessage(GIX_CONSOLE_LOG, "Cannot locate module for file " + src_file, QLogger::LogLevel::Error);
	}
}

void DebugManager::step()
{
	QString module;
	int ln;

	ide_task_manager->setStatus(IdeStatus::Debugging);

	driverWrite(DebugDriver::CMD_STEP);
}

void DebugManager::stop()
{
	if (debug_driver)
		debug_driver->stop();

	driverWrite(DebugDriver::CMD_CONTINUE);

	//debug_driver->quit();
}

void DebugManager::continue_running()
{
	QString module;
	int ln;

	ide_task_manager->setStatus(IdeStatus::Debugging);

	driverWrite(DebugDriver::CMD_CONTINUE);
	QString resp = debug_driver->getLastResponse();
}

QString DebugManager::getCurrentSourceFile()
{
	return QDir::cleanPath(cur_src_file);
}

QString DebugManager::getCurrentCobolModuleName()
{
	return cur_module;
}

int DebugManager::getCurrentLine()
{
	return cur_line;
}

bool DebugManager::parsePosition(QString pos, QString &module, int *line)
{
	QRegularExpression rx("^([A-Za-z0-9\\-_]+):([0-9]+)$");
	QRegularExpressionMatch m = rx.match(pos);

	if (m.hasMatch()) {
		module = m.captured(1);
		*line = m.captured(2).toInt();
		return true;
	}
	return false;
}

void DebugManager::loadDebugManagerState()
{
	ProjectCollection *ppj = Ide::TaskManager()->getCurrentProjectCollection();
	if (ppj == nullptr)
		return;

	watched_vars.append(Ide::TaskManager()->getWatchedVars());
}

void DebugManager::saveDebugManagerState()
{
	ProjectCollection *ppj = Ide::TaskManager()->getCurrentProjectCollection();
	if (ppj == nullptr)
		return;

	Ide::TaskManager()->setWatchedVars(watched_vars);
}

QString DebugManager::driverWrite(const QString &msg)
{
	//driver_client.write(msg.toLocal8Bit().constData());
	debug_driver->write(msg);
	return "OK";
}

QStringList DebugManager::parseArguments(QString args)
{
	if (args.isEmpty())
		return QStringList();

	bool inside = (args.at(0) == "\""); //true if the first character is "
	QStringList tmpList = args.split(QRegExp("\""), QString::SkipEmptyParts); // Split by " and make sure you don't have an empty string at the beginning
	QStringList arglist;
	foreach(QString s, tmpList)
	{
		if (inside) { // If 's' is inside quotes ...
			arglist.append(s); // ... get the whole string
		}
		else { // If 's' is outside quotes ...
			arglist.append(s.split(" ", QString::SkipEmptyParts)); // ... get the splitted string
		}
		inside = !inside;
	}
	return arglist;
}

QString DebugManager::getPrintableVarContent(QString n)
{
	driverWrite(DebugDriver::CMD_GET_VAR + n);
	QString value = debug_driver->getLastResponse();
	if (value.startsWith("OK:"))
		return value.mid(3);
	else
		return value;
}

QMap<QString, QString> DebugManager::getPrintableVarListContent(QStringList vlist)
{
	QMap<QString, QString> res;
	if (!vlist.size())
		return res;

	QList<VariableData *> var_req;
	for (QString var_name : vlist) {
		VariableData *vd = new VariableData();
		vd->var_name = var_name;
		var_req.push_back(vd);
	}

	if (debug_driver->debuggerInstance()->getVariables(var_req)) {
		for (VariableData *vd : var_req) {
			
			if (!vd)
				continue;

			QString s = "";
			VariableResolverData *vrd = vd->resolver_data;

			if (!vrd) {
				res[vd->var_name] = "{??}";
				continue;
			}
			
			formatVariable(vrd->var_path, vd->data, vrd->type, (WsEntryStorageType)vrd->storage_type, vrd->storage_size, vrd->display_size, vrd->is_signed, vrd->decimals, s);
			res[vd->var_name] = s;

			delete vd->data;
		}
	}

	qDeleteAll(var_req);

	return res;
}

void DebugManager::formatVariable(QString var_path, uint8_t *data, WsEntryType type, WsEntryStorageType storage_type, int storage_size, int display_size, bool is_signed, int decimals, QString& vres)
{
	char *s = nullptr;
	int ndigits = 0;

	if (!data || !storage_size) {
		vres += "{??}";
		return;
	}

	switch (type) {
		// TODO: storage size and address are not computed correctly for group variables
		case WsEntryType::Group: 
			{
				QString module_name = debug_driver->debuggerInstance()->getCurrentCobolModuleName();
				CobolModuleMetadata *cmm = GixGlobals::getMetadataManager()->getModuleMetadata(module_name);
				if (!cmm) {
					vres += "{??}";
					break;
				}

				// We need a DataEntry, since it has more information (children, etc.)
				DataEntry *e = cmm->findDefinition(var_path, true);
				if (!e) {
					vres += "{??}";
					break;
				}

				for (DataEntry *c : e->children) {
					formatVariable(c->path, data + c->offset_local, c->type, c->storage_type, c->storage_size, c->display_size, c->is_signed, c->decimals, vres);
				}
			}
			break;

		case WsEntryType::Filler:
			vres += QString(display_size, ' ');
			break;

		case WsEntryType::Unknown:
			vres += "{??}";
			break;

		default:
			switch (storage_type) {

				case WsEntryStorageType::Literal:
					s = (char *)malloc(storage_size + 1);
					memcpy(s, data, storage_size);
					s[storage_size] = 0;

					vres += QString(s);
					free(s);
					break;

				case WsEntryStorageType::Comp3:
					ndigits = display_size - ((is_signed ? 1 : 0) + (decimals > 0 ? 1 : 0));
					s = comp3_to_display(ndigits, decimals, is_signed, data);
					vres += QString::fromLocal8Bit(s);
					free(s);
					break;

				case WsEntryStorageType::Comp:
				case WsEntryStorageType::Comp5:
					bool is_native_binary = (storage_type == WsEntryStorageType::Comp5);
					ndigits = display_size - ((is_signed ? 1 : 0) + (decimals > 0 ? 1 : 0));
					s = comp5_to_display(ndigits, decimals, is_signed, data, is_native_binary);
					vres += QString::fromLocal8Bit(s);
					free(s);
					break;

			}
	}

}

int DebugManager::getWatchedVarCount()
{
	return watched_vars.size();
}

QString DebugManager::getWatchedVarName(int i)
{
	return watched_vars.at(i);
}

void DebugManager::addWatchedVar(QString s)
{
	if (!watched_vars.contains(s))
		watched_vars.append(s);
}

void DebugManager::removeWatchedVar(QString s)
{
	if (watched_vars.contains(s))
		watched_vars.removeOne(s);
}

QStringList DebugManager::getWatchedVarList()
{
	return watched_vars;
}

QStringList DebugManager::getTranslatedBreakpoints()
{
	bool dbg_stop_at_first_line = debugged_prj->PropertyGetValue("dbg_stop_at_first_line").toBool();
	QStringList usr_breakpoints;
	QStringList translated_breakpoints;

	usr_breakpoints = Ide::TaskManager()->getBreakpoints();

	for (auto usr_bkp : usr_breakpoints) {
		QString srcfile = usr_bkp.mid(usr_bkp.indexOf('@') + 1);
		CobolModuleMetadata *the_module = nullptr;

		for (QString module_name : GixGlobals::getMetadataManager()->getModulesSourceMap().keys()) {
			CobolModuleMetadata *cmm = GixGlobals::getMetadataManager()->getModuleMetadata(module_name);
			if (!cmm) {
				QString msg = QString(tr("Cannot resolve data for module %1")).arg(module_name);
				ide_task_manager->logMessage(GIX_CONSOLE_LOG, msg, QLogger::LogLevel::Error);
				continue;
			}

			if (GixGlobals::getMetadataManager()->getModulesSourceMap().value(module_name)->GetFileFullPath() == srcfile || 
					cmm->getFileDependencies().contains(srcfile)) {
				the_module = cmm;
				break;
			}
		}

		if (!the_module)
			continue;

#if _DEBUG_LOG_ON
		the_module->dumpOriginalToRunning();
#endif

		bool is_esql = the_module->isPreprocessedESQL();

		if (!is_esql)
			translated_breakpoints.append(usr_bkp);
		else
			translated_breakpoints.append(translateBreakpoint(the_module, usr_bkp));
	}

	return translated_breakpoints;
}

void DebugManager::setUserInititatedStop(bool b)
{
	is_user_initiated_stop = b;
}

void DebugManager::writeToProcess(QString s)
{
	if (debug_driver)
		debug_driver->writeToProcess(s);
}

QString DebugManager::translateBreakpoint(CobolModuleMetadata *cmm, const QString &orig_bkp)
{
	QString orig_src_file = orig_bkp.mid(orig_bkp.indexOf("@") + 1);
	int orig_ln = orig_bkp.mid(0, orig_bkp.indexOf("@")).toInt();

	QString running_file;
	int running_file_id = 0;
	int running_line = 0;
	if (!cmm->originalToRunning(cmm->originalFileId(), orig_ln, &running_file_id, &running_line))
		return QString();

	if (!cmm->getFileById(running_file_id, running_file))
		return QString();

	if (running_file.startsWith("#")) {
		running_file = PathUtils::combine(build_dir, running_file.mid(1));
	}

	QString tx_bkp = QString::number(running_line) + "@" + running_file;

#if defined(_WIN32) && defined(_DEBUG)
	OutputDebugStringA(("BKP : " + orig_bkp + "-> " + tx_bkp + "\n").toLocal8Bit().constData());
#endif

	return tx_bkp;
}

QStringList DebugManager::translateBreakpoints(CobolModuleMetadata *cmm, const QStringList &orig_bkps)
{
	QStringList tx_bkps;
	for (QString orig_bkp : orig_bkps) {
		QString tx_bkp = translateBreakpoint(cmm, orig_bkp);
		if (!tx_bkp.isEmpty())
			tx_bkps.append(tx_bkp);
	}
	return tx_bkps;
}

bool DebugManager::translateBreakpointReverse(CobolModuleMetadata *cmm, const QString &running_file, int running_ln, QString &orig_file, int *orig_ln)
{
	QString r_orig_file;
	int r_orig_ln = 0;
	int r_orig_file_id = 0;

	if (!cmm->runningToOriginal(cmm->runningFileId(), running_ln, &r_orig_file_id, &r_orig_ln))
		return false;

	if (!cmm->getFileById(r_orig_file_id, r_orig_file))
		return false;

	orig_file = r_orig_file;
	*orig_ln = r_orig_ln;

	return true;
}

char *DebugManager::comp3_to_display(int total_len, int scale, int has_sign, uint8_t *addr)
{
	int display_len = total_len;

	int bfrlen = display_len + (has_sign ? 1 : 0) + (scale > 0 ? 1 : 0);

	char *copy = (char *)malloc(bfrlen + 1);
	memset(copy, ASCII_ZERO, display_len);
	uint8_t *ptr = (uint8_t *)copy;

	int storage_len = (total_len / 2) + 1;

	if (has_sign) {
		uint8_t sign_byte = addr[storage_len - 1] & 0x0f;
		char sign = (sign_byte == 0x0d) ? '-' : '+';
		*(ptr++) = sign;

	}

	int ndigits = total_len;

	int skip_first_nibble = ((ndigits + 1) % 2) > 0;
	int ndigit = 0;
	int decsep_pos = (total_len - scale) + (has_sign ? 1 : 0) - 1;
	for (int i = 0; i < storage_len; i++) {

		uint8_t b = addr[i];

		if (i > 0 || !skip_first_nibble) {
			uint8_t digit_l = (b & 0xf0) >> 4;
			digit_l += ASCII_ZERO;
			*(ptr++) = digit_l;
			ndigit++;
		}


		if (ndigit == decsep_pos) {
			*(ptr++) = '.';
		}

		uint8_t digit_r = (b & 0x0f);
		digit_r += ASCII_ZERO;
		*(ptr++) = digit_r;

		ndigit++;
		if (ndigit == decsep_pos) {
			*(ptr++) = '.';
		}
	}

	copy[bfrlen] = '\0';

	return copy;
}

char *DebugManager::comp5_to_display(int total_len, int scale, int has_sign, uint8_t *addr, bool is_native_binary)
{
	int display_len = total_len;

	int bfrlen = display_len + (has_sign ? 1 : 0) + (scale > 0 ? 1 : 0);

	char *bfr = (char *)malloc(bfrlen + 1);
	memset(bfr, ASCII_ZERO, display_len);
	uint8_t *ptr = (uint8_t *)bfr;

	bool is_negative = false;

	if (!has_sign) {
		if (total_len == 1) {	// 1 byte
			uint8_t n8 = *((uint8_t *)addr);
			snprintf((char *)bfr, total_len, "%d", n8);
		}
		else {
			if (total_len == 2) {	// 1 byte
				uint8_t n8 = *((uint8_t *)addr);
				snprintf((char *)bfr, total_len, "%d", n8);
			}
			else {
				if (total_len == 3 || total_len == 4) {	// 2 bytes
					uint16_t n16 = *((uint16_t *)addr);
					if (!is_native_binary)
						n16 = COB_BSWAP_16(n16);
					snprintf((char *)bfr, total_len, "%d", n16);
				}
				else {
					if (total_len >= 5 || total_len <= 9) {	// 4 bytes
						uint32_t n32 = *((uint32_t *)addr);
						if (!is_native_binary)
							n32 = COB_BSWAP_32(n32);
						snprintf((char *)bfr, total_len, "%d", n32);
					}
					else {
						if (total_len >= 10 || total_len <= 18) {	// 8 bytes
							uint64_t n64 = *((uint64_t *)addr);
							if (!is_native_binary)
								n64 = COB_BSWAP_64(n64);
							snprintf((char *)bfr, total_len, "%d", n64);
						}
						else {
							// Should never happen
						}
					}
				}
			}
		}
	} 
	else {
		if (total_len == 1) {	// 1 byte
			int8_t n8 = *((int8_t *)addr);
			is_negative = n8 < 0;
			snprintf((char *)bfr, total_len, "%d", abs(n8));
		}
		else {
			if (total_len == 2) {	// 1 byte
				int8_t n8 = *((int8_t *)addr);
				is_negative = n8 < 0;
				snprintf((char *)bfr, total_len, "%d", abs(n8));
			}
			else {
				if (total_len == 3 || total_len == 4) {	// 2 bytes
					int16_t n16 = *((int16_t *)addr);
					if (!is_native_binary)
						n16 = COB_BSWAP_16(n16);
					is_negative = n16 < 0;
					snprintf((char *)bfr, total_len, "%d", abs(n16));
				}
				else {
					if (total_len >= 5 || total_len <= 9) {	// 4 bytes
						int32_t n32 = *((int32_t *)addr);
						if (!is_native_binary)
							n32 = COB_BSWAP_32(n32);
						is_negative = n32 < 0;
						snprintf((char *)bfr, total_len, "%d", labs(n32));
					}
					else {
						if (total_len >= 10 || total_len <= 18) {	// 8 bytes
							int64_t n64 = *((int64_t *)addr);
							if (!is_native_binary)
								n64 = COB_BSWAP_64(n64);
							is_negative = n64 < 0;
							snprintf((char *)bfr, total_len, "%d", llabs(n64));
						}
						else {
							// Should never happen
						}
					}
				}
			}
		}
	}

	bfr[bfrlen] = '\0';

	char *final_bfr = (char *)malloc(bfrlen + 1);
	memset(final_bfr, ASCII_ZERO, display_len);
	char *fptr = (char *)final_bfr + (has_sign ? 1 : 0) +  (display_len -strlen(bfr));
	strcpy(fptr, bfr);

	if (has_sign)
		final_bfr[0] = is_negative ? '-' : '+';

	free(bfr);

	return final_bfr;
}
