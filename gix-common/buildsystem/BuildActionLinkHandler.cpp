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

#include "BuildActionLinkHandler.h"

#include <QEventLoop>

#include "ProjectFile.h"
#include "PathUtils.h"
#include "GixGlobals.h"
#include "BuildConsts.h"
#include "MacroManager.h"
#include "MetadataManager.h"
#include "SymbolMappingEntry.h"
#include "SysUtils.h"
#include "ESQLConfiguration.h"
#include "DataEntry.h"
#include "linq/linq.hpp"

#if defined(_WIN32) || defined(_WIN64)
#include "windows.h"
#endif

using namespace cpplinq;

#if defined(_WIN32)
#define EMIT_FORCE_ATTR if (!compiler_cfg->isVsBased) fs << "__attribute__((__used__)) "; if (!compiler_cfg->isVsBased) fs << " __declspec(dllexport) ";
#else
#define EMIT_FORCE_ATTR if (!compiler_cfg->isVsBased) fs << "__attribute__((__used__)) ";
#endif

BuildActionLinkHandler::BuildActionLinkHandler()
{}


BuildActionLinkHandler::~BuildActionLinkHandler()
{}

bool BuildActionLinkHandler::startBuild()
{
	QSettings settings;

	if (target->dependencies()->size() < 1)
		return false;

	importProjectEnvironment();

	MacroManager mm(environment);

	QString build_dir = getBuildDirectory();
	QDir(".").mkpath(build_dir);

	bool is_web_project = environment["__project_type_id"].toInt() == (int)ProjectType::Web;

	QString build_configuration = build_driver->getBuildEnvironment()["configuration"].toString();
	QString target_platform = build_driver->getBuildEnvironment()["platform"].toString();
	QString target_type = build_configuration + "/" + target_platform;

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

	QStringList link_dirs = retrieve_link_dirs(esql_cfg.get());
	if (SysUtils::isLinux()) {
		link_dirs.append(compiler_cfg->libDirPath);
	}

	if (link_dirs.size() > 0) {
		for (QString link_dir : link_dirs) {
			cobc_opts.append("-L");
			cobc_opts.append(link_dir);
		}
	}

	QStringList link_libs = retrieve_link_libs(esql_cfg.get());
	if (link_libs.size() > 0) {
		for (QString link_lib : link_libs) {
			cobc_opts.append("-l");
#ifdef Q_OS_UNIX
			if (link_lib.startsWith("lib") && link_lib.size() > 3)
				link_lib = link_lib.mid(3);

#endif
			cobc_opts.append(link_lib);
		}
	}

	QString target_final_path;
	if (!QDir::isAbsolutePath(target->filename()))
		target_final_path = PathUtils::combine(build_dir, target->filename());
	else
		target_final_path = target->filename();

	if (environment["build_type"].toString() == "exe")
		cobc_opts.append("-x");
	else
		cobc_opts.append("-b");

#if _DEBUG
	cobc_opts.append("-v");
#endif

	if (build_configuration == "debug") {
		cobc_opts.append("-g");
		cobc_opts.append("-debug");
		cobc_opts.append("-Og");
		cobc_opts.append("-A");
		cobc_opts.append("-gdwarf-4");

		if (compiler_cfg->isVsBased) {
			cobc_opts.append("-A");
			cobc_opts.append("/DEBUG:FULL");			
			
			cobc_opts.append("-A");
			cobc_opts.append("/WHOLEARCHIVE");

			cobc_opts.append("-A");
			cobc_opts.append("/OPT:NOREF");
		}
		else {
			cobc_opts.append("-A");
			cobc_opts.append("-O0");
		}
	}
    
    if (environment["cobc_c_link_opts"].isValid() && !environment["cobc_c_link_opts"].toString().isEmpty()) {
        QStringList l_opts = environment["cobc_c_link_opts"].toString().split(" ", QString::SplitBehavior::SkipEmptyParts);
        for (QString l_opt : l_opts)
            cobc_opts.append(l_opt);
	}    

	cobc_opts.append("-o");
	cobc_opts.append(target_final_path);

	QList<BuildTarget *> *deps;
	deps = target->dependencies();

	std::vector<QString> obj_list = from(*deps).where([](BuildTarget *b) { return b->providesOneOf({ BuildConsts::TYPE_OBJ, BuildConsts::TYPE_OBJ_MAIN }); }).select([](BuildTarget *bt) { return bt->filename(); }).to_vector();
	for (QString s : obj_list) {
		cobc_opts.append(s);
	}

	if (build_configuration == "debug" || is_web_project) {

		//std::vector<BuildTarget *> mod_bt_list = from(*deps).where([](BuildTarget *b) {  return b->providesOneOf({ BuildConsts::TYPE_OBJ, BuildConsts::TYPE_OBJ_MAIN }); }).select([](BuildTarget *bt) { return bt; }).to_vector();

		//QStringList mod_src_list;

		//for (auto mod_bt : mod_bt_list) {
		//	if (environment.contains("preprocess_esql") && environment["preprocess_esql"].toBool()) {
		//		auto cbsql_bt = mod_bt->getDependency(BuildConsts::TYPE_CBSQL);
		//		if (cbsql_bt) {
		//			auto cbl_bt = cbsql_bt->getDependency(BuildConsts::TYPE_COBOL);
		//			if (cbl_bt) {
		//				mod_src_list.append(cbl_bt->filename());
		//			}
		//		}
		//	}
		//	else {
		//		auto cbl_bt = mod_bt->getDependency(BuildConsts::TYPE_COBOL);
		//		if (cbl_bt) {
		//			mod_src_list.append(cbl_bt->filename());
		//		}
		//	}
		//}

		bool md_status = GixGlobals::getMetadataManager()->rebuildMetadata();

		auto msfl = GixGlobals::getMetadataManager()->getModulesSourceMap().values();
		QStringList mod_src_list = QStringList::fromStdList(from(msfl).select([](ProjectFile *pf) { return pf->GetFileFullPath();  }).to_list());

		QString dbg_helper_obj;
		if (generateDebugHelperObj(mod_src_list, target_final_path, build_dir, dbg_helper_obj))
			cobc_opts.append(dbg_helper_obj);
		else {
			build_driver->log_build_message(QString(tr("Cannot build debug helper")), spdlog::level::warn);
		}
	}

	build_driver->log_build_message(cobc + " " + cobc_opts.join(" "), spdlog::level::info);

	QProcess *p = new QProcess();

	p->setTextModeEnabled(false);

	p->setProgram(cobc);

	p->setArguments(cobc_opts);
	p->setProcessEnvironment(env);

	QString output_path = environment["output_path"].toString();
	p->setWorkingDirectory(build_dir);

	//dump_environment(env, build_driver);

	QEventLoop loop;
	bool ps_err = false;

	connect(p, &QProcess::readyReadStandardError, this, [this, p] { readStdErr(p); });
	connect(p, &QProcess::readyReadStandardOutput, this, [this, p] { readStdOut(p); });
	connect(p, &QProcess::errorOccurred, this, [this, &p, ps_err](QProcess::ProcessError _err) mutable { build_driver->log_build_message(p->errorString(), spdlog::level::err); ps_err = true; });

	connect(p, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),
		[this, &loop, p](int exitCode, QProcess::ExitStatus exitStatus) { readStdErr(p); readStdOut(p); loop.quit(); });

	p->start();
	if (!p->waitForStarted()) {
		build_driver->log_build_message("ERROR: " + p->errorString(), spdlog::level::err);
		return false;
	}

	loop.exec();

	int rc = p->exitCode();
	bool res = ((!rc)); // && QFile::exists(target_final_path)
	if (res) {
		build_driver->log_build_message(tr("Build successful") + ": " + target_final_path, spdlog::level::info);
	}
	else {
		build_driver->log_build_message(tr("Build error"), spdlog::level::err);
		build_driver->log_build_message("Exit code: " + QString::number(rc), spdlog::level::trace);
		return false;
	}

	delete(p);

	return true;
}

QStringList BuildActionLinkHandler::retrieve_link_dirs(ESQLConfiguration *esql_cfg)
{
	QStringList res;

	// TODO: user libs

	if (environment.value("preprocess_esql", false).toBool()) {
		res.append(esql_cfg->getLinkLibPathList());
	}
	return res;
}

QStringList BuildActionLinkHandler::retrieve_link_libs(ESQLConfiguration *esql_cfg)
{
	QStringList res;

	// TODO: user libs

	if (environment.value("preprocess_esql", false).toBool()) {
		res.append(esql_cfg->getLinkLibNameList());
	}
	return res;
}

#define DUMP_QSTRING(QS)  for (unsigned char c : QS.toLocal8Bit()) { fs << (int)c; fs << ", "; } fs << "0,\n";
#define DUMP_STDSTRING(S)  for (unsigned char c : S) { fs << (int)c; fs << ", "; } fs << "0,\n";
#define DUMP_INT(II) { uint32_t l = (uint32_t)II; unsigned char *pl = (unsigned char *)&l; fs << (unsigned char)(*(pl++)); fs << ", "; fs << (unsigned char)(*(pl++)); fs << ", "; fs << (unsigned char)(*(pl++)); fs << ", "; fs << (unsigned char)(*(pl++)); fs << ",\n"; }

bool BuildActionLinkHandler::generateDebugHelperObj(QStringList srclist, QString target_path, QString build_dir, QString& dbg_helper_obj)
{
	if (!QDir(build_dir).exists())
		return false;

	QString build_configuration = build_driver->getBuildEnvironment()["configuration"].toString();
	QString target_platform = build_driver->getBuildEnvironment()["platform"].toString();
	QString target_type = build_configuration + "/" + target_platform;

	QScopedPointer<CompilerConfiguration> ccfg(CompilerConfiguration::get(build_configuration, target_platform, environment));
	CompilerConfiguration* compiler_cfg = ccfg.data();
	if (compiler_cfg == nullptr) {
		build_driver->log_build_message(QString(tr("Invalid compiler configuration for target ")).arg(target_type), spdlog::level::err, 1);
		return false;
	}


	target_path = PathUtils::changeExtension(PathUtils::getFilename(target_path), environment.value("sys.objext").toString());
	QString obj_filename = "gix_dbg_" + target_path;
	obj_filename = PathUtils::combine(build_dir, obj_filename);
	QString c_filename = PathUtils::changeExtension(obj_filename, ".c");

	/*
	#include <stdint.h>

// cl -c test.c /Fott.obj

typedef struct {
	const char *sym_name;
	const char *var_name;
	uint64_t len;
} st_gix_sym;

int __GIX_SYM_TEST000_C = 2;

st_gix_sym __GIX_SYM_TEST000_D[] = {
	{ "b_8", "AA", 255},
	{ "b_9", "BB", 255},
};

	*/

	QFile cfile(c_filename);
	cfile.open(QIODevice::OpenModeFlag::WriteOnly | QIODevice::OpenModeFlag::Text);
	QTextStream fs(&cfile);

	fs << "typedef struct {const char *sym_name; const char *var_name; int len; } st_gix_sym; \n";

	QList<CobolModuleMetadata*> mod_list;

	for (QString src : srclist) {
		CobolModuleMetadata* cmm = GixGlobals::getMetadataManager()->getModuleMetadataBySource(src);
		if (cmm)
			mod_list.append(cmm);
	}

	if (compiler_cfg->isVsBased) {

		if (target_platform != "x86") {
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_MODULES\"));\n");	// Module list
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_MODULES_C\"));\n");	// Module list (count)
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_MODULES_S\"));\n");	// Module list (size)

			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_PPB\"));\n");	// Pre-processed block list
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_PPB_C\"));\n");	// Pre-processed block list (count)
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_PPB_S\"));\n");	// Pre-processed block list (size)
		}
		else {
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_MODULES=___GIX_SYM_MODULES\"));\n");		// Module list
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_MODULES_C=___GIX_SYM_MODULES_C\"));\n");	// Module list (count)
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_MODULES_S=___GIX_SYM_MODULES_S\"));\n");	// Module list (size)

			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_PPB=___GIX_SYM_PPB\"));\n");		// Pre-processed block list
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_PPB_C=___GIX_SYM_PPB_C\"));\n");	// Pre-processed block list (count)
			fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_PPB_S=___GIX_SYM_PPB_S\"));\n");	// Pre-processed block list (size)
		}

		for (CobolModuleMetadata* cmm : mod_list) {
			// Some VS weirdness at work: VS decorates names for x86 .obj symbols, but not for x64
			if (target_platform != "x86") {
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_M\"));\n").arg(cmm->getModuleName());	// Mapping (data)
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_MC\"));\n").arg(cmm->getModuleName());	// Mapping (count)
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_MS\"));\n").arg(cmm->getModuleName());	// Mapping (size)

				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_E\"));\n").arg(cmm->getModuleName());	// Entries (data)
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_EC\"));\n").arg(cmm->getModuleName());	// Entries (count)
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_ES\"));\n").arg(cmm->getModuleName());	// Entries (size)
			}
			else {
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_M=___GIX_SYM_%1_M\"));\n").arg(cmm->getModuleName());	// Mapping (size)
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_MC=___GIX_SYM_%1_MC\"));\n").arg(cmm->getModuleName());	// Mapping (data)
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_MS=___GIX_SYM_%1_MS\"));\n").arg(cmm->getModuleName());	// Mapping (count)

				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_E=___GIX_SYM_%1_E\"));\n").arg(cmm->getModuleName());	// Entries (data)
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_EC=___GIX_SYM_%1_EC\"));\n").arg(cmm->getModuleName());	// Entries (count)
				fs << QString("__pragma(comment(linker, \"/export:__GIX_SYM_%1_ES=___GIX_SYM_%1_ES\"));\n").arg(cmm->getModuleName());	// Entries (size)
			}
		}
	}

	int m_size = 0;
	int m_count = mod_list.size();
	QStringList m_names;
	for (CobolModuleMetadata* cmm : mod_list) {
		m_size += (cmm->getModuleName().size() + 1);
		m_names.append(cmm->getModuleName());
	}
	m_size += 1;

	// Module-list data

	fs << QString("/* Modules: %1  */\n").arg(m_names.join(", "));

	EMIT_FORCE_ATTR
		fs << QString("int __GIX_SYM_MODULES_C = %1;\n").arg(m_count);

	EMIT_FORCE_ATTR
		fs << QString("int __GIX_SYM_MODULES_S = %1;\n").arg(m_size);

	fs << QString("unsigned char __GIX_SYM_MODULES[] = {\n");
	for (QString m_name : m_names) {
		DUMP_QSTRING(m_name);
	}
	fs << "0\n";
	fs << "};\n\n";


	// Module-specific data

	for (CobolModuleMetadata* cmm : mod_list) {

		EMIT_FORCE_ATTR
			fs << QString("int __GIX_SYM_%1_MC = %2;\n").arg(cmm->getModuleName()).arg(cmm->getSymbolMappingTable().size());

		int size = 0;
		for (int i = 0; i < cmm->getSymbolMappingTable().size(); i++) {
			auto sm = cmm->getSymbolMappingTable().at(i);
			size += (sm->id.length() + sm->cbl_var.length() + sizeof(int) + 2);
		}
		size += 1;

		EMIT_FORCE_ATTR
			fs << QString("int __GIX_SYM_%1_MS = %2;\n").arg(cmm->getModuleName()).arg(size);

		EMIT_FORCE_ATTR
			fs << QString("unsigned char __GIX_SYM_%1_M[] = {\n").arg(cmm->getModuleName());
		for (int i = 0; i < cmm->getSymbolMappingTable().size(); i++) {
			auto sm = cmm->getSymbolMappingTable().at(i);

			fs << QString("/* \"%1\",  \"%2\", %3  */\n").arg(sm->id).arg(sm->cbl_var).arg(sm->storage_len);
			DUMP_QSTRING(sm->id);
			DUMP_QSTRING(sm->cbl_var);

			DUMP_INT(sm->storage_len);
		}
		fs << "0\n";
		fs << "};\n\n";

		QList<DataEntry*> entries;
		QList<DataEntry*> etree = cmm->getDataEntries();
		cmm->flattenEntryTree(entries, etree);

		EMIT_FORCE_ATTR
			fs << QString("int __GIX_SYM_%1_EC = %2;\n\n").arg(cmm->getModuleName()).arg(entries.size());

		EMIT_FORCE_ATTR
			fs << QString("unsigned char __GIX_SYM_%1_E[] = {\n").arg(cmm->getModuleName());

		/*
		entry:
		name, path, topmost_symbol_name, local_offset, storage_size,

		display_size, is_signed, decimals, format, storage_type,
		storage, occurs, redefines
		*/

		int esize = 0;
		for (DataEntry* e : entries) {
			QString local_sym_name = e->getTopMostParent()->name;
			fs << QString("/* \"%1\", \"%2\", %3, %4, \"%5\", %6, %7 */\n").arg(e->name).arg(e->path).arg((uint32_t)e->type).arg(e->level).arg(local_sym_name).arg(e->offset_local).arg(e->storage_size);
			fs << QString("/* %1, %2, %3, \"%4\", %5, %6, %7 */\n").arg(e->display_size).arg(e->is_signed).arg(e->decimals).arg(e->format).arg((int)e->storage_type).arg(e->occurs).arg(e->redefines);
			DUMP_QSTRING(e->name); esize += (e->name.length() + 1);
			DUMP_QSTRING(e->path); esize += (e->path.length() + 1);
			DUMP_INT((uint32_t)e->type); esize += (sizeof(uint32_t));
			DUMP_INT((uint32_t)e->level); esize += (sizeof(uint32_t));
			DUMP_QSTRING(local_sym_name); esize += (local_sym_name.length() + 1);
			DUMP_INT((uint32_t)e->offset_local); esize += (sizeof(uint32_t));
			DUMP_INT((uint32_t)e->storage_size); esize += (sizeof(uint32_t));

			DUMP_INT((uint32_t)e->display_size); esize += (sizeof(uint32_t));
			DUMP_INT((uint32_t)e->is_signed); esize += (sizeof(uint32_t));
			DUMP_INT((uint32_t)e->decimals); esize += (sizeof(uint32_t));
			DUMP_QSTRING(e->format); esize += (e->format.length() + 1);
			DUMP_INT((uint32_t)e->storage_type); esize += (sizeof(uint32_t));
			DUMP_INT((uint32_t)e->occurs); esize += (sizeof(uint32_t));
			DUMP_QSTRING(e->redefines); esize += (e->redefines.length() + 1);

			fs << "\n";
		}
		esize++;

		fs << "0\n";
		fs << "};\n\n";

		EMIT_FORCE_ATTR
			fs << QString("int __GIX_SYM_%1_ES = %2;\n\n").arg(cmm->getModuleName()).arg(esize);
	}

	// Pre-processed blocks

	int ppbs_count = 0;
	int ppbs_size = 0;

	for (CobolModuleMetadata* cmm : mod_list) {
		ppbs_count += cmm->getPreprocessedBlocks().size();
	}

	fs << QString("/* Pre-processed blocks: %1  */\n").arg(ppbs_count);

	// block count
	EMIT_FORCE_ATTR
		fs << QString("int __GIX_SYM_PPB_C = %1;\n\n").arg(ppbs_count);

	// data declaration
	EMIT_FORCE_ATTR
		fs << QString("unsigned char __GIX_SYM_PPB[] = {\n");

	if (ppbs_count > 0) {
		// data (all modules)	
		for (CobolModuleMetadata* cmm : mod_list) {
			for (auto ppblk : cmm->getPreprocessedBlocks()) {

				fs << QString("/* %1, %2, %3, %4, %5, %6 %7 %8 %9*/")
					.arg(QString::fromStdString(ppblk->orig_source_file)).arg(QString::fromStdString(ppblk->pp_source_file)).arg(QString::fromStdString(ppblk->module_name))
					.arg(ppblk->orig_start_line).arg(ppblk->orig_end_line).arg(ppblk->pp_start_line).arg(ppblk->pp_end_line)
					.arg((int)ppblk->type).arg(QString::fromStdString(ppblk->command));

				DUMP_STDSTRING(ppblk->module_name); ppbs_size += (ppblk->module_name.length() + 1);

				DUMP_STDSTRING(ppblk->orig_source_file); ppbs_size += (ppblk->orig_source_file.length() + 1);
				DUMP_INT(ppblk->orig_start_line); ppbs_size += (sizeof(uint32_t));
				DUMP_INT(ppblk->orig_end_line); ppbs_size += (sizeof(uint32_t));

				DUMP_STDSTRING(ppblk->pp_source_file); ppbs_size += (ppblk->pp_source_file.length() + 1);
				DUMP_INT(ppblk->pp_start_line); ppbs_size += (sizeof(uint32_t));
				DUMP_INT(ppblk->pp_end_line); ppbs_size += (sizeof(uint32_t));

				DUMP_INT(ppblk->pp_gen_start_line); ppbs_size += (sizeof(uint32_t));
				DUMP_INT(ppblk->pp_gen_end_line); ppbs_size += (sizeof(uint32_t));

				DUMP_INT((int)ppblk->type); ppbs_size += (sizeof(uint32_t));
				DUMP_STDSTRING(ppblk->command); ppbs_size += (ppblk->command.length() + 1);
			}
		}
	}
	else {
		fs << "0";	// we need to put at least one byte in the array, or we'll gt an error when compiling the generated file
	}
	fs << "};\n\n";

	EMIT_FORCE_ATTR
		fs << QString("int __GIX_SYM_PPB_S = %1;\n").arg(ppbs_size);

	// ****************************************************

	cfile.close();

	bool res = compileDebugHelperObj(build_dir, c_filename, obj_filename);
	if (res)
		dbg_helper_obj = obj_filename;

	return res;
}



bool BuildActionLinkHandler::compileDebugHelperObj(QString build_dir, QString c_filename, QString obj_filename)
{
	QString build_configuration = build_driver->getBuildEnvironment()["configuration"].toString();
	QString target_platform = build_driver->getBuildEnvironment()["platform"].toString();
	QString target_type = build_configuration + "/" + target_platform;

	QScopedPointer<CompilerConfiguration> ccfg(CompilerConfiguration::get(build_configuration, target_platform, environment));
	CompilerConfiguration *compiler_cfg = ccfg.data();
	if (compiler_cfg == nullptr) {
		build_driver->log_build_message(QString(tr("Invalid compiler configuration for target ")).arg(target_type), spdlog::level::err, 1);
		return false;
	}

	QString cobc = compiler_cfg->executablePath;
	QProcessEnvironment env = compiler_cfg->getEnvironment(build_driver);
	QStringList cobc_opts;

	cobc_opts.append("-g");
	cobc_opts.append("-c");
	cobc_opts.append(c_filename);
	cobc_opts.append("-o");
	cobc_opts.append(obj_filename);

	QProcess *p = new QProcess();

	p->setTextModeEnabled(false);

	p->setProgram(cobc);

	p->setArguments(cobc_opts);
	p->setProcessEnvironment(env);

	QString output_path = environment["output_path"].toString();
	p->setWorkingDirectory(build_dir);

	//dump_environment(env, build_driver);

	QEventLoop loop;
	bool ps_err = false;

	connect(p, &QProcess::readyReadStandardError, this, [this, p] { readStdErr(p); });
	connect(p, &QProcess::readyReadStandardOutput, this, [this, p] { readStdOut(p); });
	connect(p, &QProcess::errorOccurred, this, [this, &p, ps_err](QProcess::ProcessError _err) mutable { build_driver->log_build_message(p->errorString(), spdlog::level::err); ps_err = true; });

	connect(p, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),
		[this, &loop, p](int exitCode, QProcess::ExitStatus exitStatus) { readStdErr(p); readStdOut(p); loop.quit(); });

	p->start();
	if (!p->waitForStarted()) {
		build_driver->log_build_message("ERROR: " + p->errorString(), spdlog::level::err);
		return false;
	}

	loop.exec();

	int rc = p->exitCode();
	bool res = ((!rc)); // && QFile::exists(target_final_path)
	if (res) {
		//build_driver->log_build_message(tr("Build successful") + ": " + target_final_path, spdlog::level::info);
		return true;
	}
	else {
		build_driver->log_build_message(tr("Build error"), spdlog::level::err);
		build_driver->log_build_message("Exit code: " + QString::number(rc), spdlog::level::trace);
		return false;
	}
}
