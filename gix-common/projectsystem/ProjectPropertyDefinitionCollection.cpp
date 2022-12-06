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

#include "ProjectPropertyDefinitionCollection.h"
#include "Project.h"


ProjectPropertyDefinitionCollection::ProjectPropertyDefinitionCollection() : PropertyDefinitionCollection::PropertyDefinitionCollection()
{
	property_groups.append(new PropertyGroup("compiler", "Compiler options"));
	property_groups.append(new PropertyGroup("debug", "Debug"));
	property_groups.append(new PropertyGroup("esql", "ESQL"));

	build_type_opts = new QMap<QString, QVariant>();
	build_type_opts->insert("exe", "Executable program");
	build_type_opts->insert("dll", "Dynamically loadable program");

	build_type_opts_module = new QMap<QString, QVariant>();
	build_type_opts_module->insert("exe", "Executable program");
	build_type_opts_module->insert("dll", "Dynamically loadable program");

	compiler_dialect_opts = new QMap<QString, QVariant>();
	compiler_dialect_opts->insert("acu_strict", "ACUCOBOL-GT");
	compiler_dialect_opts->insert("acu", "ACUCOBOL-GT (lax)");
	compiler_dialect_opts->insert("bs2000_strict", "BS2000 COBOL");
	compiler_dialect_opts->insert("bs2000", "BS2000 COBOL (lax)");
	compiler_dialect_opts->insert("cobol2002", "COBOL 2002");
	compiler_dialect_opts->insert("cobol2014", "COBOL 2014");
	compiler_dialect_opts->insert("cobol85", "COBOL 85");
	compiler_dialect_opts->insert("default", "GnuCOBOL");
	compiler_dialect_opts->insert("ibm_strict", "IBM COBOL");
	compiler_dialect_opts->insert("ibm", "IBM COBOL (lax)");
	compiler_dialect_opts->insert("mf_strict", "Micro Focus COBOL");
	compiler_dialect_opts->insert("mf", "Micro Focus COBOL (lax)");
	compiler_dialect_opts->insert("mvs_strict", "MVS/VM COBOL");
	compiler_dialect_opts->insert("mvs", "MVS/VM COBOL (lax)");
	compiler_dialect_opts->insert("rm_strict", "RM-COBOL");
	compiler_dialect_opts->insert("rm", "RM-COBOL (lax)");
	compiler_dialect_opts->insert("xopen", "X/Open COBOL");

	defs.append(new PropertyDefinition("__project_type", "Project type", PropertyType::PropertyTypeText, "", true));

	//defs.append(new PropertyDefinition("build_type", "Build type", PropertyType::PropertyTypeOption, "exe", false, build_type_opts));
	PropertyDefinition *build_type = new PropertyDefinition("build_type", "Build type", PropertyType::PropertyTypeOption, "exe", false, build_type_opts);
	build_type->show_depending_on = [](PropertySource *ps) {
		Project *prj = dynamic_cast<Project *>(ps);
		if (prj) {
				return prj->getType() != ProjectType::Web;
		}
		return true;
	};
	defs.append(build_type);

	defs.append(new PropertyDefinition("compiler_dialect", "Compiler dialect", PropertyType::PropertyTypeOption, "default", false, compiler_dialect_opts));
	defs.append(new PropertyDefinition("copy_ext_list", "COPY file extensions", PropertyType::PropertyTypeText, ".,.cpy,.CPY", false));
	defs.append(new PropertyDefinition("output_path", "Output path", PropertyType::PropertyTypeDirPath, "${prj.basedir}/bin/${configuration}/${platform}", false));
	defs.append(new PropertyDefinition("target_name", "Target name", PropertyType::PropertyTypeText, "", false));
	defs.append(new PropertyDefinition("preprocess_esql", "Preprocess for ESQL", PropertyType::PropertyTypeBoolean, true, false));
	//defs.append(new PropertyDefinition("preprocess_cics", "Preprocess for CICS", PropertyType::PropertyTypeBoolean, true, false));
	defs.append(new PropertyDefinition("copy_include_path", "COPY include path", PropertyType::PropertyTypeDirPathList, "", false));
	
	//defs.append(new PropertyDefinition("keep_temp_files", "Keep temporary output files", PropertyType::PropertyTypeBoolean, false, false));
	PropertyDefinition *keep_temp_files = new PropertyDefinition("keep_temp_files", "Keep temporary files", PropertyType::PropertyTypeBoolean, false, false);
	keep_temp_files->show_depending_on = [](PropertySource *ps) {
		Project *prj = dynamic_cast<Project *>(ps);
		if (prj) {
			return prj->getType() != ProjectType::Web;
		}
		return true;
	};
	defs.append(keep_temp_files);

	defs.append(new PropertyDefinition("dbg_cmd", "Command", PropertyType::PropertyTypeText, "", false, nullptr, "debug"));
	defs.append(new PropertyDefinition("dbg_working_dir", "Working directory", PropertyType::PropertyTypeDirPath, "", false, nullptr, "debug"));
	defs.append(new PropertyDefinition("dbg_args", "Arguments", PropertyType::PropertyTypeText, "", false, nullptr, "debug"));
	defs.append(new PropertyDefinition("dbg_separate_console", "Run in separate console", PropertyType::PropertyTypeBoolean, true, false, nullptr, "debug"));
	defs.append(new PropertyDefinition("dbg_stdin_file", "Input file (STDIN)", PropertyType::PropertyTypeText, "", false, nullptr, "debug"));
	defs.append(new PropertyDefinition("dbg_stop_at_first_line", "Stop at first line", PropertyType::PropertyTypeBoolean, true, false, nullptr, "debug"));
	defs.append(new PropertyDefinition("dbg_run_env_vars", "Run/Debug environment", PropertyType::PropertyTypeEnvVarsList, "", false, nullptr, "debug"));
	defs.append(new PropertyDefinition("dbg_merge_env", "Merge environment", PropertyType::PropertyTypeBoolean, true, false, nullptr, "debug"));

	defs.append(new PropertyDefinition("esql_preprocess_copy_files", "Preprocess COPY files", PropertyType::PropertyTypeBoolean, false, false, nullptr, "esql"));
	defs.append(new PropertyDefinition("esql_consolidate_source", "Preprocess COPY files", PropertyType::PropertyTypeBoolean, false, false, nullptr, "esql"));
	defs.append(new PropertyDefinition("esql_params_style", "Parameter style", PropertyType::PropertyTypeOption, "a", false, new QMap<QString, QVariant>({ { "a", "Anonymous" },{ "d", "Dollar" },{ "c", "Colon" } }), "esql"));
	defs.append(new PropertyDefinition("esql_static_calls", "Emit static calls", PropertyType::PropertyTypeBoolean, true, false, nullptr, "esql"));
	defs.append(new PropertyDefinition("esql_debug_log_on", "Enable debug log", PropertyType::PropertyTypeBoolean, false, false, nullptr, "esql"));
	defs.append(new PropertyDefinition("esql_debug_log_file", "Debug log path", PropertyType::PropertyTypeFilePath, "", false, nullptr, "esql"));
	defs.append(new PropertyDefinition("esql_error_log_file", "Error log path", PropertyType::PropertyTypeFilePath, "", false, nullptr, "esql"));

	defs.append(new PropertyDefinition("cobc_verbose", "Verbosity level", PropertyType::PropertyTypeOption, "", false, new QMap<QString, QVariant>({ { "", "Normal" },{ "-v", "Compiler" },{ "-vv", "Compiler/Assembler" },{ "-vvv", "Compiler/Assembler/Linker" } } ), "compiler"));
	//defs.append(new PropertyDefinition("cobc_default_source_format", "Source format", PropertyType::PropertyTypeOption, "-fixed", false, new QMap<QString, QVariant>({ { "-fixed", "Fixed" },{ "-free", "Free" } } ), "compiler"));
	defs.append(new PropertyDefinition("cobc_optimization", "Optimization", PropertyType::PropertyTypeOption, "", false, new QMap<QString, QVariant>({ { "", "None" },{ "-O", "Level 1" },{ "-O2", "Level 2" },{ "-O3", "Level 3" },{ "-Os", "By size" } } ), "compiler"));
	defs.append(new PropertyDefinition("cobc_generate_listing", "Generate listing", PropertyType::PropertyTypeOption, "", false, new QMap<QString, QVariant>({ { "", "No" },{ "-t", "Standard" },{ "-T", "Wide" } }), "compiler"));
	defs.append(new PropertyDefinition("cobc_symbols_in_listing", "Symbols in listing", PropertyType::PropertyTypeOption, "", false, new QMap<QString, QVariant>({ { "", "No" },{ "--tsymbols", "Yes" } }), "compiler"));
	defs.append(new PropertyDefinition("cobc_c_comp_opts", "Additional C compiler options", PropertyType::PropertyTypeText, "", false, nullptr, "compiler"));
	defs.append(new PropertyDefinition("cobc_c_link_opts", "Additional C linker options", PropertyType::PropertyTypeText, "", false, nullptr, "compiler"));
	defs.append(new PropertyDefinition("cobc_warnings", "Display warnings", PropertyType::PropertyTypeOption, "", false, new QMap<QString, QVariant>({ { "", "Default" },{ "-W", "All" },{ "-Wall", "Most" } }), "compiler"));
}


ProjectPropertyDefinitionCollection::~ProjectPropertyDefinitionCollection()
{
	QList<PropertyDefinition*>::iterator it;

	for (it = defs.begin(); it != defs.end(); ++it) {
		delete (*it);
	}

	delete(build_type_opts);
	delete(compiler_dialect_opts);
}
