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

#include <map>
#include <vector>
#include <string>

#include "popl.hpp"

#include "libgixpp.h"
#include "GixPreProcessor.h"
#include "TPSourceConsolidation.h"
#include "TPESQLProcessing.h"
#include "libcpputils.h"


#ifdef _WIN32
#define PATH_LIST_SEP ";"
#else
#define PATH_LIST_SEP ":"
#endif

#define GIXPP_VER "1.0.11"

using namespace popl;

int main(int argc, char **argv)
{
	int rc = -1;

	GixPreProcessor gp;
	CopyResolver copy_resolver;


	// Do processing here
	const auto args = argv;

	char vbfr[1024];
	sprintf(vbfr, "gixpp - the ESQL preprocessor for Gix-IDE/GixSQL\nVersion: %s\nlibgixpp version: %s\n\nOptions", GIXPP_VER, LIBGIXPP_VER);

	OptionParser options(vbfr);

	auto opt_help = options.add<Switch>("h", "help", "displays help on commandline options");
	auto opt_version = options.add<Switch>("V", "version", "displays version information");
	auto opt_copypath = options.add<Value<std::string>>("I", "copypath", "COPY file path list");

	auto opt_infile = options.add<Value<std::string>>("i", "infile", "input file");
	auto opt_outfile = options.add<Value<std::string>>("o", "outfile", "output file");
	auto opt_symfile = options.add<Value<std::string>>("s", "symfile", "output symbol file");
	auto opt_esql = options.add<Switch>("e", "esql", "preprocess for ESQL");
	auto opt_esql_preprocess_copy = options.add<Switch>("p", "esql-preprocess-copy", "ESQL: preprocess all included COPY files");
	auto opt_esql_copy_exts = options.add<Value<std::string>>("E", "esql-copy-exts", "ESQL: copy files extension list (comma-separated)");
	auto opt_esql_anon_params = options.add<Switch>("a", "esql-anon-params", "ESQL: use anonymous (not numbered) parameters");
	auto opt_esql_static_calls = options.add<Switch>("S", "esql-static-calls", "ESQL: emit static calls");
	auto opt_debug_info = options.add<Switch>("g", "debug-info", "generate debug info");
	auto opt_consolidate = options.add<Switch>("c", "consolidate", "consolidate source to single-file");
	auto opt_keep = options.add<Switch>("k", "keep", "keep temporary files");
	auto opt_verbose = options.add<Switch>("v", "verbose", "verbose");
	auto opt_verbose_debug = options.add<Switch>("d", "verbose-debug", "verbose (debug)");
	auto opt_emit_map_file = options.add<Switch>("m", "map", "emit map file");

	options.parse(argc, argv);

	if (opt_help->is_set()) {
		rc = 0;
		std::cout << options << std::endl;
	}
	else {
		if (opt_version->is_set()) {
			sprintf(vbfr, "gixpp - the ESQL preprocessor for Gix-IDE/GixSQL\nVersion: %s\nlibgixpp version: %s\n", GIXPP_VER, LIBGIXPP_VER);
			std::cout << vbfr << std::endl;
		}
		else {

			if (!opt_consolidate->is_set() && !opt_esql->is_set()) {
				std::cout << options << std::endl;
				fprintf(stderr, "ERROR: please enter at least one of the -e or -c options\n");
				return 1;
			}

			if (!opt_infile->is_set() || !opt_outfile->is_set()) {
				std::cout << options << std::endl;
				fprintf(stderr, "ERROR: please enter at least the input and output file parameters\n");
				return 1;
			}

			copy_resolver.setVerbose(opt_verbose->is_set());

			if (opt_copypath->is_set()) {
				for (int i = 0; i < opt_copypath->count(); i++) {
					std::vector<std::string> copy_dirs = string_split(opt_copypath->value(i), PATH_LIST_SEP);
					if (copy_dirs.size() && !copy_dirs.at(0).empty()) {
						copy_resolver.addCopyDirs(copy_dirs);
					}
				}
			}

			gp.setCopyResolver(&copy_resolver);

			if (opt_consolidate->is_set())
				gp.addStep(new TPSourceConsolidation(&gp));

			if (opt_esql->is_set()) {
				gp.setOpt("emit_static_calls", opt_esql_static_calls->is_set());
				gp.setOpt("anonymous_params", opt_esql_anon_params->is_set());
				gp.setOpt("preprocess_copy_files", opt_esql_preprocess_copy->is_set());
				gp.setOpt("consolidated_map", true);
				gp.setOpt("emit_map_file", opt_emit_map_file->is_set());
				gp.addStep(new TPESQLProcessing(&gp));
				if (opt_esql_copy_exts->is_set())
					copy_resolver.setExtensions(string_split(opt_esql_copy_exts->value(), ","));
			}

			gp.setOpt("emit_debug_info", opt_debug_info->is_set());
			gp.verbose = opt_verbose->is_set();
			gp.verbose_debug = opt_verbose_debug->is_set();


			gp.setInputFile(opt_infile->value(0));
			gp.setOutputFile(opt_outfile->value(0));

			bool b = gp.process();
			if (!b) {
				rc = gp.err_data.err_code;
				for (std::string m : gp.err_data.err_messages)
					fprintf(stderr, "%s\n", m.c_str());
			}

			for (std::string w : gp.err_data.warnings)
				fprintf(stderr, "%s\n", w.c_str());

			rc = gp.err_data.err_code;

		}

		return rc;
	}

}
