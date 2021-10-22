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

#include "TPSourceConsolidation.h"

#include <filesystem>

#include "GixPreProcessor.h"
#include "libcpputils.h"

TPSourceConsolidation::TPSourceConsolidation(GixPreProcessor *gpp) : ITransformationStep(gpp)
{
	nlines = 0;
	cur_output_line = 0;
	owner = gpp;
	map_only = false;
	current_input_line = 0;
	output_line = 0;
}

bool TPSourceConsolidation::run(ITransformationStep *prev_step)
{
	if (input_file.empty() && !prev_step)
		return false;

	if (input_file.empty())
		input_file = prev_step->getOutput();
	
	map_only = std::get<bool>(owner->getOpt("no_output", false));

	if (!map_only) {
		if (output_file.empty()) {
			std::string f = filename_change_ext(input_file, ".cblpp");
			f = std::filesystem::temp_directory_path().string() + PATH_SEPARATOR + std::filesystem::path(f).filename().string();
			output_file = f;
		}

		if (!file_is_writable(output_file))
			return false;
	}

	input_file_stack.push(input_file);
	if (!processNextFile())
		return false;

	if (!map_only) {
		file_write_all_lines(output_file, all_lines);
	}

	return true;
}

std::string TPSourceConsolidation::getOutput(ITransformationStep *me)
{
	return output_file;
}

std::map<int, std::string> &TPSourceConsolidation::getFileMap() const
{
	return const_cast<std::map<int, std::string>&>(filemap);
}

std::map<std::string, std::string> &TPSourceConsolidation::getSrcLineMap() const
{
	return const_cast<std::map<std::string, std::string>&>(in_to_out);
}

bool TPSourceConsolidation::processNextFile()
{
	std::string the_file = input_file_stack.top();
	std::vector<std::string> input_lines = file_read_all_lines(the_file);
	std::string copy_name, copy_file;

	if (!input_lines.size()) {
		input_file_stack.pop();
		return true;
	}

	for (int input_line = 1; input_line <= input_lines.size(); input_line++) {
		current_input_line = input_line;
		//fprintf(stderr, "Processing line %d of file %s\n", input_line, the_file.toUtf8().constData());

		std::string cur_line = input_lines.at(input_line - 1);

		if (is_copy_statement(cur_line, copy_name)) {
			if (!owner->getCopyResolver()->resolveCopyFile(copy_name, copy_file)) {
				SET_ERR(5, string_format("Cannot resolve copy %s", copy_name));
				return false;
			}

			input_file_stack.push(copy_file);
			if (!processNextFile()) {
				SET_ERR(6, string_format("Cannot process file %s", copy_file));
				return false;
			}
		}
		else {
			put_output_line(cur_line);
		}

		current_input_line = input_line;
	}

	input_file_stack.pop();

	return true;
}

void TPSourceConsolidation::put_output_line(const std::string &line)
{
	output_line++;

	std::string output_id =  string_format("%d@%s", output_line, (map_only ? input_file : output_file));	// No precompilation, input file is the same as output file
	std::string input_id = string_format("%d@%s", current_input_line, (input_file_stack.top()));
	in_to_out[input_id] = output_id;

	if (!map_only)
		all_lines.push_back(line);

}


bool TPSourceConsolidation::is_copy_statement(const std::string line, std::string &copy_name)
{
	std::string ln = line;

	if (ln.length() < 7)
		return false;;

	if (ln[6] == '*')
		return false;

	ln = trim_copy(ln.substr(7));

	if (starts_with(ln, "COPY ")) {
		std::string cname = ln.substr(5);
		if (cname[cname.length() - 1] == '.')
			cname = cname.substr(0, cname.length() - 1);

		cname = trim_copy(cname);

		copy_name = cname;
		return true;
	}

	// TODO: handle multi-line EXEC SQL INCLUDE statements
	if (starts_with(ln, "EXEC SQL INCLUDE ") && ln.find_first_of("END-EXEC") != std::string::npos) {
		std::string cname = trim_copy(ln.substr(16));
		cname = trim_copy(cname.substr(0, cname.find(" ")));
		copy_name = cname;
		return true;
	}

	return false;
}
