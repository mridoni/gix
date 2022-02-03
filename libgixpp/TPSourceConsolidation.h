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

#pragma once

#include "ITransformationStep.h"

#include <string>
#include <vector>
#include <map>
#include <stack>

class TPSourceConsolidation : public ITransformationStep
{
public:
	TPSourceConsolidation(GixPreProcessor* gpp);

	// Inherited via ITransformationStep
	virtual bool run(ITransformationStep* prev_step) override;

	virtual std::string getOutput(ITransformationStep* me = nullptr) override;

	std::map<std::string, std::string> &getSrcLineMap() const;
	std::map<int, std::string> &getFileMap() const;

private:
	std::vector<std::string> all_lines;

	std::map<int, std::string> filemap;
	std::map<std::string, std::string> in_to_out;

	std::stack<std::string> input_file_stack;

	int current_input_line;
	int cur_output_line;
	int nlines;
	int output_line;

	bool map_only;

	bool processNextFile();
	void put_output_line(const std::string &line);
	bool is_copy_statement(const std::string line, std::string &copy_name);
};

