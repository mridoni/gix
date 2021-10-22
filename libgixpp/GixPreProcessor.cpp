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

#include "GixPreProcessor.h"

#include <string>

#include "FileData.h"
#include "libcpputils.h"
#include "TPESQLProcessing.h"


#define SET_ERR(I,S) err_data.err_code = I; err_data.err_messages.push_back(S)

GixPreProcessor::GixPreProcessor()
{
	check_update_status = true;
	keep_temp_files = false;
	verbose = false;
	verbose_debug = false;

	err_data.err_code = 0;
}

GixPreProcessor::~GixPreProcessor()
{
	for (auto step : steps) {
		if (step)
			delete step;
	}
}

//const std::stringList GixPreProcessor::getCopyDirs()
//{
//	return copy_dirs;
//}

//void GixPreProcessor::setCopyDirs(std::string cdl)
//{
//	copy_dirs = cdl.string_split(QDir::listSeparator());
//}

void GixPreProcessor::setCopyResolver(const CopyResolver *cr)
{
	copy_resolver = const_cast<CopyResolver *>(cr);
}

CopyResolver *GixPreProcessor::getCopyResolver() const
{
	return copy_resolver;
}

//void GixPreProcessor::setCopyDirs(const std::stringList cdl)
//{
//	copy_dirs = cdl;
//}


void GixPreProcessor::addCustomStep(ITransformationStep *stp)
{
	this->addStep(stp);
}

bool GixPreProcessor::process()
{
    if (input_file.empty()) {
        SET_ERR(1, "Bad input file");
        return false;
    }

    if (!std::get<bool>(getOpt("no_output")) && output_file.empty()) {
        SET_ERR(2, "Bad output file");
        return false;
    }

    if (!file_exists(input_file)) {
        SET_ERR(4, "Input file does not exist");
        return false;
    }

	bool b = this->transform();

	return b;
}

bool GixPreProcessor::transform()
{
	ITransformationStep *prev_step = nullptr;
	for (ITransformationStep *step : this->steps) {
		if (!step->run(prev_step)) {
			return false;
		}

		prev_step = step;
	}

	return true;
}

void GixPreProcessor::addStep(ITransformationStep *s)
{
	steps.push_back(s);
}

bool GixPreProcessor::setInputFile(std::string infile)
{
	if (!steps.size()) {
		input_file = std::string();
		return false;
	}

	steps.at(0)->setInput(infile);
	input_file = infile;

	return true;
}

bool GixPreProcessor::setOutputFile(std::string outfile)
{
	if (!steps.size()) {
		output_file = std::string();
		return false;
	}

	steps.back()->setOutput(outfile);
	output_file = outfile;

	return true;
}

variant GixPreProcessor::getOpt(std::string id, bool b)
{	
	return map_contains<std::string, variant>(opts, id) ? opts[id] : variant(b);
}

variant_map& GixPreProcessor::getOpts() const
{
	return const_cast<variant_map &>(opts);
}

void GixPreProcessor::setOpt(std::string id, variant v)
{
	opts[id] = v;
}
