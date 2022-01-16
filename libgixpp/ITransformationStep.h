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

#include <string>

#define SET_ERR(I,S) owner->err_data.err_code = I; owner->err_data.err_messages.push_back(S)

class ITransformationStep;
class GixPreProcessor;

class ITransformationStep
{
public:

	virtual ~ITransformationStep() {}

	virtual bool run(ITransformationStep* prev_step) = 0;
	virtual std::string getInput();
	virtual std::string getOutput(ITransformationStep* me = nullptr);

	virtual void setInput(std::string in_file);
	virtual void setOutput(std::string out_file);

	GixPreProcessor *getOwner();


protected:

	ITransformationStep(GixPreProcessor* gpp);

	GixPreProcessor* owner;
	std::string input_file;
	std::string output_file;

};

