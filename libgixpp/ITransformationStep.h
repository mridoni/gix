/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
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

#include <QString>

#define SET_ERR(I,S) owner->err_code = I; owner->err_messages << QCoreApplication::translate("gix", S)

class ITransformationStep;
class GixPreProcessor;

class ITransformationStep
{
public:

	virtual bool run(ITransformationStep* prev_step) = 0;
	virtual QString getInput();
	virtual QString getOutput(ITransformationStep* me = nullptr);

	virtual void setInput(QString in_file);
	virtual void setOutput(QString out_file);


protected:

	ITransformationStep(GixPreProcessor* gpp);

	GixPreProcessor* owner;
	QString input_file;
	QString output_file;

};

