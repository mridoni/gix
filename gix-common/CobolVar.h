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

#include "gixcommon_global.h"

#define COBOL_TYPE_UNSIGNED_NUMBER		1         
#define COBOL_TYPE_SIGNED_NUMBER_TC		3        // (trailing combined)
#define COBOL_TYPE_SIGNED_NUMBER_LS		4        // (leading separate)
#define COBOL_TYPE_UNSIGNED_NUMBER_PD	8
#define COBOL_TYPE_SIGNED_NUMBER_PD		9     
#define COBOL_TYPE_ALPHANUMERIC			16
#define COBOL_TYPE_JAPANESE				24
#define COBOL_TYPE_MIN					0 
#define COBOL_TYPE_MAX					29 

#define COBOL_TYPE_UNSIGNED_BINARY		22
#define COBOL_TYPE_SIGNED_BINARY		23


class CobolVar
{
	friend class CobolVarList;

public:
	GIXCOMMON_EXPORT CobolVar();
	GIXCOMMON_EXPORT ~CobolVar();

	GIXCOMMON_EXPORT CobolVar *copy();

	GIXCOMMON_EXPORT char* getRealData();

	void GIXCOMMON_EXPORT setType(int);
	void GIXCOMMON_EXPORT setLength(int);
	void GIXCOMMON_EXPORT setPower(int);
	void GIXCOMMON_EXPORT setAddr(void *);
	void GIXCOMMON_EXPORT setData(void *);
	void GIXCOMMON_EXPORT setRealData(char *);
	void GIXCOMMON_EXPORT createRealData();

	void GIXCOMMON_EXPORT createCobolData(char * retstr);

	void GIXCOMMON_EXPORT createCobolDataLowValue();

	int GIXCOMMON_EXPORT getType();

private:
	int type; // set OCDB_TYPE_*
	int length; // size
	int power; // power
	void *addr; // address of variable
	void *data; // data(for SetSQLParams)
	char *realdata; // realdata

};

