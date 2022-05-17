/*
* This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
* Copyright (C) 2021 Marco Ridoni
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License
* as published by the Free Software Foundation; either version 3,
* or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; see the file COPYING.LIB.  If
* not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
* Boston, MA 02110-1301 USA
*/

#pragma once

#include <stdint.h>

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

// These must be in sync with the ones in TPESQLProcessing.cpp
#ifdef USE_VARLEN_32
#define VARLEN_LENGTH_PIC		"9(8) COMP-5"
#define VARLEN_PIC_SZ			9
#define VARLEN_LENGTH_SZ		4
#define VARLEN_LENGTH_T			uint32_t
#define VARLEN_BSWAP			COB_BSWAP_32
#else
#define VARLEN_LENGTH_PIC		"9(4) COMP-5"
#define VARLEN_PIC_SZ			4
#define VARLEN_LENGTH_SZ		2
#define VARLEN_LENGTH_T			uint16_t
#define VARLEN_BSWAP			COB_BSWAP_16
#endif

class SqlVar
{
	friend class SqlVarList;

public:
	SqlVar(int _type, int _length, int _power, uint32_t _flags, void *_addr);
	~SqlVar();

	SqlVar *copy();

	void createRealData();

	void* getAddr();
	char *getRealData();
	int getType();
	int getLength();

	void createCobolData(char *retstr, int datalen);

	void createCobolDataLowValue();


private:
	int type; // set OCDB_TYPE_*
	int length; // includes the extra 2 bytes for variable length fields (level 49)
	int power; // power
	void *addr = nullptr; // address of variable
	char *realdata = nullptr; // realdata
	unsigned int realdata_len = 0; // length of realdata (actual length of allocated buffer is always realdata_len + 1)

	// Variable length (level 49) support
	bool is_variable_length = false;
	
	// Binary/VarBinary support
	bool is_binary = false;

    static const char _decimal_point;

	void display_to_comp3(const char *data, bool has_sign);	// , int total_len, int scale, int has_sign, uint8_t *addr
	char *allocate_realdata_buffer();
};

