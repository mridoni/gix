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
#include <vector>

#include "ICursor.h"
#include "ILogger.h"
#include "IConnection.h"
#include "IConnectionString.h"
#include "IDbManagerInterface.h"



#define USE_DEFAULT_CONNECTION		-998

#define DBERR_NO_ERROR				0
#define DBERR_CONNECTION_FAILED		-100
#define DBERR_BEGIN_TX_FAILED		-101
#define DBERR_END_TX_FAILED			-102
#define DBERR_CONN_NOT_FOUND		-103
#define DBERR_CONN_RESET_FAILED		-104
#define DBERR_EMPTY_QUERY			-105
#define DBERR_SQL_ERROR				-106
#define DBERR_TOO_MANY_ARGUMENTS	-107
#define DBERR_TOO_FEW_ARGUMENTS		-108
#define DBERR_NO_PARAMETERS			-109
#define DBERR_CURSOR_EXISTS			-110
#define DBERR_NO_SUCH_CURSOR		-111
#define DBERR_CLOSE_CURSOR_FAILED	-112
#define DBERR_DISCONNECT_FAILED		-113
#define DBERR_OUT_OF_MEMORY			-114
#define DBERR_DECLARE_CURSOR_FAILED	-115
#define DBERR_OPEN_CURSOR_FAILED	-116
#define DBERR_FETCH_ROW_FAILED		-117
#define DBERR_INVALID_COLUMN_DATA	-118
#define DBERR_CURSOR_CLOSED			-119
#define DBERR_MOVE_TO_FIRST_FAILED	-120
#define DBERR_FIELD_COUNT_MISMATCH	-121
#define DBERR_NO_DATA				-122
#define DBERR_TOO_MUCH_DATA			-123

#define DBERR_CONN_INIT_ERROR		-201
#define DBERR_CONN_INVALID_DBTYPE	-202

#define FETCH_NEXT_ROW	1
#define FETCH_PREV_ROW	2
#define FETCH_CUR_ROW	3

class IConnectionString;
class IConnection;
class ICursor;

class IDbInterface
{
public:
	virtual int init(ILogger *) = 0;
	virtual int connect(IConnectionString *, int, std::string) = 0;
	virtual int reset() = 0;
	virtual int terminate_connection() = 0;
	virtual int begin_transaction() = 0;
	virtual int end_transaction(std::string) = 0;
	virtual int exec(std::string) = 0;
	virtual int exec_params(std::string, int, int *, std::vector<std::string>&, int *, int *, int) = 0;
	virtual int close_cursor(ICursor *) = 0;
	virtual int cursor_declare(ICursor *, bool, int) = 0;
	virtual int cursor_declare_with_params(ICursor *, char **, bool, int) = 0;
	virtual int cursor_open(ICursor *) = 0;
	virtual int fetch_one(ICursor *, int) = 0;
	virtual bool get_resultset_value(ICursor *, int, int, char* bfr, int bfrlen) = 0;
	virtual int move_to_first_record() = 0;
	virtual int supports_num_rows() = 0;
	virtual int get_num_rows() = 0;
	virtual int get_num_fields() = 0;
	virtual char *get_error_message() = 0;
	virtual int get_error_code() = 0;
	virtual void set_owner(IConnection *) = 0;
	virtual IConnection* get_owner() = 0;

	IDbManagerInterface* manager()
	{
		return dynamic_cast<IDbManagerInterface*>(this);
	}

protected:
	IConnection* owner;
	ILogger* logger;
};

