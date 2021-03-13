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

#include <QString>
#include <QList>

//namespace gix::esql {

	enum class Usage
	{
		None,
		Float,
		Double,
		Packed,
		Binary
	};

	typedef QString cb_sql_token_t;

	struct cb_hostreference_t
	{
		QString hostreference;
		int hostno;
		int lineno;
	};

	typedef cb_hostreference_t *cb_hostreference_ptr;

	struct cb_res_hostreference_t
	{
		QString hostreference;
		int lineno;
	};

	typedef cb_res_hostreference_t *cb_res_hostreference_ptr;

	struct cb_exec_sql_stmt_t
	{
		int startLine;
		int endLine;
		int period;

		QList<cb_hostreference_ptr> *host_list;
		QList<cb_res_hostreference_ptr> *res_host_list;
		QList<cb_sql_token_t> *sql_list;

		bool conn_use_other_db;
		QString  dbName;
		QString  cursorName;
		QString  commandName;
		bool command_putother;
		QString  sqlName;
		QString  incfileName;
		bool startup_item;
		bool cursor_hold;

		int sql_query_list_id;
		
		QString src_file;

		cb_exec_sql_stmt_t()
		{
			startLine = 0;
			endLine = 0;
			period = 0;
			sql_query_list_id = 0;

			host_list = new QList<cb_hostreference_ptr>;
			res_host_list = new QList<cb_res_hostreference_ptr>;
			sql_list = new QList<cb_sql_token_t>;

			command_putother = false;
			conn_use_other_db = false;
			startup_item = false;
			cursor_hold = false;
		}

		~cb_exec_sql_stmt_t()
		{
			delete host_list;
			delete res_host_list;
			delete sql_list;
		}
	};

	typedef cb_exec_sql_stmt_t *cb_exec_sql_stmt_ptr;

	struct cb_field_t;

	typedef cb_field_t *cb_field_ptr;

	enum class DataSectionType
	{
		Unknown = 0,
		WorkingStorage,
		LinkageSection,
		FileSection
	};

	struct cb_field_t
	{
		QString sname;
		QString path;
		DataSectionType data_section = DataSectionType::Unknown;
		int		level;
		Usage	usage;
		int		occurs;
		bool	flag_varying;

		cb_field_ptr parent;
		cb_field_ptr children;
		cb_field_ptr sister;

		int pictype;
		int picnsize;
		int scale;
		unsigned char have_sign;
		bool sign_leading;
		bool separate;

		QString defined_at_source_file;
		int defined_at_source_line;
	};


//}