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

#include "TPESQLProcessing.h"
#include "PathUtils.h"
#include "SysUtils.h"
#include "ESQLCall.h"
#include "gix_esql_driver.hh"
#include "MapFileWriter.h"

#include "linq/linq.hpp"

#include <QFile>
#include <QDir>
#include <QSet>
#include <QPair>
#if defined(_WIN32) && defined(_DEBUG)
#include <Windows.h>
#endif


#define ESQL_CONNECT				"CONNECT"
#define ESQL_CONNECT_TO				"CONNECT_TO"
#define ESQL_CLOSE					"CLOSE"
#define ESQL_COMMIT					"COMMIT"
#define ESQL_FETCH					"FETCH"
#define ESQL_INCFILE				"INCFILE"
#define ESQL_INCSQLCA				"INCSQLCA"
#define ESQL_INSERT					"INSERT"
#define ESQL_OPEN					"OPEN"
#define ESQL_SELECT					"SELECT"
#define ESQL_UPDATE					"UPDATE"
#define ESQL_DELETE					"DELETE"
#define ESQL_WORKING_BEGIN			"WORKING_BEGIN"
#define ESQL_WORKING_END			"WORKING_END"
#define ESQL_FILE_BEGIN				"FILE_BEGIN"
#define ESQL_FILE_END				"FILE_END"
#define ESQL_LINKAGE_BEGIN			"LINKAGE_BEGIN"
#define ESQL_LINKAGE_END			"LINKAGE_END"
#define ESQL_PROCEDURE_DIVISION		"PROCEDURE_DIVISION"
#define ESQL_DECLARE_TABLE			"DECLARE_TABLE"

#define AREA_B_PREFIX       "           " // 11 spaces
#define AREA_B_CPREFIX      "GIXSQL     " // comment + 5 spaces

#define PIC_ALPHABETIC 		0x01
#define PIC_NUMERIC 		0x02
#define PIC_NATIONAL		0x04
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)

#define CBL_FIELD_FLAG_NONE		(uint32_t)0x0
#define CBL_FIELD_FLAG_VARLEN	(uint32_t)0x80

#define MAP_FILE_FMT_VER ((uint16_t) 0x0100)
#define FLAG_M_BASE					0

#define HVARTYPE_UNSIGNED_NUMERIC 1
#define HVARTYPE_SIGNED_TRAILING_SEPARATE 2
#define HVARTYPE_SIGNED_TRAILING_COMBINED 3
#define HVARTYPE_SIGNED_LEADING_SEPARATE 4
#define HVARTYPE_SIGNED_LEADING_COMBINED 5

#define HVARTYPE_UNSIGNED_PACKED 8
#define HVARTYPE_SIGNED_PACKED 9

#define HVARTYPE_UNSIGNED_BINARY 22
#define HVARTYPE_SIGNED_BINARY 23

#define HVARTYPE_ALPHABETIC 16

#define HVARTYPE_GROUP 22
#define HVARTYPE_FLOAT 23
#define HVARTYPE_NATIONAL 24

#define ERR_NOTDEF_CONVERSION -1

enum oc_usage
{
	USAGE_NONE,
	USAGE_FLOAT,
	USAGE_DOUBLE,
	USAGE_PACKED,
	USAGE_BINARY
};

enum class ESQL_Command
{
	Connect,
	ConnectTo,
	Close,
	Commit,
	Fetch,
	Incfile,
	IncSQLCA,
	Insert,
	Open,
	Select,
	Update,
	Delete,
	WorkingBegin,
	WorkingEnd,
	FileBegin,
	FileEnd,
	LinkageBegin,
	LinkageEnd,
	ProcedureDivision,
	DeclareTable,

	// Helpers
	StartSQL,
	EndSQL,

	Unknown
};



static QMap<QString, ESQL_Command> ESQL_cmd_map{ { ESQL_CONNECT, ESQL_Command::Connect }, { ESQL_CONNECT_TO, ESQL_Command::ConnectTo },{ ESQL_CLOSE, ESQL_Command::Close },
												 { ESQL_COMMIT, ESQL_Command::Commit }, { ESQL_FETCH, ESQL_Command::Fetch },{ ESQL_DELETE, ESQL_Command::Delete },
												 { ESQL_INCFILE, ESQL_Command::Incfile }, { ESQL_INCSQLCA, ESQL_Command::IncSQLCA }, { ESQL_INSERT, ESQL_Command::Insert },
												 { ESQL_OPEN, ESQL_Command::Open }, { ESQL_SELECT, ESQL_Command::Select }, { ESQL_UPDATE, ESQL_Command::Update },
												 { ESQL_WORKING_BEGIN, ESQL_Command::WorkingBegin }, { ESQL_WORKING_END, ESQL_Command::WorkingEnd } ,
												 { ESQL_LINKAGE_BEGIN, ESQL_Command::LinkageBegin }, { ESQL_LINKAGE_END, ESQL_Command::WorkingEnd } ,
												 { ESQL_FILE_BEGIN, ESQL_Command::FileBegin }, { ESQL_FILE_END, ESQL_Command::FileEnd } ,
												 { ESQL_PROCEDURE_DIVISION, ESQL_Command::ProcedureDivision }, { ESQL_DECLARE_TABLE, ESQL_Command::DeclareTable } };

#define CALL_PREFIX	"GIXSQL"
#define TAG_PREFIX	"GIXSQL"

inline QString TPESQLProcessing::get_call_id(const QString s)
{
	return CALL_PREFIX + s;
}

TPESQLProcessing::TPESQLProcessing(GixPreProcessor *gpp) : ITransformationStep(gpp)
{
	opt_anonymous_params = gpp->getOpt("anonymous_params").toBool();
	opt_preprocess_copy_files = gpp->getOpt("preprocess_copy_files").toBool();
	opt_emit_static_calls = gpp->getOpt("emit_static_calls").toBool();
	opt_emit_debug_info = gpp->getOpt("emit_debug_info").toBool();
	opt_emit_compat = gpp->getOpt("emit_compat").toBool();
	opt_consolidated_map = gpp->getOpt("consolidated_map").toBool();
	opt_no_output = gpp->getOpt("no_output").toBool();

	output_line = 0;
	working_begin_line = 0;
	working_end_line = 0;
	current_input_line = 0;
}

bool TPESQLProcessing::run(ITransformationStep *prev_step)
{
	if (input_file.isEmpty()) {
		if (!prev_step || prev_step->getOutput().isEmpty())
			return false;

		input_file = prev_step->getOutput();
	}

	main_module_driver.opt_use_anonymous_params = opt_anonymous_params;
	main_module_driver.opt_preprocess_copy_files = opt_preprocess_copy_files;

	code_tag = TAG_PREFIX;

	int rc = main_module_driver.parse(owner, input_file);
	if (!rc) {
		rc = outputESQL();
	}

	owner->err_code = rc;

	return rc == 0;
}

QString TPESQLProcessing::getOutput(ITransformationStep *me)
{
	return output_file;
}

int TPESQLProcessing::outputESQL()
{
	working_begin_line = 0;
	working_end_line = 0;


	if (output_file.isEmpty()) {
		QString f = PathUtils::changeExtension(input_file, ".cbsql");
		//f = QDir::tempPath() + QDir::separator() + PathUtils::getFilename(f);
		output_file = "#" + PathUtils::getFilename(f);
	}

	if (!output_file.startsWith("#") && !SysUtils::isWritableFile(output_file))
		return -1;

	input_file_stack.push(QDir::cleanPath(input_file));

	if (!find_working_storage(&working_begin_line, &working_end_line))
		return -1;

	startup_items = QList<cb_exec_sql_stmt_ptr>::fromStdList(cpplinq::from(*(main_module_driver.exec_list)).where([](cb_exec_sql_stmt_ptr p) { return p->startup_item != 0; }).to_list());
	process_sql_query_list();

#if defined(_WIN32) && defined(_DEBUG)
	QList<cb_exec_sql_stmt_ptr> *p = main_module_driver.exec_list;
	for (auto e : *p) {
		char bfr[1024];
		sprintf(bfr, "%04d-%04d : %s\n", e->startLine, e->endLine, e->commandName.toLocal8Bit().constData());
		OutputDebugStringA(bfr);
	}
#endif

	if (!processNextFile())
		return 1;

	bool b1 = opt_no_output ? true : SysUtils::FileWriteAllLines(output_file, output_lines);
	bool b2 = opt_no_output ? build_map_data() : write_map_file(output_file);

	return (b1 && b2) ? 0 : 1;
}


bool TPESQLProcessing::generate_consolidated_map()
{
	output_line = 0;
	working_begin_line = 0;
	working_end_line = 0;
	working_begin_line = 0;
	working_end_line = 0;
	current_input_line = 0;
	in_to_out.clear();
	out_to_in.clear();
	input_file_stack.clear();
	input_file_stack.push(QDir::cleanPath(input_file));

	opt_preprocess_copy_files = true;
	main_module_driver.opt_preprocess_copy_files = true;

	int rc = main_module_driver.parse(owner, input_file);

	return processNextFile();
}


bool TPESQLProcessing::processNextFile()
{
	QString the_file = input_file_stack.top();
	QStringList input_lines = SysUtils::FileReadAllLines(the_file);

	if (!input_lines.size()) {
		input_file_stack.pop();
		if (input_file_stack.size() > 0)
			main_module_driver.file = input_file_stack.top().toStdString();

		return true;
	}

	for (int input_line = 1; input_line <= input_lines.size(); input_line++) {
		current_input_line = input_line;
#if defined(_WIN32) && defined(_DEBUG)
		//char bfr[256];
		//sprintf(bfr, "Processing line %d of file %s\n", input_line, the_file.toUtf8().constData());
		//OutputDebugStringA(bfr);
#endif
		QString cur_line = input_lines.at(input_line - 1);

		bool in_ws = (input_line >= working_begin_line) && (input_line <= working_end_line);

		cb_exec_sql_stmt_ptr exec_sql_stmt = find_exec_sql_stmt(the_file, input_line);
		if (!exec_sql_stmt) {
			put_output_line(cur_line);
			continue;
		}

		QString cmdname = exec_sql_stmt->commandName;
		ESQL_Command cmd = ESQL_cmd_map.contains(cmdname) ? ESQL_cmd_map[cmdname] : ESQL_Command::Unknown;

		switch (cmd) {

			case ESQL_Command::WorkingBegin:
			case ESQL_Command::LinkageBegin:
			case ESQL_Command::LinkageEnd:
			case ESQL_Command::FileBegin:
			case ESQL_Command::FileEnd:
				put_output_line(input_lines.at(exec_sql_stmt->startLine - 1));
				break;

			//case ESQL_Command::WorkingEnd:
			//	put_query_defs();
			//	break;

			case ESQL_Command::ProcedureDivision:
				put_output_line(input_lines.at(exec_sql_stmt->startLine - 1));

				if (!put_cursor_declarations()) {
					owner->err_messages << QString("An error eccurred while generating ESQL cursor declarations at line %1 of file %2: %3").arg(input_line).arg(input_file).arg(cur_line);
					return false;
				}
				break;

			default:
				// Add original text, commented
				for (int n = exec_sql_stmt->startLine; n <= exec_sql_stmt->endLine; n++) {
					put_output_line(comment_line("GIXSQL", input_lines.at(n - 1)));
				}

				// Add ESQL calls
				if (!handle_esql_stmt(cmd, exec_sql_stmt, in_ws)) {
					owner->err_messages << QString("Unsupported ESQL statement at line %1 of file %2: %3").arg(input_line).arg(input_file).arg(cur_line);
					return false;
				}
		}

		// Special case
		if (exec_sql_stmt->endLine == working_end_line) {
			if (!handle_esql_stmt(ESQL_Command::WorkingEnd, find_esql_cmd(ESQL_WORKING_END, 0), 0)) {
				owner->err_messages << QString("Unsupported ESQL statement at line %1 of file %2: %3").arg(input_line).arg(input_file).arg(cur_line);
				return false;
			}
		}

		// Update input line pointer
		input_line = exec_sql_stmt->endLine;
		current_input_line = input_line;
	}

	input_file_stack.pop();
	if (input_file_stack.size() > 0)
		main_module_driver.file = input_file_stack.top().toStdString();

	return true;
}

void TPESQLProcessing::put_start_exec_sql(bool with_period)
{
	ESQLCall start_exec_sql_call(get_call_id("StartSQL"), opt_emit_static_calls);
	put_call(start_exec_sql_call, with_period);
}

void TPESQLProcessing::put_end_exec_sql(bool with_period)
{
	ESQLCall start_exec_sql_call(get_call_id("EndSQL"), opt_emit_static_calls);
	put_call(start_exec_sql_call, with_period);
}

QString take_max(QString &s, int n)
{
	QString res;
	if (s.length() > n) {
		res = s.mid(0, n);
		s = s.mid(n);
	}
	else {
		res = s;
		s = "";
	}
	return res;
}

void TPESQLProcessing::put_query_defs()
{
	if (emitted_query_defs)
		return;

	for (int i = 1; i <= ws_query_list.size(); i++) {
		QString qry = ws_query_list.at(i - 1);
		int qry_len = qry.length();
		qry = qry.replace("\"", "\"\"");
		put_output_line(code_tag + QString(" 01  SQ%1.").arg(i, 4, 10, QLatin1Char('0')));

		int pos = 0;
		int max_sec_len = 30;

		QString s;

		s = take_max(qry, 30);
		put_output_line(code_tag + QString("     02  FILLER PIC X(%1) VALUE \"%2\"").arg(qry_len).arg(s));

		while (true) {
			s = take_max(qry, 58);
			if (s.isEmpty())
				break;

			put_output_line(code_tag + QString("  &  \"%1\"").arg(s));
		}
		output_lines.last() += ".";

		put_output_line(code_tag + QString("     02  FILLER PIC X(1) VALUE X\"00\"."));
	}

	emitted_query_defs = true;
}

void TPESQLProcessing::put_procedure_division()
{
	put_output_line("      " + QString(" PROCEDURE DIVISION."));
}

void TPESQLProcessing::put_working_storage()
{
	put_output_line(code_tag + QString(" WORKING-STORAGE SECTION."));
}

bool TPESQLProcessing::put_cursor_declarations()
{
	int f_type, f_size, f_scale, f_flags;
	bool emit_static = opt_emit_static_calls;

	if (!startup_items.size())
		return true;

	put_output_line(code_tag + "*   ESQL STARTUP DECLARATIONS (START)");

	for (cb_exec_sql_stmt_ptr stmt : startup_items) {
		bool has_params = stmt->host_list->size() > 0;

		if (has_params) {
			put_start_exec_sql(stmt->period);

			for (cb_hostreference_ptr p : *stmt->host_list) {
				ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);
				cb_field_ptr hr = main_module_driver.field_map[p->hostreference.mid(1)];
				if (!hr)
					return false;

				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				p_call.addParameter(f_type, BY_VALUE);
				p_call.addParameter(f_size, BY_VALUE);
				p_call.addParameter(f_scale, BY_VALUE);
				p_call.addParameter(is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE, BY_VALUE);
				p_call.addParameter(p->hostreference.mid(1), BY_REFERENCE);

				put_call(p_call, stmt->period);
			}

			ESQLCall cd_call(get_call_id("CursorDeclareParams"), emit_static);
			cd_call.addParameter("SQLCA", BY_REFERENCE);
			cd_call.addParameter("\"" + stmt->cursorName + "\" & x\"00\"", BY_REFERENCE); //& x\"00\"
			cd_call.addParameter(QString::number(stmt->cursor_hold ? 1 : 0), BY_VALUE);
			cd_call.addParameter(stmt->sqlName, BY_REFERENCE); //& x\"00\"
			cd_call.addParameter(QString::number(stmt->host_list->size()), BY_VALUE);

			put_call(cd_call, stmt->period);

			put_end_exec_sql(stmt->period);
		}
		else { // (struct sqlca_t *st, char *cname, int with_hold, char *_query)
			ESQLCall cd_call(get_call_id("CursorDeclare"), emit_static);
			cd_call.addParameter("SQLCA", BY_REFERENCE);
			cd_call.addParameter("\"" + stmt->cursorName + "\" & x\"00\"", BY_REFERENCE);
			cd_call.addParameter(stmt->cursor_hold, BY_VALUE);
			cd_call.addParameter(QString("SQ%1").arg(stmt->sql_query_list_id, 4, 10, QLatin1Char('0')), BY_REFERENCE);

			put_call(cd_call, stmt->period);
		}
	}

	put_output_line(code_tag + "*   ESQL STARTUP DECLARATIONS (END)");
	return true;
}

void TPESQLProcessing::put_call(const ESQLCall &c, bool terminate_with_period)
{
	auto lines = c.format();

	if (terminate_with_period) {
		lines.last() = lines.last() + ".";
	}

	for (auto ln : lines) {
		put_output_line(ln);
	}
}

cb_exec_sql_stmt_ptr TPESQLProcessing::find_exec_sql_stmt(const QString filename, int i)
{
	QList<cb_exec_sql_stmt_ptr> *p = main_module_driver.exec_list;
	for (auto e : *p) {
		if (e->src_file == filename && (e->startLine <= i && e->endLine >= i))
			return e;
	}

	return NULL;
}

cb_exec_sql_stmt_ptr TPESQLProcessing::find_esql_cmd(QString cmd, int idx)
{
	int n = 0;
	QList<cb_exec_sql_stmt_ptr> *p = main_module_driver.exec_list;
	for (auto e : *p) {
		if (e->commandName == cmd) {
			if (n == idx)
				return e;
			else
				n++;
		}
	}

	return NULL;
}

void TPESQLProcessing::put_output_line(const QString &line)
{
	output_line++;
	output_lines.append(line);

	QString output_id = QString("%1@%2").arg(output_line).arg(output_file);
	QString input_id = QString("%1@%2").arg(current_input_line).arg(input_file_stack.top());
#if defined(_WIN32) && defined(_DEBUG)
	OutputDebugStringA((input_id + "-> " + output_id + "\n").toLocal8Bit().constData());
#endif
	out_to_in[output_id] = input_id;
	in_to_out[input_id] = output_id;
}

bool TPESQLProcessing::handle_esql_stmt(const ESQL_Command cmd, const cb_exec_sql_stmt_ptr stmt, bool in_ws)
{
	int f_type, f_size, f_scale, f_flags;
	bool emit_static = opt_emit_static_calls;

	if (stmt->startup_item)
		return true;

	switch (cmd) {

		case ESQL_Command::WorkingBegin:
			put_working_storage();
			break;

		case ESQL_Command::DeclareTable:
			// Do nothing
			break;

		case ESQL_Command::WorkingEnd:
			put_query_defs();
			break;

		case ESQL_Command::Incfile:
			if (opt_preprocess_copy_files) {
				// inline file
				QString copy_file;
				if (!owner->getCopyResolver()->resolveCopyFile(stmt->incfileName, copy_file)) {
					owner->err_messages << "Cannot resolve copybook: " + stmt->incfileName;
					return false;
				}

				add_dependency(input_file_stack.top(), copy_file);

				input_file_stack.push(QDir::cleanPath(copy_file));
				if (!processNextFile())
					return false;
			}
			else {
				QString copy_file;
				// we treat it as a standard copybook file, and let the compiler deal with any error
				// but we still try to resolve the copy to gather some metadata
				if (owner->getCopyResolver()->resolveCopyFile(stmt->incfileName, copy_file)) {
					add_dependency(input_file_stack.top(), copy_file);
				}
				else
					add_dependency(input_file_stack.top(), "*" + stmt->incfileName);

				put_output_line(AREA_B_PREFIX + QString("COPY %1.").arg(stmt->incfileName));
			}
			break;


		case ESQL_Command::IncSQLCA:
			if (opt_preprocess_copy_files) {
				// inline file
				QString copy_file;
				if (!owner->getCopyResolver()->resolveCopyFile("SQLCA", copy_file)) {
					owner->err_messages << "Cannot resolve copybook: SQLCA";
					return false;
				}

				add_dependency(input_file_stack.top(), copy_file);

				input_file_stack.push(QDir::cleanPath(copy_file));
				if (!processNextFile())
					return false;
			}
			else {
				QString copy_file;
				// we treat it as a standard copybook file, and let the compiler deal with any error
				put_output_line(AREA_B_PREFIX + QString("COPY SQLCA."));

				if (owner->getCopyResolver()->resolveCopyFile("SQLCA", copy_file)) {
					add_dependency(input_file_stack.top(), copy_file);
				}
				else
					add_dependency(input_file_stack.top(), "*SQLCA");
			}
			break;

		case ESQL_Command::Connect:
		case ESQL_Command::ConnectTo:
		{
			ESQLCall connect_call(get_call_id("Connect"), emit_static);
			connect_call.addParameter("SQLCA", BY_REFERENCE);

			if (main_module_driver.field_map.find(stmt->host_list->at(0)->hostreference.mid(1)) == main_module_driver.field_map.end()) {
				owner->err_messages << "Cannot find host variable: " + stmt->host_list->at(0)->hostreference.mid(1);
				return false;
			}
			connect_call.addParameter(stmt->host_list->at(0)->hostreference.mid(1), BY_REFERENCE);
			connect_call.addParameter(main_module_driver.field_map[stmt->host_list->at(0)->hostreference.mid(1)]->picnsize, BY_VALUE);

			if (main_module_driver.field_map.find(stmt->host_list->at(1)->hostreference.mid(1)) == main_module_driver.field_map.end()) {
				owner->err_messages << "Cannot find host variable: " + stmt->host_list->at(1)->hostreference.mid(1);
				return false;
			}
			connect_call.addParameter(stmt->host_list->at(1)->hostreference.mid(1), BY_REFERENCE);
			connect_call.addParameter(main_module_driver.field_map[stmt->host_list->at(1)->hostreference.mid(1)]->picnsize, BY_VALUE);

			if (main_module_driver.field_map.find(stmt->host_list->at(2)->hostreference.mid(1)) == main_module_driver.field_map.end()) {
				owner->err_messages << "Cannot find host variable: " + stmt->host_list->at(2)->hostreference.mid(1);
				return false;
			}
			connect_call.addParameter(stmt->host_list->at(2)->hostreference.mid(1), BY_REFERENCE);
			connect_call.addParameter(main_module_driver.field_map[stmt->host_list->at(2)->hostreference.mid(1)]->picnsize, BY_VALUE);

			put_call(connect_call, stmt->period);
		}
		break;


		case ESQL_Command::Select:
		{
			/*
				0   RESULT parameters -> CursorDeclare(Params) or Exec(Params).
										 (Params) depends on having input params
										 CursorDeclare/Exec depends on stmt->cursorName being in use
				>0 RESULT parameters  -> SelectInto
			*/

			put_start_exec_sql(stmt->period);
			for (cb_res_hostreference_ptr rp : *stmt->res_host_list) {
				ESQLCall rp_call(get_call_id("SetResultParams"), emit_static);
				cb_field_ptr hr = main_module_driver.field_map[rp->hostreference.mid(1)];

				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				rp_call.addParameter(f_type, BY_VALUE);
				rp_call.addParameter(f_size, BY_VALUE);
				rp_call.addParameter(f_scale, BY_VALUE);
				rp_call.addParameter(is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE, BY_VALUE);
				rp_call.addParameter(rp->hostreference.mid(1), BY_REFERENCE);

				put_call(rp_call, stmt->period);
			}

			for (cb_hostreference_ptr p : *stmt->host_list) {
				ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);

				if (main_module_driver.field_map.find(p->hostreference.mid(1)) == main_module_driver.field_map.end()) {
					owner->err_messages << "Cannot find host variable: " + p->hostreference.mid(1);
					return false;
				}

				cb_field_ptr hr = main_module_driver.field_map[p->hostreference.mid(1)];

				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				p_call.addParameter(f_type, BY_VALUE);
				p_call.addParameter(f_size, BY_VALUE);
				p_call.addParameter(f_scale, BY_VALUE);
				p_call.addParameter(is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE, BY_VALUE);
				p_call.addParameter(p->hostreference.mid(1), BY_REFERENCE);

				put_call(p_call, stmt->period);
			}

			QString call_id;
			if (!stmt->res_host_list->size()) {
				call_id = get_call_id(stmt->cursorName.isEmpty() ? "Exec" : "CursorDeclare");
				if (stmt->host_list->size())
					call_id += "Params";
			}
			else {
				call_id = get_call_id("ExecSelectIntoOne");
			}

			if (stmt->cursorName.isEmpty()) {
				ESQLCall select_call(call_id, emit_static);
				select_call.addParameter("SQLCA", BY_REFERENCE);
				select_call.addParameter(QString("SQ%1").arg(stmt->sql_query_list_id, 4, 10, QLatin1Char('0')), BY_REFERENCE);
				select_call.addParameter(stmt->host_list->size(), BY_VALUE);
				select_call.addParameter(stmt->res_host_list->size(), BY_VALUE);
				put_call(select_call, stmt->period);
			}
			else {
				ESQLCall select_call(call_id, emit_static);
				select_call.addParameter("SQLCA", BY_REFERENCE);
				select_call.addParameter("\"" + stmt->cursorName + "\" & x\"00\"", BY_REFERENCE);
				select_call.addParameter(stmt->cursor_hold, BY_VALUE);
				select_call.addParameter(QString("SQ%1").arg(stmt->sql_query_list_id, 4, 10, QLatin1Char('0')), BY_REFERENCE);
				put_call(select_call, stmt->period);
			}
			put_end_exec_sql(stmt->period);
		}
		break;

		case ESQL_Command::Open:
		{
			QString cursor_id = stmt->cursorName;
			ESQLCall open_call(get_call_id("CursorOpen"), emit_static);
			open_call.addParameter("SQLCA", BY_REFERENCE);
			open_call.addParameter("\"" + cursor_id + "\" & x\"00\"", BY_REFERENCE);
			put_call(open_call, stmt->period);
		}
		break;

		case ESQL_Command::Close:
		{
			QString cursor_id = stmt->cursorName;
			ESQLCall close_call(get_call_id("CursorClose"), emit_static);
			close_call.addParameter("SQLCA", BY_REFERENCE);
			close_call.addParameter("\"" + cursor_id + "\" & x\"00\"", BY_REFERENCE);
			put_call(close_call, stmt->period);
		}
		break;

		case ESQL_Command::Fetch:
		{
			put_start_exec_sql(stmt->period);
			for (cb_res_hostreference_ptr rp : *stmt->res_host_list) {
				ESQLCall rp_call(get_call_id("SetResultParams"), emit_static);
				cb_field_ptr hr = main_module_driver.field_map[rp->hostreference.mid(1)];
				if (!hr) {
					owner->err_messages << "Cannot find host variable: " + rp->hostreference.mid(1);
					return false;
				}

				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				rp_call.addParameter(f_type, BY_VALUE);
				rp_call.addParameter(f_size, BY_VALUE);
				rp_call.addParameter(f_scale, BY_VALUE);
				rp_call.addParameter(is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE, BY_VALUE);
				rp_call.addParameter(rp->hostreference.mid(1), BY_REFERENCE);

				put_call(rp_call, stmt->period);
			}

			QString cursor_id = stmt->cursorName;
			ESQLCall fetch_call(get_call_id("CursorFetchOne"), emit_static);
			fetch_call.addParameter("SQLCA", BY_REFERENCE);
			fetch_call.addParameter("\"" + cursor_id + "\" & x\"00\"", BY_REFERENCE);
			put_call(fetch_call, stmt->period);
			put_end_exec_sql(stmt->period);
		}
		break;

		case ESQL_Command::Commit:
		{
			put_start_exec_sql(stmt->period);
			ESQLCall fetch_call(get_call_id("Exec"), emit_static);
			fetch_call.addParameter("SQLCA", BY_REFERENCE);
			fetch_call.addParameter("\"COMMIT\" & x\"00\"", BY_REFERENCE);
			put_call(fetch_call, stmt->period);
			put_end_exec_sql(stmt->period);
		}
		break;

		case ESQL_Command::Update:
		case ESQL_Command::Delete:
		case ESQL_Command::Insert:
		{
			put_start_exec_sql(stmt->period);

			for (cb_hostreference_ptr p : *stmt->host_list) {
				ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);
				if (main_module_driver.field_map.find(p->hostreference.mid(1)) == main_module_driver.field_map.end()) {
					owner->err_messages << "Cannot find host variable: " + p->hostreference.mid(1);
					return false;
				}
				cb_field_ptr hr = main_module_driver.field_map[p->hostreference.mid(1)];
				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				p_call.addParameter(f_type, BY_VALUE);
				p_call.addParameter(f_size, BY_VALUE);
				p_call.addParameter(f_scale, BY_VALUE);
				p_call.addParameter(is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE, BY_VALUE);
				p_call.addParameter(p->hostreference.mid(1), BY_REFERENCE);

				put_call(p_call, stmt->period);
			}

			ESQLCall dml_call(get_call_id(stmt->host_list->size() == 0 ? "Exec" : "ExecParams"), emit_static);
			dml_call.addParameter(QString("SQ%1").arg(stmt->sql_query_list_id, 4, 10, QLatin1Char('0')), BY_REFERENCE);
			dml_call.addParameter(stmt->host_list->size(), BY_VALUE);
			put_call(dml_call, stmt->period);
			put_end_exec_sql(stmt->period);
		}
		break;


		default:
		{
			//owner->err_messages << "Invalid statement: " + stmt->commandName;
			//return false;
			ESQLCall exec_call(get_call_id("Exec"), emit_static);
			exec_call.addParameter("SQLCA", BY_REFERENCE);
			exec_call.addParameter(QString("SQ%1").arg(stmt->sql_query_list_id, 4, 10, QLatin1Char('0')), BY_REFERENCE);
			put_call(exec_call, stmt->period);
		}
		break;



	}
	return true;
}

bool TPESQLProcessing::find_working_storage(int *working_begin_line, int *working_end_line)
{
	QList<cb_exec_sql_stmt_ptr> *p = main_module_driver.exec_list;

	*working_begin_line = 0;
	*working_end_line = 0;

	for (auto e : *p) {
		if (e->commandName == ESQL_WORKING_BEGIN)
			*working_begin_line = e->startLine;

		if (e->commandName == ESQL_WORKING_END)
			*working_end_line = e->startLine;
	}

	return (*working_begin_line | *working_end_line) > 0;
}

QString TPESQLProcessing::comment_line(const QString &comment, const QString &line)
{
	QString ln = line;
	if (ln.size() < 7)
		ln = ln.leftJustified(7);

	ln[6] = '*';

	int m = comment.size() > 6 ? 6 : comment.size();

	ln = comment + ln.mid(m);
	return ln;
}


bool TPESQLProcessing::is_var_len_group(cb_field_ptr f)
{
	if (f->level == 49)
		return false;

	cb_field_ptr f1 = f->children;
	if (!f1 || f1->level != 49)
		return false;

	cb_field_ptr f2 = f1->sister;
	if (!f2 || f2->level != 49 || f2->pictype != PIC_ALPHANUMERIC)
		return false;

	return true;
}

int gethostvarianttype(cb_field_ptr p, int *type)
{
	int tmp_type = ERR_NOTDEF_CONVERSION;

	if (p->pictype != 0) {
		switch (p->pictype) {
			case PIC_ALPHANUMERIC:
				tmp_type = HVARTYPE_ALPHABETIC;
				break;
			case PIC_NATIONAL:
				tmp_type = HVARTYPE_NATIONAL;
				break;
			case PIC_NUMERIC:
				if (p->have_sign) {
					if (p->usage != Usage::None) {
						switch (p->usage) {
							case Usage::Packed:
								tmp_type = HVARTYPE_SIGNED_PACKED;
								break;
							case Usage::Binary:
								tmp_type = HVARTYPE_SIGNED_BINARY;
								break;
							default:
								return ERR_NOTDEF_CONVERSION;
						}
					}
					else if (p->sign_leading) {
						if (p->separate) {
							tmp_type = HVARTYPE_SIGNED_LEADING_SEPARATE;
						}
						else {
							tmp_type = HVARTYPE_SIGNED_LEADING_COMBINED;
						}
					}
					else {
						if (p->separate) {
							tmp_type = HVARTYPE_SIGNED_TRAILING_SEPARATE;
						}
						else {
							tmp_type = HVARTYPE_SIGNED_TRAILING_COMBINED;
						}
					}
				}
				else {
					if (p->usage != Usage::None) {
						switch (p->usage) {
							case Usage::Packed:
								tmp_type = HVARTYPE_UNSIGNED_PACKED;
								break;
							case Usage::Binary:
								tmp_type = HVARTYPE_UNSIGNED_BINARY;
								break;
							default:
								return ERR_NOTDEF_CONVERSION;
						}
					}
					else {
						tmp_type = HVARTYPE_UNSIGNED_NUMERIC;
					}
				}
				break;
			default:
				break;
		}
		*type = tmp_type;
		return 0;
	}
	if (p->usage != Usage::None) {
		switch (p->usage) {
			case Usage::Float:
				tmp_type = HVARTYPE_FLOAT;
				break;
			case Usage::Double:
				tmp_type = HVARTYPE_FLOAT;
				break;
			default:
				return ERR_NOTDEF_CONVERSION;
		}
		*type = tmp_type;
		return 0;
	}

	if (p->children && (p->level == 1 || ((p->level % 10) == 0))) {
		*type = HVARTYPE_GROUP;
		return 0;
	}

	return ERR_NOTDEF_CONVERSION;
}

bool TPESQLProcessing::get_actual_field_data(cb_field_ptr f, int *type, int *size, int *scale)
{
	bool is_varlen = is_var_len_group(f);
	if (!is_varlen) {
		gethostvarianttype(f, type);
		//*type = f->pictype;
		*size = f->picnsize;
		*scale = f->scale;
	}
	else {
		QString f_name = f->children->sister->sname;
		cb_field_ptr f_actual = main_module_driver.field_map[f_name];

		gethostvarianttype(f_actual, type);
		//*type = f_actual->pictype;
		*size = f_actual->picnsize + 2;
		*scale = f_actual->scale;
	}
	return is_varlen;
}

void TPESQLProcessing::process_sql_query_list()
{
	for (cb_exec_sql_stmt_ptr p : *main_module_driver.exec_list) {
		if (p->sql_list->size()) {
			QString sql = p->sql_list->join(" ");
			ws_query_list.append(sql);
		}
	}
}

QMap<uint64_t, uint64_t> &TPESQLProcessing::getBinarySrcLineMap() const
{
	return const_cast<QMap<uint64_t, uint64_t>&>(b_in_to_out);
}

QMap<uint64_t, uint64_t> &TPESQLProcessing::getBinarySrcLineMapReverse() const
{
	return const_cast<QMap<uint64_t, uint64_t>&>(b_out_to_in);
}

bool TPESQLProcessing::build_map_data()
{
	map_collect_files(filemap);

	QString file;
	int line_in, line_out;
	//uint64_t k, v;

	// in to out map
	for (auto it = in_to_out.constBegin(); it != in_to_out.constEnd(); ++it) {
		splitLineEntry(it.key(), file, &line_in);
		int fin_id = filemap[file];

		splitLineEntry(it.value(), file, &line_out);
		int fout_id = filemap[file];

		uint64_t k = ((uint64_t)fin_id << 32) + line_in;
		uint64_t v = ((uint64_t)fout_id << 32) + line_out;

		b_in_to_out[k] = v;
	}

	// out to in map
	for (auto it = out_to_in.constBegin(); it != out_to_in.constEnd(); ++it) {

		splitLineEntry(it.key(), file, &line_out);
		int fout_id = filemap[file];

		splitLineEntry(it.value(), file, &line_in);
		int fin_id = filemap[file];

		uint64_t k = ((uint64_t)fout_id << 32) + line_out;
		uint64_t v = ((uint64_t)fin_id << 32) + line_in;

		b_out_to_in[k] = v;

	}

	// Variable declaraton source location info

	//mw.addSection("field_map");
	//mw.appendToSectionContents("field_map", main_module_driver.field_map.size());
	//for (auto it = main_module_driver.field_map.begin(); it != main_module_driver.field_map.end(); ++it) {
	//	QString path = "";
	//	QString var = it.key();
	//	cb_field_ptr fld = it.value();

	//	cb_field_ptr p = fld;
	//	do {
	//		path = p->sname + ":" + path;
	//		p = p->parent;
	//	} while (p);

	//	if (path.length() > 0)
	//		path = path.left(path.length() - 1);

	//	path = "WS:" + path;

	//	mw.appendToSectionContents("field_map", QString("%1/%2@%3:%4").arg(fld->sname).arg(path).arg(fld->defined_at_source_file).arg(fld->defined_at_source_line));
	//}

	return true;
}

bool TPESQLProcessing::write_map_file(const QString &preprocd_file)
{
	map_collect_files(filemap);

	if (opt_no_output)
		return true;

	MapFileWriter mw;
	QString outfile = PathUtils::changeExtension(preprocd_file, ".cbsql.map");

	uint32_t nflags = FLAG_M_BASE;

	// global data
	mw.addSection("map");
	mw.appendToSectionContents("map", MAP_FILE_FMT_VER);
	mw.appendToSectionContents("map", nflags);
	mw.appendToSectionContents("map", input_file);
	mw.appendToSectionContents("map", output_file);
	mw.appendToSectionContents("map", filemap[input_file]);
	mw.appendToSectionContents("map", filemap[output_file]);

	// file map
	mw.addSection("filemap");
	mw.appendToSectionContents("filemap", filemap.size());
	for (auto it = filemap.constBegin(); it != filemap.constEnd(); ++it) {
		mw.appendToSectionContents("filemap", QString("#%1:%2").arg(it.value()).arg(it.key()));
	}

	QString file;
	int line_in, line_out;

	// in to out map
	mw.addSection("in_to_out_map");
	mw.appendToSectionContents("in_to_out_map", in_to_out.size());

	for (auto it = in_to_out.constBegin(); it != in_to_out.constEnd(); ++it) {
		splitLineEntry(it.key(), file, &line_in);
		int fin_id = filemap[file];

		splitLineEntry(it.value(), file, &line_out);
		int fout_id = filemap[file];

		mw.appendToSectionContents("in_to_out_map", QString("%1@%2:%3@%4").arg(line_in).arg(fin_id).arg(line_out).arg(fout_id));
	}

	// out to in map
	mw.addSection("out_to_in_map");
	mw.appendToSectionContents("out_to_in_map", out_to_in.size());

	for (auto it = out_to_in.constBegin(); it != out_to_in.constEnd(); ++it) {

		splitLineEntry(it.key(), file, &line_out);
		int fout_id = filemap[file];

		splitLineEntry(it.value(), file, &line_in);
		int fin_id = filemap[file];

		mw.appendToSectionContents("out_to_in_map", QString("%1@%2:%3@%4").arg(line_out).arg(fout_id).arg(line_in).arg(fin_id));
	}

	// Variable declaraton source location info

	mw.addSection("field_map");
	mw.appendToSectionContents("field_map", main_module_driver.field_map.size());
	for (auto it = main_module_driver.field_map.begin(); it != main_module_driver.field_map.end(); ++it) {
		QString path = "";
		QString var = it.key();
		cb_field_ptr fld = it.value();

		cb_field_ptr p = fld;
		do {
			path = p->sname + ":" + path;
			p = p->parent;
		} while (p);

		if (path.length() > 0)
			path = path.left(path.length() - 1);

		path = "WS:" + path;

		mw.appendToSectionContents("field_map", QString("%1/%2@%3:%4").arg(fld->sname).arg(path).arg(fld->defined_at_source_file).arg(fld->defined_at_source_line));
	}

	return mw.writeToFile(outfile);
}

void TPESQLProcessing::add_dependency(const QString &parent, const QString &dep_path)
{
	QStringList deps = (file_dependencies.contains(parent) ? file_dependencies.value(parent) : QStringList());
	deps.append(dep_path);
	file_dependencies[parent] = deps;
}

QString TPESQLProcessing::getModuleName()
{
	return main_module_driver.program_id;
}
//
//bool TPESQLProcessing::write_map_file(const QString &preprocd_file)
//{
//	map_collect_files(filemap);
//
//	if (opt_no_output)
//		return true;
//
//	QFile f(PathUtils::changeExtension(preprocd_file,".cbsql.map"));
//	if (!f.open(QIODevice::WriteOnly | QIODevice::Text))
//		return false;
//
//	uint32_t nflags = FLAG_M_BASE;
//
//	QTextStream ts(&f);
//
//	ts << MAP_FILE_FMT_VER << "\n";
//	ts << nflags << "\n";
//
//	ts << input_file << "\n";
//	ts << output_file << "\n";
//
//	ts << filemap[input_file] << "\n";
//	ts << filemap[output_file] << "\n";
//	
//	ts << filemap.size() << "\n";
//
//	QMap<QString, int>::const_iterator it = filemap.constBegin();
//	auto end = filemap.constEnd();
//	while (it != end) {
//		ts << QString("#%1:%2\n").arg(it.value()).arg(it.key());
//		++it;
//	}
//
//	// in to out map
//	ts << in_to_out.size() << "\n";
//
//	QString file;
//	int line_in, line_out;
//	QMap<QString, QString>::const_iterator itm = in_to_out.constBegin();
//	auto endm = in_to_out.constEnd();
//	while (itm != endm) {
//
//		splitLineEntry(itm.key(), file, &line_in);
//		int fin_id = filemap[file];
//		
//		splitLineEntry(itm.value(), file, &line_out);
//		int fout_id = filemap[file];
//
//		ts << QString("%1@%2:%3@%4\n").arg(line_in).arg(fin_id).arg(line_out).arg(fout_id);
//
//		++itm;
//	}
//
//	// out to in map
//	ts << out_to_in.size() << "\n";
//
//	QString file;
//	int line_in, line_out;
//	QMap<QString, QString>::const_iterator itm = out_to_in.constBegin();
//	auto endm = out_to_in.constEnd();
//	while (itm != endm) {
//
//		splitLineEntry(itm.key(), file, &line_out);
//		int fout_id = filemap[file];
//
//		splitLineEntry(itm.value(), file, &line_in);
//		int fin_id = filemap[file];
//
//		ts << QString("%1@%2:%3@%4\n").arg(line_out).arg(fout_id).arg(line_in).arg(fin_id);
//
//		++itm;
//	}
//
//	// Variable declaraton source location info
//	ts << main_module_driver.field_map.size() << "\n";
//	for (auto it = main_module_driver.field_map.begin(); it != main_module_driver.field_map.end(); ++it) {
//		QString path = "";
//		QString var = it.key();
//		cb_field_ptr fld = it.value();
//
//		cb_field_ptr p = fld;
//		do {
//			path = p->sname + ":" + path;
//			p = p->parent;
//		} while (p);
//
//		if (path.length() > 0)
//			path = path.left(path.length() - 1);
//
//		path = "WS:" + path;
//
//		ts << QString("%1/%2@%3:%4\n").arg(fld->sname).arg(path).arg(fld->defined_at_source_file).arg(fld->defined_at_source_line);
//	}
//
//	f.close();
//
//	return true;
//}

void TPESQLProcessing::splitLineEntry(const QString &k, QString &s, int *i)
{
	int p = k.indexOf("@");
	s = k.mid(p + 1);
	*i = k.mid(0, p).toInt();
}


void TPESQLProcessing::map_collect_files(QMap<QString, int> &filemap)
{
	int l;
	int n = 1;
	QString f;

	if (!in_to_out.size())
		return;

	QString s = in_to_out[in_to_out.keys().at(0)];
	splitLineEntry(s, f, &l);
	filemap[f] = n++;

	QMap<QString, QString>::const_iterator it = in_to_out.constBegin();
	auto end = in_to_out.constEnd();
	while (it != end) {
		splitLineEntry(it.key(), f, &l);
		if (!filemap.contains(f))
			filemap[f] = n++;

		splitLineEntry(it.value(), f, &l);
		if (!filemap.contains(f))
			filemap[f] = n++;

		++it;
	}
}

QMap<QString, QString> &TPESQLProcessing::getSrcLineMap() const
{
	return const_cast<QMap<QString, QString>&>(in_to_out);
}

QMap<QString, QString> &TPESQLProcessing::getSrcLineMapReverse() const
{
	return const_cast<QMap<QString, QString>&>(out_to_in);
}

QMap<QString, int> &TPESQLProcessing::getFileMap() const
{
	return const_cast<QMap<QString, int>&>(filemap);
}

QMap<int, QString> TPESQLProcessing::getReverseFileMap()
{
	QMap<int, QString> rm;
	for (QString k : filemap.keys()) {
		int v = filemap[k];
		rm[v] = k;
	}
	return rm;
}

QMap<QString, cb_field_ptr> &TPESQLProcessing::getVariableDeclarationInfoMap() const
{
	return const_cast<QMap<QString, cb_field_ptr>&>(main_module_driver.field_map);
}

QMap<QString, srcLocation> TPESQLProcessing::getParagraphs()
{
	return main_module_driver.paragraphs;
}

QMap<QString, QStringList> TPESQLProcessing::getFileDependencies()
{
	return file_dependencies;
}
