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
#include "ESQLCall.h"
#include "gix_esql_driver.hh"
#include "MapFileWriter.h"
#include "libcpputils.h"

#include "linq/linq.hpp"

#if defined(_WIN32) && defined(_DEBUG)
#include <Windows.h>
#endif

#define ESQL_CONNECT					"CONNECT"
#define ESQL_CONNECT_RESET				"CONNECT_RESET"
#define ESQL_DISCONNECT					"DISCONNECT"
#define ESQL_CLOSE						"CLOSE"
#define ESQL_COMMIT						"COMMIT"
#define ESQL_FETCH						"FETCH"
#define ESQL_INCFILE					"INCFILE"
#define ESQL_INCSQLCA					"INCSQLCA"
#define ESQL_INSERT						"INSERT"
#define ESQL_OPEN						"OPEN"
#define ESQL_SELECT						"SELECT"
#define ESQL_UPDATE						"UPDATE"
#define ESQL_DELETE						"DELETE"
#define ESQL_WORKING_BEGIN				"WORKING_BEGIN"
#define ESQL_WORKING_END				"WORKING_END"
#define ESQL_FILE_BEGIN					"FILE_BEGIN"
#define ESQL_FILE_END					"FILE_END"
#define ESQL_LINKAGE_BEGIN				"LINKAGE_BEGIN"
#define ESQL_LINKAGE_END				"LINKAGE_END"
#define ESQL_PROCEDURE_DIVISION			"PROCEDURE_DIVISION"
#define ESQL_DECLARE_TABLE				"DECLARE_TABLE"
#define ESQL_DECLARE_VAR				"DECLARE_VAR"
#define ESQL_COMMENT					"COMMENT"
#define ESQL_IGNORE						"IGNORE"
#define ESQL_PREPARE					"PREPARE_STATEMENT"
#define ESQL_EXEC_PREPARED				"EXECUTE_PREPARED"
#define ESQL_EXEC_IMMEDIATE				"EXECUTE_IMMEDIATE"
#define ESQL_PASSTHRU					"PASSTHRU"

#define BEGIN_DECLARE_SECTION			"HOST_BEGIN"
#define END_DECLARE_SECTION				"HOST_END"

#define AREA_B_PREFIX       "           " // 11 spaces
#define AREA_B_CPREFIX      "GIXSQL     " // comment + 5 spaces
#define TAB					"    "		  // 4 spaces

#define PIC_ALPHABETIC 		0x01
#define PIC_NUMERIC 		0x02
#define PIC_NATIONAL		0x04
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)

#define CBL_FIELD_FLAG_NONE		(uint32_t)0x0
#define CBL_FIELD_FLAG_VARLEN	(uint32_t)0x80
#define CBL_FIELD_FLAG_BINARY	(uint32_t)0x100

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

// These must be in sync with the ones in SqlVar.h
#define VARLEN_SUFFIX_DATA		"DATA"
#define VARLEN_SUFFIX_LENGTH	"LENGTH"
#ifdef USE_VARLEN_32
#define VARLEN_LENGTH_PIC		"9(05) BINARY"
#define VARLEN_PIC_SZ			5
#define VARLEN_LENGTH_SZ		4
#define VARLEN_LENGTH_T			uint32_t
#define VARLEN_BSWAP			COB_BSWAP_32
#else
#define VARLEN_LENGTH_PIC		"9(4) BINARY"
#define VARLEN_PIC_SZ			4
#define VARLEN_LENGTH_SZ		2
#define VARLEN_LENGTH_T			uint16_t
#define VARLEN_BSWAP			COB_BSWAP_16
#endif

enum class ESQL_Command
{
	Connect,
	ConnectReset,
	Disconnect,
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
	PrepareStatement,
	ExecPrepared,
	ExecImmediate,

	// Helpers
	StartSQL,
	EndSQL,

	BeginDeclareSection,
	EndDeclareSection,

	// Fields declaration
	DeclareVar,

	// Comment code ranges (also comment everything between EXEC SQL IGNORE and END-EXEC)
	Comment,

	// Comment code ranges (leave anything between EXEC SQL IGNORE and END-EXEC uncommented)
	Ignore,

	// ESQL is passed directly to the database driver (unknow statements, DBMS-specific syntax, etc.)
	PassThru,

	Unknown
};

static bool check_sql_type_compatibility(uint64_t type_info, cb_field_ptr var);

static std::map<std::string, ESQL_Command> ESQL_cmd_map{ { ESQL_CONNECT, ESQL_Command::Connect }, { ESQL_CONNECT_RESET, ESQL_Command::ConnectReset },
												 { ESQL_DISCONNECT, ESQL_Command::Disconnect }, { ESQL_CLOSE, ESQL_Command::Close },
												 { ESQL_COMMIT, ESQL_Command::Commit }, { ESQL_FETCH, ESQL_Command::Fetch },{ ESQL_DELETE, ESQL_Command::Delete },
												 { ESQL_INCFILE, ESQL_Command::Incfile }, { ESQL_INCSQLCA, ESQL_Command::IncSQLCA }, { ESQL_INSERT, ESQL_Command::Insert },
												 { ESQL_OPEN, ESQL_Command::Open }, { ESQL_SELECT, ESQL_Command::Select }, { ESQL_UPDATE, ESQL_Command::Update },
												 { ESQL_WORKING_BEGIN, ESQL_Command::WorkingBegin }, { ESQL_WORKING_END, ESQL_Command::WorkingEnd } ,
												 { ESQL_LINKAGE_BEGIN, ESQL_Command::LinkageBegin }, { ESQL_LINKAGE_END, ESQL_Command::WorkingEnd } ,
												 { ESQL_FILE_BEGIN, ESQL_Command::FileBegin }, { ESQL_FILE_END, ESQL_Command::FileEnd } ,
												 { ESQL_PROCEDURE_DIVISION, ESQL_Command::ProcedureDivision }, { ESQL_DECLARE_TABLE, ESQL_Command::DeclareTable },
												 { ESQL_PREPARE, ESQL_Command::PrepareStatement }, { ESQL_EXEC_PREPARED, ESQL_Command::ExecPrepared },
												 { ESQL_EXEC_IMMEDIATE, ESQL_Command::ExecImmediate}, { ESQL_DECLARE_VAR, ESQL_Command::DeclareVar },
												 { BEGIN_DECLARE_SECTION, ESQL_Command::BeginDeclareSection}, { END_DECLARE_SECTION, ESQL_Command::EndDeclareSection },
												 { ESQL_COMMENT, ESQL_Command::Comment } , { ESQL_IGNORE, ESQL_Command::Ignore }, { ESQL_PASSTHRU, ESQL_Command::PassThru } };

#define CALL_PREFIX	"GIXSQL"
#define TAG_PREFIX	"GIXSQL"

inline std::string TPESQLProcessing::get_call_id(const std::string s)
{
	return CALL_PREFIX + s;
}

TPESQLProcessing::TPESQLProcessing(GixPreProcessor *gpp) : ITransformationStep(gpp)
{
	opt_anonymous_params = std::get<bool>(gpp->getOpt("anonymous_params", false));
	opt_preprocess_copy_files = std::get<bool>(gpp->getOpt("preprocess_copy_files", false));
	opt_emit_static_calls = std::get<bool>(gpp->getOpt("emit_static_calls", false));
	opt_emit_debug_info = std::get<bool>(gpp->getOpt("emit_debug_info", false));
	opt_emit_compat = std::get<bool>(gpp->getOpt("emit_compat", false));
	opt_consolidated_map = std::get<bool>(gpp->getOpt("consolidated_map", false));
	opt_no_output = std::get<bool>(gpp->getOpt("no_output", false));
	opt_emit_map_file = std::get<bool>(gpp->getOpt("emit_map_file", false));

	output_line = 0;
	working_begin_line = 0;
	working_end_line = 0;
	current_input_line = 0;
}

bool TPESQLProcessing::run(ITransformationStep *prev_step)
{
	if (input_file.empty()) {
		if (!prev_step || prev_step->getOutput().empty())
			return false;

		input_file = prev_step->getOutput();
	}

#if _DEBUG_LOG_ON
	char dbg_bfr[512];
#if defined(_WIN32)
	OutputDebugStringA("TPESQLProcessing invoked\n===============\n");
#else
	fprintf(stderr, "TPESQLProcessing invoked\n===============\n");
#endif

	for (auto it = owner->getOpts().begin(); it != owner->getOpts().end(); ++it) {
		sprintf(dbg_bfr, "%s = %s\n", it.key().toLocal8Bit().data(), it.value().toString().toLocal8Bit().data());
#if defined(_WIN32)
		OutputDebugStringA(dbg_bfr);
#else
		fprintf(stderr, dbg_bfr);
#endif

	}
	for (auto cd : owner->getCopyResolver()->getCopyDirs()) {
		sprintf(dbg_bfr, "COPY path: %s\n", cd.toLocal8Bit().data());
#if defined(_WIN32)
		OutputDebugStringA(dbg_bfr);
#else
		fprintf(stderr, dbg_bfr);
#endif
	}
#if defined(_WIN32)
	OutputDebugStringA("===============\n");
#else
	fprintf(stderr, "TPESQLProcessing invoked\n===============\n");
#endif
#endif

	main_module_driver.opt_use_anonymous_params = opt_anonymous_params;
	main_module_driver.opt_preprocess_copy_files = opt_preprocess_copy_files;

	code_tag = TAG_PREFIX;

	int rc = main_module_driver.parse(owner, input_file);
	if (!rc) {
		rc = outputESQL();
	}

	owner->err_data.err_code = rc;

	return rc == 0;
}

std::string TPESQLProcessing::getOutput(ITransformationStep *me)
{
	return output_file;
}

int TPESQLProcessing::outputESQL()
{
	working_begin_line = 0;
	working_end_line = 0;


	if (output_file.empty()) {
		std::string f = filename_change_ext(input_file, ".cbsql");
		output_file = "#" + filename_get_name(f);
	}

	if (!starts_with(output_file, "#") && !file_is_writable(output_file))
		return -1;

	input_file_stack.push(filename_clean_path(input_file));

	if (!find_working_storage(&working_begin_line, &working_end_line))
		return -1;

	startup_items = cpplinq::from(*(main_module_driver.exec_list)).where([](cb_exec_sql_stmt_ptr p) { return p->startup_item != 0; }).to_vector();
	process_sql_query_list();
	if (!fixup_declared_vars()) {
		return -1;
	}


#if defined(_WIN32) && defined(_DEBUG)
	std::vector<cb_exec_sql_stmt_ptr> *p = main_module_driver.exec_list;
	for (auto e : *p) {
		char bfr[1024];
		sprintf(bfr, "%04d-%04d : %s\n", e->startLine, e->endLine, e->commandName.c_str());
		OutputDebugStringA(bfr);
	}
#endif

	if (!processNextFile())
		return 1;

	bool b1 = opt_no_output ? true : file_write_all_lines(output_file, output_lines);
	bool b2;
	if (opt_no_output) {
		build_map_data();
		b2 = true;
	}
	else {
		if (opt_emit_map_file)
			b2 = write_map_file(output_file);
		else {
			build_map_data();
			b2 = true;
		}
	}

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
	input_file_stack = std::stack<std::string>();
	input_file_stack.push(filename_clean_path(input_file));

	opt_preprocess_copy_files = true;
	main_module_driver.opt_preprocess_copy_files = true;

	int rc = main_module_driver.parse(owner, input_file);

	return processNextFile();
}


bool TPESQLProcessing::processNextFile()
{
	std::string the_file = input_file_stack.top();
	std::vector<std::string> input_lines = file_read_all_lines(the_file);

	if (!input_lines.size()) {
		input_file_stack.pop();
		if (input_file_stack.size() > 0)
			main_module_driver.file = input_file_stack.top();

		return true;
	}

	for (int input_line = 1; input_line <= input_lines.size(); input_line++) {
		current_input_line = input_line;

#if defined(_WIN32) && defined(_DEBUG) && defined(VERBOSE)
		char bfr[512];
		sprintf(bfr, "Processing line %d of file %s\n", input_line, the_file.c_str());
		OutputDebugStringA(bfr);
#endif
		std::string cur_line = input_lines.at(input_line - 1);

		bool in_ws = (input_line >= working_begin_line) && (input_line <= working_end_line);

		cb_exec_sql_stmt_ptr exec_sql_stmt = find_exec_sql_stmt(the_file, input_line);
		if (!exec_sql_stmt) {
			put_output_line(cur_line);
			continue;
		}

		std::string cmdname = exec_sql_stmt->commandName;
		ESQL_Command cmd = map_contains<std::string, ESQL_Command>(ESQL_cmd_map, cmdname) ? ESQL_cmd_map[cmdname] : ESQL_Command::Unknown;

		switch (cmd) {

			case ESQL_Command::WorkingBegin:
			case ESQL_Command::LinkageBegin:
			case ESQL_Command::LinkageEnd:
			case ESQL_Command::FileBegin:
			case ESQL_Command::FileEnd:
				put_output_line(input_lines.at(exec_sql_stmt->startLine - 1));
				break;

			case ESQL_Command::ProcedureDivision:	// PROCEDURE DIVISION can be string_split across several lines if a USING clause is added
				for (int iline = exec_sql_stmt->startLine; iline <= exec_sql_stmt->endLine; iline++) {
					put_output_line(input_lines.at(iline - 1));
				}

				if (!put_cursor_declarations()) {
					main_module_driver.error("An error occurred while generating ESQL cursor declarations", ERR_ALREADY_SET);
					return false;
				}
				break;

			case ESQL_Command::Ignore:
				{
					std::vector<std::string> tmp_outlines;
					for (int iline = exec_sql_stmt->startLine; iline <= exec_sql_stmt->endLine; iline++) {
						tmp_outlines.push_back(input_lines.at(iline - 1));
					}
					if (!tmp_outlines.size())
						break;

					std::string txt = vector_join(tmp_outlines, "\n");
					txt = string_replace_regex(txt, "EXEC[\\ \\r\\n]+SQL[\\ \\r\\n]+IGNORE([\\ \\r\\n]+)?", "");
					txt = string_replace_regex(txt, "[\\ \\r\\n]+END-EXEC([\\ ]*\\.)", "");
					tmp_outlines = string_split(txt, "[\\n]");

					put_output_line(code_tag + "*   ESQL IGNORE");
					for (auto tl : tmp_outlines) {
						put_output_line(tl);
					}
					put_output_line(code_tag + "*   ESQL END IGNORE");
				}
				break;

			default:
				// Add original text, commented
				for (int n = exec_sql_stmt->startLine; n <= exec_sql_stmt->endLine; n++) {
					put_output_line(comment_line("GIXSQL", input_lines.at(n - 1)));
					current_input_line++;
				}

				// Add ESQL calls
				if (!handle_esql_stmt(cmd, exec_sql_stmt, in_ws)) {
					//owner->err_data.err_messages.push_back(string_format("Error in ESQL statement at line %d of file %s: %s", input_line, input_file, cur_line));
					main_module_driver.error("Error in ESQL statement", ERR_ALREADY_SET);
					return false;
				}
		}

		// Special case
		if (exec_sql_stmt->endLine == working_end_line) {
			if (!handle_esql_stmt(ESQL_Command::WorkingEnd, find_esql_cmd(ESQL_WORKING_END, 0), 0)) {
				//owner->err_data.err_messages.push_back(string_format("Error in ESQL statement at line %d of file %s: %s", input_line, input_file, cur_line));
				main_module_driver.error("Error in ESQL statement", ERR_ALREADY_SET);
				return false;
			}
		}

		// Update input line pointer
		input_line = exec_sql_stmt->endLine;
		current_input_line = input_line;
	}

	input_file_stack.pop();
	if (input_file_stack.size() > 0)
		main_module_driver.file = input_file_stack.top();

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

std::string take_max(std::string &s, int n)
{
	std::string res;
	if (s.length() > n) {
		res = s.substr(0, n);
		s = s.substr(n);
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
		std::string qry = ws_query_list.at(i - 1);
		int qry_len = qry.length();
		qry = string_replace(qry, "\"", "\"\"");
		put_output_line(code_tag + string_format(" 01  SQ%04d.", i));

		int pos = 0;
		int max_sec_len = 30;

		std::string s;

		s = take_max(qry, 30);
		put_output_line(code_tag + string_format("     02  FILLER PIC X(%d) VALUE \"%s\"", qry_len, s));

		while (true) {
			s = take_max(qry, 58);
			if (s.empty())
				break;

			put_output_line(code_tag + string_format("  &  \"%s\"", s));
		}
		output_lines.back() += ".";

		put_output_line(code_tag + std::string("     02  FILLER PIC X(1) VALUE X\"00\"."));
	}

	emitted_query_defs = true;
}

void TPESQLProcessing::put_working_storage()
{
	put_output_line(code_tag + std::string(" WORKING-STORAGE SECTION."));
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
			put_start_exec_sql(false);

			for (cb_hostreference_ptr p : *stmt->host_list) {
				ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);
				if (!main_module_driver.field_exists(p->hostreference.substr(1))) {
					main_module_driver.error("Cannot find host variable " + p->hostreference.substr(1), ERR_MISSING_HOSTVAR);
					return false;
				}

				cb_field_ptr hr = main_module_driver.field_map[p->hostreference.substr(1)];
				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
				flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

				p_call.addParameter(f_type, BY_VALUE);
				p_call.addParameter(f_size, BY_VALUE);
				p_call.addParameter(f_scale, BY_VALUE);
				p_call.addParameter(flags, BY_VALUE);
				p_call.addParameter(p->hostreference.substr(1), BY_REFERENCE);

				if (!put_call(p_call, false))
					return false;
			}

			ESQLCall cd_call(get_call_id("CursorDeclareParams"), emit_static);
			cd_call.addParameter("SQLCA", BY_REFERENCE);
			cd_call.addParameter(&main_module_driver, stmt->connectionId);
			cd_call.addParameter("\"" + stmt->cursorName + "\" & x\"00\"", BY_REFERENCE); //& x\"00\"
			cd_call.addParameter(std::to_string(stmt->cursor_hold ? 1 : 0), BY_VALUE);
			cd_call.addParameter(stmt->sqlName, BY_REFERENCE); //& x\"00\"
			cd_call.addParameter(std::to_string(stmt->host_list->size()), BY_VALUE);

			if (!put_call(cd_call, false))
				return false;

			put_end_exec_sql(stmt->period);
		}
		else { // (struct sqlca_t *st, char *cname, int with_hold, char *_query)
			ESQLCall cd_call(get_call_id("CursorDeclare"), emit_static);
			cd_call.addParameter("SQLCA", BY_REFERENCE);
			cd_call.addParameter(&main_module_driver, stmt->connectionId);
			cd_call.addParameter("\"" + stmt->cursorName + "\" & x\"00\"", BY_REFERENCE);
			cd_call.addParameter(stmt->cursor_hold, BY_VALUE);
			cd_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);

			if (!put_call(cd_call, stmt->period))
				return false;
		}
	}

	put_output_line(code_tag + "*   ESQL STARTUP DECLARATIONS (END)");
	return true;
}

bool TPESQLProcessing::put_call(const ESQLCall &c, bool terminate_with_period)
{
	if (c.hasError()) {
		owner->err_data.err_messages.push_back(c.error());
		return false;
	}

	auto lines = c.format();

	// The END-CALL statement was added by the format method, we just need (in case) to add the period
	if (terminate_with_period) {
		lines.back() = lines.back() + ".";
	}

	for (auto ln : lines) {
		put_output_line(ln);
	}

	return true;
}

cb_exec_sql_stmt_ptr TPESQLProcessing::find_exec_sql_stmt(const std::string filename, int i)
{
	std::vector<cb_exec_sql_stmt_ptr> *p = main_module_driver.exec_list;
	for (auto e : *p) {
		std::string f1 = filename_absolute_path(filename);
		std::string f2 = filename_absolute_path(e->src_file);
		if (f1 == f2 && (e->startLine <= i && e->endLine >= i))
			return e;
	}

	return NULL;
}

cb_exec_sql_stmt_ptr TPESQLProcessing::find_esql_cmd(std::string cmd, int idx)
{
	int n = 0;
	std::vector<cb_exec_sql_stmt_ptr> *p = main_module_driver.exec_list;
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

void TPESQLProcessing::put_output_line(const std::string &line)
{
	output_line++;

	output_lines.push_back(line);

	std::string output_id = string_format("%d@%s", output_line, output_file);
	std::string input_id = string_format("%d@%s", current_input_line, input_file_stack.top());
#if defined(_WIN32) && defined(_DEBUG) && defined(_DEBUG_LOG_ON)
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
		case ESQL_Command::IncSQLCA:
		{
			std::string copy_file;
			std::string inc_copy_name = (cmd == ESQL_Command::Incfile) ? stmt->incfileName : "SQLCA";
			if (opt_preprocess_copy_files) {
				// inline file
				if (!owner->getCopyResolver()->resolveCopyFile(inc_copy_name, copy_file)) {
					//owner->err_data.err_messages.push_back("Cannot resolve copybook: " + inc_copy_name);
					main_module_driver.error("Cannot resolve copybook: " + inc_copy_name, ERR_MISSING_COPYFILE);
					return false;
				}

				add_dependency(input_file_stack.top(), copy_file);

				input_file_stack.push(filename_clean_path(copy_file));
				if (!processNextFile())
					return false;
			}
			else {

				// we treat it as a standard copybook file, and let the compiler deal with any error
				// but we still try to resolve the copy to gather some metadata
				put_output_line(AREA_B_PREFIX + string_format("COPY %s.", inc_copy_name));

				if (owner->getCopyResolver()->resolveCopyFile(inc_copy_name, copy_file)) {
					add_dependency(input_file_stack.top(), copy_file);
				}
				else
					add_dependency(input_file_stack.top(), "*" + inc_copy_name);

			}

			_DBG_OUT("Copy resolved: %s -> %s\n", inc_copy_name.c_str(), copy_file.c_str());
		}
		break;

		case ESQL_Command::Connect:
		{
			ESQLCall connect_call(get_call_id("Connect"), emit_static);
			connect_call.addParameter("SQLCA", BY_REFERENCE);

			connect_call.addParameter(&main_module_driver, stmt->conninfo->data_source);
			connect_call.addParameter(&main_module_driver, stmt->conninfo->id);
			connect_call.addParameter(&main_module_driver, stmt->conninfo->username);
			connect_call.addParameter(&main_module_driver, stmt->conninfo->password);

			if (!put_call(connect_call, stmt->period))
				return false;
		}
		break;

		case ESQL_Command::ConnectReset:
		{
			ESQLCall connect_call(get_call_id("ConnectReset"), emit_static);
			connect_call.addParameter("SQLCA", BY_REFERENCE);
			connect_call.addParameter(&main_module_driver, stmt->connectionId);

			if (!put_call(connect_call, stmt->period))
				return false;
		}
		break;

		case ESQL_Command::Disconnect:
		{
			ESQLCall connect_call(get_call_id("ConnectReset"), emit_static);
			connect_call.addParameter("SQLCA", BY_REFERENCE);
			connect_call.addParameter(&main_module_driver, stmt->connectionId);

			if (!put_call(connect_call, stmt->period))
				return false;
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

			put_start_exec_sql(false);
			for (cb_res_hostreference_ptr rp : *stmt->res_host_list) {
				ESQLCall rp_call(get_call_id("SetResultParams"), emit_static);

				if (!main_module_driver.field_exists(rp->hostreference.substr(1))) {
					//owner->err_data.err_messages.push_back("Cannot find host variable: " + rp->hostreference.substr(1));
					main_module_driver.error("Cannot find host variable: " + rp->hostreference.substr(1), ERR_MISSING_HOSTVAR);
					return false;
				}

				cb_field_ptr hr = main_module_driver.field_map[rp->hostreference.substr(1)];

				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
				flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

				rp_call.addParameter(f_type, BY_VALUE);
				rp_call.addParameter(f_size, BY_VALUE);
				rp_call.addParameter(f_scale, BY_VALUE);
				rp_call.addParameter(flags, BY_VALUE);
				rp_call.addParameter(rp->hostreference.substr(1), BY_REFERENCE);

				if (!put_call(rp_call, false))
					return false;
			}

			for (cb_hostreference_ptr p : *stmt->host_list) {
				ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);

				if (!main_module_driver.field_exists(p->hostreference.substr(1))) {
					//owner->err_data.err_messages.push_back("Cannot find host variable: " + p->hostreference.substr(1));
					main_module_driver.error("Cannot find host variable: " + p->hostreference.substr(1), ERR_MISSING_HOSTVAR);
					return false;
				}

				cb_field_ptr hr = main_module_driver.field_map[p->hostreference.substr(1)];

				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
				flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

				p_call.addParameter(f_type, BY_VALUE);
				p_call.addParameter(f_size, BY_VALUE);
				p_call.addParameter(f_scale, BY_VALUE);
				p_call.addParameter(flags, BY_VALUE);
				p_call.addParameter(p->hostreference.substr(1), BY_REFERENCE);

				if (!put_call(p_call, false))
					return false;
			}

			std::string call_id;
			if (!stmt->res_host_list->size()) {
				call_id = get_call_id(stmt->cursorName.empty() ? "Exec" : "CursorDeclare");
				if (stmt->host_list->size())
					call_id += "Params";
			}
			else {
				call_id = get_call_id("ExecSelectIntoOne");
			}

			if (stmt->cursorName.empty()) {
				ESQLCall select_call(call_id, emit_static);
				select_call.addParameter("SQLCA", BY_REFERENCE);
				select_call.addParameter(&main_module_driver, stmt->connectionId);
				select_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
				select_call.addParameter(stmt->host_list->size(), BY_VALUE);
				select_call.addParameter(stmt->res_host_list->size(), BY_VALUE);
				if (!put_call(select_call, false))
					return false;
			}
			else {
				ESQLCall select_call(call_id, emit_static);
				select_call.addParameter("SQLCA", BY_REFERENCE);
				select_call.addParameter(&main_module_driver, stmt->connectionId);
				select_call.addParameter("\"" + stmt->cursorName + "\" & x\"00\"", BY_REFERENCE);
				select_call.addParameter(stmt->cursor_hold, BY_VALUE);
				select_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);

				if (!put_call(select_call, false))
					return false;
			}
			put_end_exec_sql(stmt->period);
		}
		break;

		case ESQL_Command::Open:
		{
			std::string cursor_id = stmt->cursorName;
			ESQLCall open_call(get_call_id("CursorOpen"), emit_static);
			open_call.addParameter("SQLCA", BY_REFERENCE);
			open_call.addParameter("\"" + cursor_id + "\" & x\"00\"", BY_REFERENCE);

			if (!put_call(open_call, stmt->period))
				return false;
		}
		break;

		case ESQL_Command::Close:
		{
			std::string cursor_id = stmt->cursorName;
			ESQLCall close_call(get_call_id("CursorClose"), emit_static);
			close_call.addParameter("SQLCA", BY_REFERENCE);
			close_call.addParameter("\"" + cursor_id + "\" & x\"00\"", BY_REFERENCE);

			if (!put_call(close_call, stmt->period))
				return false;
		}
		break;

		case ESQL_Command::Fetch:
		{
			put_start_exec_sql(false);
			for (cb_res_hostreference_ptr rp : *stmt->res_host_list) {
				ESQLCall rp_call(get_call_id("SetResultParams"), emit_static);
				if (!main_module_driver.field_exists(rp->hostreference.substr(1))) {
					//owner->err_data.err_messages.push_back("Cannot find host variable: " + rp->hostreference.substr(1));
					main_module_driver.error("Cannot find host variable: " + rp->hostreference.substr(1), ERR_MISSING_HOSTVAR);
					return false;
				}

				cb_field_ptr hr = main_module_driver.field_map[rp->hostreference.substr(1)];
				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
				flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

				rp_call.addParameter(f_type, BY_VALUE);
				rp_call.addParameter(f_size, BY_VALUE);
				rp_call.addParameter(f_scale, BY_VALUE);
				rp_call.addParameter(flags, BY_VALUE);
				rp_call.addParameter(rp->hostreference.substr(1), BY_REFERENCE);

				if (!put_call(rp_call, false))
					return false;
			}

			std::string cursor_id = stmt->cursorName;
			ESQLCall fetch_call(get_call_id("CursorFetchOne"), emit_static);
			fetch_call.addParameter("SQLCA", BY_REFERENCE);
			fetch_call.addParameter("\"" + cursor_id + "\" & x\"00\"", BY_REFERENCE);

			if (!put_call(fetch_call, false))
				return false;

			put_end_exec_sql(stmt->period);
		}
		break;

		case ESQL_Command::Commit:
		{
			put_start_exec_sql(false);
			ESQLCall fetch_call(get_call_id("Exec"), emit_static);
			fetch_call.addParameter("SQLCA", BY_REFERENCE);
			fetch_call.addParameter(&main_module_driver, stmt->connectionId);
			fetch_call.addParameter("\"COMMIT\" & x\"00\"", BY_REFERENCE);

			if (!put_call(fetch_call, false))
				return false;

			put_end_exec_sql(stmt->period);
		}
		break;

		case ESQL_Command::Update:
		case ESQL_Command::Delete:
		case ESQL_Command::Insert:
		{
			put_start_exec_sql(false);

			for (cb_hostreference_ptr p : *stmt->host_list) {
				ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);
				if (!main_module_driver.field_exists(p->hostreference.substr(1))) {
					//owner->err_data.err_messages.push_back("Cannot find host variable: " + p->hostreference.substr(1));
					main_module_driver.error("Cannot find host variable: " + p->hostreference.substr(1), ERR_MISSING_HOSTVAR);
					return false;
				}

				cb_field_ptr hr = main_module_driver.field_map[p->hostreference.substr(1)];
				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

				int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
				flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

				p_call.addParameter(f_type, BY_VALUE);
				p_call.addParameter(f_size, BY_VALUE);
				p_call.addParameter(f_scale, BY_VALUE);
				p_call.addParameter(flags, BY_VALUE);
				p_call.addParameter(p->hostreference.substr(1), BY_REFERENCE);

				if (!put_call(p_call, false))
					return false;
			}

			ESQLCall dml_call(get_call_id(stmt->host_list->size() == 0 ? "Exec" : "ExecParams"), emit_static);
			dml_call.addParameter("SQLCA", BY_REFERENCE);
			dml_call.addParameter(&main_module_driver, stmt->connectionId);
			dml_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
			dml_call.addParameter(stmt->host_list->size(), BY_VALUE);

			if  (!put_call(dml_call, false))
				return false;

			put_end_exec_sql(stmt->period);
		}
		break;

		case ESQL_Command::DeclareVar:
			{
				if (stmt->host_list->size() != 1) {
					main_module_driver.error("Cannot find host variable (invalid declaration)", ERR_MISSING_HOSTVAR);
					return false;
				}

				std::string var_name = stmt->host_list->at(0)->hostreference;
				if (!main_module_driver.field_exists(var_name)) {
					main_module_driver.error("Cannot find host variable: " + var_name, ERR_MISSING_HOSTVAR);
					return false;
				}

				cb_field_ptr var = main_module_driver.field_map[var_name];
				if (var->level != 1) {
					main_module_driver.error(string_format("Invalid level for SQL variable, it is %02d, should be 01", var->level), ERR_INVALID_LEVEL, var->defined_at_source_file, var->defined_at_source_line);
					return false;
				}

				uint64_t type_info = var->sql_type;

				uint32_t length = type_info & 0xffffffff;
				uint16_t precision = (length >> 16);
				uint16_t scale = (length & 0xffff);

				int sql_type = (type_info >> 32);

				if (!check_sql_type_compatibility(type_info, var)) {
					std::string msg = string_format("SQL type definition for %s (%s) is not compatible with the COBOL one (%s)", var->sname, "N/A", "N/A");
					//owner->err_data.err_messages.push_back(msg);
					main_module_driver.error(msg, ERR_INCOMPATIBLE_TYPES);
					return false;
				}

				switch (sql_type) {

					case TYPE_SQL_CHAR:
					case TYPE_SQL_VARCHAR:
					case TYPE_SQL_BINARY:
					case TYPE_SQL_VARBINARY:
						{
							int cbl_int_part_len = var->picnsize;

							if (cbl_int_part_len <= 0) {
								if (precision == 0) {
									main_module_driver.error(string_format("Missing length for field %s", var_name), ERR_MISSING_LENGTH, var->defined_at_source_file, var->defined_at_source_line);
									return false;
								}
								else {
									cbl_int_part_len = precision;
								}
							}

							if (!var->is_varlen) {
								put_output_line(AREA_B_CPREFIX + string_format("01 %s PIC X(%d).", var->sname, cbl_int_part_len));

								cb_field_ptr fdata = new cb_field_t();
								fdata->sql_type = sql_type;
								fdata->level = 1;
								fdata->sname = var->sname;
								fdata->pictype = var->pictype != -1 ? var->pictype : (IS_NUMERIC(var->sql_type) ? PIC_NUMERIC : PIC_ALPHANUMERIC);
								fdata->usage = var->usage;
								fdata->picnsize = cbl_int_part_len;
								fdata->scale = 0;
								fdata->parent = nullptr;
								main_module_driver.field_map[fdata->sname] = fdata;
							}
							else {
								put_output_line(AREA_B_CPREFIX + string_format("01 %s.", var->sname));
								put_output_line(AREA_B_CPREFIX + string_format("    49 %s-%s PIC %s.", var->sname, VARLEN_SUFFIX_LENGTH, VARLEN_LENGTH_PIC));
								put_output_line(AREA_B_CPREFIX + string_format("    49 %s-%s PIC X(%d).", var->sname, VARLEN_SUFFIX_DATA, cbl_int_part_len));

								cb_field_ptr flength = new cb_field_t();
								flength->level = 49;
								flength->sname = var->sname + "-" + VARLEN_SUFFIX_LENGTH;
								flength->pictype = PIC_NUMERIC;
								flength->usage = var->usage;
								flength->picnsize = VARLEN_PIC_SZ;
								flength->parent = var;
								var->children = flength;

								cb_field_ptr fdata = new cb_field_t();
								fdata->level = 49;
								fdata->sql_type = sql_type;
								fdata->sname = var->sname + "-" + VARLEN_SUFFIX_DATA;
								fdata->pictype = PIC_ALPHANUMERIC;
								fdata->usage = Usage::None;
								fdata->picnsize = cbl_int_part_len;
								fdata->scale = 0;
								fdata->parent = var;
								var->children->sister = fdata;

								main_module_driver.field_map[flength->sname] = flength;
								main_module_driver.field_map[fdata->sname] = fdata;
							}
						}
						break;

					// TODO: should we use COMP-1/COMP-2 types for float?
					case TYPE_SQL_FLOAT:
					case TYPE_SQL_DECIMAL:
						{
							int cbl_int_part_len = var->picnsize - var->scale;
							int cbl_dec_part_len = var->scale;

							if (cbl_int_part_len <= 0) {
								if (precision == 0) {
									//owner->err_data.err_messages.push_back(string_format("Missing length for field %s", var_name));
									main_module_driver.error(string_format("Missing length for field %s", var_name), ERR_MISSING_LENGTH, var->defined_at_source_file, var->defined_at_source_line);
									return false;
								}
								else {
									cbl_int_part_len = precision;
									cbl_dec_part_len = scale;
								}
							}

							std::string pic = AREA_B_CPREFIX + string_format("01 %s PIC S", var->sname);	// SQL DECIMAL is always signed
							pic += string_format("9(%d)", cbl_int_part_len);
							if (cbl_dec_part_len)
								pic += string_format("V9(%d)", cbl_dec_part_len);

							pic += ".";

							put_output_line(pic);
						}
						break;

					case TYPE_SQL_INT:
						{
							int int_part_len = var->picnsize;
							std::string pic = AREA_B_CPREFIX + string_format("01 %s PIC S9(%d).", var->sname, var->picnsize);	// SQL INTEGER is always signed
							put_output_line(pic);
						}
						break;

					default:
						main_module_driver.error(string_format("Unsupported SQL type (%d)", sql_type), ERR_INVALID_TYPE, var->defined_at_source_file, var->defined_at_source_line);
						return false;
				}
			}
			break;

		case ESQL_Command::Comment:
			/* Do nothing */
			break;

		case ESQL_Command::PrepareStatement:
			{
				ESQLCall ps_call(get_call_id("PrepareStatement"), emit_static);
				ps_call.addParameter("SQLCA", BY_REFERENCE);
				ps_call.addParameter(&main_module_driver, stmt->connectionId);
				ps_call.addParameter("\"" + stmt->statementName + "\" & x\"00\"", BY_REFERENCE);
				if (stmt->statementSource)
					ps_call.addParameter(&main_module_driver, stmt->statementSource);
				else {
					ps_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
					ps_call.addParameter(0, BY_VALUE);
				}
				if (!put_call(ps_call, stmt->period))
					return false;
			}
			break;

		case ESQL_Command::ExecPrepared:
			{
				put_start_exec_sql(false);

				for (cb_hostreference_ptr p : *stmt->host_list) {
					ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);
					if (!main_module_driver.field_exists(p->hostreference.substr(1))) {
						//owner->err_data.err_messages.push_back("Cannot find host variable: " + p->hostreference.substr(1));
						main_module_driver.error("Cannot find host variable: " + p->hostreference.substr(1), ERR_MISSING_HOSTVAR);
						return false;
					}

					cb_field_ptr hr = main_module_driver.field_map[p->hostreference.substr(1)];
					bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

					int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
					flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

					p_call.addParameter(f_type, BY_VALUE);
					p_call.addParameter(f_size, BY_VALUE);
					p_call.addParameter(f_scale, BY_VALUE);
					p_call.addParameter(flags, BY_VALUE);
					p_call.addParameter(p->hostreference.substr(1), BY_REFERENCE);

					if (!put_call(p_call, false))
						return false;
				}

				ESQLCall ep_call(get_call_id("ExecPrepared"), emit_static);
				ep_call.addParameter("SQLCA", BY_REFERENCE);
				ep_call.addParameter(&main_module_driver, stmt->connectionId);
				ep_call.addParameter("\"" + stmt->statementName + "\" & x\"00\"", BY_REFERENCE);
				ep_call.addParameter(stmt->host_list->size(), BY_VALUE);

				if (!put_call(ep_call, false))
					return false;

				put_end_exec_sql(stmt->period);
			}
			break;

		case ESQL_Command::ExecImmediate:
			{
				ESQLCall ei_call(get_call_id("ExecImmediate"), emit_static);
				ei_call.addParameter("SQLCA", BY_REFERENCE);
				ei_call.addParameter(&main_module_driver, stmt->connectionId);
				if (stmt->statementSource)
					ei_call.addParameter(&main_module_driver, stmt->statementSource);
				else {
					ei_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
					ei_call.addParameter(0, BY_VALUE);
				}
				if (!put_call(ei_call, stmt->period))
					return false;
			}
			break;

		case ESQL_Command::BeginDeclareSection:
		case ESQL_Command::EndDeclareSection:
			/* Do nothing (for now) */
			break;

		case ESQL_Command::PassThru:
			{
				put_start_exec_sql(false);

				for (cb_hostreference_ptr p : *stmt->host_list) {
					ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);
					if (!main_module_driver.field_exists(p->hostreference.substr(1))) {
						//owner->err_data.err_messages.push_back("Cannot find host variable: " + p->hostreference.substr(1));
						main_module_driver.error("Cannot find host variable: " + p->hostreference.substr(1), ERR_MISSING_HOSTVAR);
						return false;
					}

					cb_field_ptr hr = main_module_driver.field_map[p->hostreference.substr(1)];
					bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

					int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
					flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

					p_call.addParameter(f_type, BY_VALUE);
					p_call.addParameter(f_size, BY_VALUE);
					p_call.addParameter(f_scale, BY_VALUE);
					p_call.addParameter(flags, BY_VALUE);
					p_call.addParameter(p->hostreference.substr(1), BY_REFERENCE);

					if (!put_call(p_call, false))
						return false;
				}

				ESQLCall dml_call(get_call_id(stmt->host_list->size() == 0 ? "Exec" : "ExecParams"), emit_static);
				dml_call.addParameter("SQLCA", BY_REFERENCE);
				dml_call.addParameter(&main_module_driver, stmt->connectionId);
				dml_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
				dml_call.addParameter(stmt->host_list->size(), BY_VALUE);

				if (!put_call(dml_call, false))
					return false;

				put_end_exec_sql(stmt->period);
			}
			break;

		// If we have intercepted all the cases this should never happen
		// In case it does, should we instead raise an error?
		default:
		{
			//owner->err_messages << "Invalid statement: " + stmt->commandName;
			//return false;
			ESQLCall exec_call(get_call_id("Exec"), emit_static);
			exec_call.addParameter("SQLCA", BY_REFERENCE);
			exec_call.addParameter(&main_module_driver, stmt->connectionId);
			exec_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);

			if (!put_call(exec_call, stmt->period))
				return false;
		}
		break;



	}
	return true;
}

bool TPESQLProcessing::find_working_storage(int *working_begin_line, int *working_end_line)
{
	std::vector<cb_exec_sql_stmt_ptr> *p = main_module_driver.exec_list;

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

std::string TPESQLProcessing::comment_line(const std::string &comment, const std::string &line)
{
	std::string ln = line;
	if (ln.size() < 7)
		ln = rpad(ln, 7);

	ln[6] = '*';

	int m = comment.size() > 6 ? 6 : comment.size();

	ln = comment + ln.substr(m);
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
							case Usage::NativeBinary:
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
							case Usage::NativeBinary:
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
	bool is_implicit_varlen = f->is_varlen;

	if (!is_varlen && !is_implicit_varlen) {
		gethostvarianttype(f, type);
		//*type = f->pictype;
		*size = f->picnsize;
		*scale = f->scale;
	}
	else {
		std::string f_actual_name;
		if (is_varlen) {
			f_actual_name = f->children->sister->sname;

		}
		else {	// is_implicit_varlen
			f_actual_name = f->sname + "-" + VARLEN_SUFFIX_DATA;
		}

		cb_field_ptr f_actual = main_module_driver.field_map[f_actual_name];
		gethostvarianttype(f_actual, type);
		*size = f_actual->picnsize + VARLEN_LENGTH_SZ;
		*scale = f_actual->scale;
	}
	return is_varlen;
}

void TPESQLProcessing::process_sql_query_list()
{
	for (cb_exec_sql_stmt_ptr p : *main_module_driver.exec_list) {
		if (p->sql_list->size()) {
			std::string sql = vector_join(*p->sql_list, ' ');
			ws_query_list.push_back(sql);
		}
	}
}

bool TPESQLProcessing::fixup_declared_vars()
{
	int n = 99999;
	for (auto it = main_module_driver.field_sql_type_info.begin(); it != main_module_driver.field_sql_type_info.end(); ++it) {
		cb_field_ptr var = nullptr;
		std::string var_name = it->first;
		std::tuple<uint64_t, int, int, std::string> d = it->second;
		uint64_t type_info = std::get<0>(d);
		int orig_start_line = std::get<1>(d);
		int orig_end_line = std::get<2>(d);
		std::string orig_src_file = std::get<3>(d);

		uint32_t length = type_info & 0xffffffff;
		uint16_t precision = (length >> 16);
		uint16_t scale = (length & 0xffff);
		int sql_type = (type_info >> 32);

		if (!main_module_driver.field_exists(var_name)) {
			if (precision == 0) {
				//owner->err_data.err_messages.push_back("Cannot find host variable: " + var_name);
				main_module_driver.error("Cannot find host variable: " + var_name, ERR_MISSING_HOSTVAR);
				return false;
			}
			else {
				var = new cb_field_t;
				var->level = 1;
				var->usage = Usage::None;
				var->sname = var_name;
				var->sql_type = type_info;
				var->pictype = -1;
				var->defined_at_source_line = orig_start_line;
				var->defined_at_source_file = orig_src_file;
				main_module_driver.field_map[var_name] = var;
			}
		}

		var = main_module_driver.field_map[var_name];
		if (var->level != 1) {
			std::string msg = string_format("Host variable %s has level %02d, should be 01", var_name, var->level);
			//owner->err_data.err_messages.push_back(msg);
			main_module_driver.error(msg, ERR_INVALID_LEVEL);
			return false;
		}

		var->sql_type = type_info;
		var->is_varlen = IS_VARLEN(sql_type);
		var->usage = IS_BINARY(sql_type) ? Usage::Binary : Usage::None;
		var->picnsize = precision;
		var->scale = scale;

		cb_exec_sql_stmt_ptr stmt = new cb_exec_sql_stmt_t();
		stmt->commandName = ESQL_DECLARE_VAR;
		stmt->src_file = filename_clean_path(var->defined_at_source_file);
		stmt->startLine = var->defined_at_source_line;
		stmt->endLine = var->defined_at_source_line;

		cb_hostreference_ptr p = new cb_hostreference_t();
		p->hostno = n++;
		p->hostreference = var_name;
		p->lineno = var->defined_at_source_line;
		stmt->host_list->push_back(p);
		main_module_driver.exec_list->push_back(stmt);

		stmt = new cb_exec_sql_stmt_t();
		stmt->commandName = ESQL_COMMENT;
		stmt->src_file = filename_clean_path(var->defined_at_source_file);
		stmt->startLine = orig_start_line;
		stmt->endLine = orig_end_line;

		main_module_driver.exec_list->push_back(stmt);
	}

	return true;
}

std::map<uint64_t, uint64_t> &TPESQLProcessing::getBinarySrcLineMap() const
{
	return const_cast<std::map<uint64_t, uint64_t>&>(b_in_to_out);
}

std::map<uint64_t, uint64_t> &TPESQLProcessing::getBinarySrcLineMapReverse() const
{
	return const_cast<std::map<uint64_t, uint64_t>&>(b_out_to_in);
}

bool TPESQLProcessing::build_map_data()
{
	map_collect_files(filemap);

	std::string file;
	int line_in, line_out;
	//uint64_t k, v;

	// in to out map
	for (std::map<std::string, std::string>::const_iterator it = in_to_out.begin(); it != in_to_out.end(); ++it) {
		splitLineEntry(it->first, file, &line_in);
		int fin_id = filemap[file];

		splitLineEntry(it->second, file, &line_out);
		int fout_id = filemap[file];

		uint64_t k = ((uint64_t)fin_id << 32) + line_in;
		uint64_t v = ((uint64_t)fout_id << 32) + line_out;

		b_in_to_out[k] = v;
	}

	// out to in map
	for (std::map<std::string, std::string>::const_iterator it = out_to_in.begin(); it != out_to_in.end(); ++it) {

		splitLineEntry(it->first, file, &line_out);
		int fout_id = filemap[file];

		splitLineEntry(it->second, file, &line_in);
		int fin_id = filemap[file];

		uint64_t k = ((uint64_t)fout_id << 32) + line_out;
		uint64_t v = ((uint64_t)fin_id << 32) + line_in;

		b_out_to_in[k] = v;

	}

	// Variable declaraton source location info

	//mw.addSection("field_map");
	//mw.appendToSectionContents("field_map", main_module_driver.field_map.size());
	//for (auto it = main_module_driver.field_map.begin(); it != main_module_driver.field_map.end(); ++it) {
	//	std::string path = "";
	//	std::string var = it.key();
	//	cb_field_ptr fld = it.value();

	//	cb_field_ptr p = fld;
	//	do {
	//		path = p->sname + ":" + path;
	//		p = p->parent;
	//	} while (p);

	//	if (path.length() > 0)
	//		path = path.left(path.length() - 1);

	//	path = "WS:" + path;

	//	mw.appendToSectionContents("field_map", std::string("%1/%2@%3:%4").arg(fld->sname).arg(path).arg(fld->defined_at_source_file).arg(fld->defined_at_source_line));
	//}

	return true;
}

bool TPESQLProcessing::write_map_file(const std::string &preprocd_file)
{
	map_collect_files(filemap);

	if (opt_no_output)
		return true;

	MapFileWriter mw;
	std::string outfile = filename_change_ext(preprocd_file, ".cbsql.map");

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
	for (std::map<std::string, int>::const_iterator it = filemap.begin(); it != filemap.end(); ++it) {
		mw.appendToSectionContents("filemap", string_format("#%d:%s", it->second, it->first));
	}

	std::string file;
	int line_in, line_out;

	// in to out map
	mw.addSection("in_to_out_map");
	mw.appendToSectionContents("in_to_out_map", in_to_out.size());

	for (std::map<std::string, std::string>::const_iterator it = in_to_out.begin(); it != in_to_out.end(); ++it) {
		splitLineEntry(it->first, file, &line_in);
		int fin_id = filemap[file];

		splitLineEntry(it->second, file, &line_out);
		int fout_id = filemap[file];

		mw.appendToSectionContents("in_to_out_map", string_format("%d@%d:%d@%d", line_in, fin_id, line_out, fout_id));
	}

	// out to in map
	mw.addSection("out_to_in_map");
	mw.appendToSectionContents("out_to_in_map", out_to_in.size());

	for (std::map<std::string, std::string>::const_iterator it = out_to_in.begin(); it != out_to_in.end(); ++it) {

		splitLineEntry(it->first, file, &line_out);
		int fout_id = filemap[file];

		splitLineEntry(it->second, file, &line_in);
		int fin_id = filemap[file];

		mw.appendToSectionContents("out_to_in_map", string_format("%d@%d:%d@%d", line_out, fout_id, line_in, fin_id));
	}

	// Variable declaraton source location info

	mw.addSection("field_map");
	mw.appendToSectionContents("field_map", main_module_driver.field_map.size());
	for (std::map<std::string, cb_field_ptr>::const_iterator it = main_module_driver.field_map.begin(); it != main_module_driver.field_map.end(); ++it) {
		std::string path = "";
		std::string var = it->first;
		cb_field_ptr fld = it->second;

		cb_field_ptr p = fld;
		do {
			path = p->sname + ":" + path;
			p = p->parent;
		} while (p);

		if (path.length() > 0)
			path = path.substr(0, path.length() - 1);

		path = "WS:" + path;

		mw.appendToSectionContents("field_map", string_format("%s/%s@%s:%d", fld->sname, path, fld->defined_at_source_file, fld->defined_at_source_line));
	}

	return mw.writeToFile(outfile);
}

void TPESQLProcessing::add_dependency(const std::string &parent, const std::string &dep_path)
{
	std::vector<std::string> deps = (map_contains< std::string, std::vector<std::string>>(file_dependencies, parent) ? file_dependencies.at(parent) : std::vector<std::string>());
	deps.push_back(dep_path);
	file_dependencies[parent] = deps;
}

bool TPESQLProcessing::is_current_file_included()
{
	return input_file_stack.size() > 1;
}

std::string TPESQLProcessing::getModuleName()
{
	return main_module_driver.program_id;
}

void TPESQLProcessing::splitLineEntry(const std::string &k, std::string &s, int *i)
{
	int p = k.find("@");
	s = k.substr(p + 1);
	*i = std::stoi(k.substr(0, p));
}

void TPESQLProcessing::map_collect_files(std::map<std::string, int> &filemap)
{
	int l;
	int n = 1;
	std::string f;

	if (!in_to_out.size())
		return;

	//std::string s = in_to_out[in_to_out.keys().at(0)];
	std::string s = in_to_out.begin()->second;

	splitLineEntry(s, f, &l);
	filemap[f] = n++;

	std::map<std::string, std::string>::const_iterator it = in_to_out.begin();
	auto end = in_to_out.end();
	while (it != end) {
		splitLineEntry(it->first, f, &l);
		if (!map_contains<std::string, int>(filemap, f))
			filemap[f] = n++;

		splitLineEntry(it->second, f, &l);
		if (!map_contains<std::string, int>(filemap, f))
			filemap[f] = n++;

		++it;
	}
}

std::map<std::string, std::string> &TPESQLProcessing::getSrcLineMap() const
{
	return const_cast<std::map<std::string, std::string>&>(in_to_out);
}

std::map<std::string, std::string> &TPESQLProcessing::getSrcLineMapReverse() const
{
	return const_cast<std::map<std::string, std::string>&>(out_to_in);
}

std::map<std::string, int> &TPESQLProcessing::getFileMap() const
{
	return const_cast<std::map<std::string, int>&>(filemap);
}

std::map<int, std::string> TPESQLProcessing::getReverseFileMap()
{
	std::map<int, std::string> rm;
	for (const std::string& k : map_get_keys(filemap)) {
		int v = filemap[k];
		rm[v] = k;
	}
	return rm;
}

std::map<std::string, cb_field_ptr> &TPESQLProcessing::getVariableDeclarationInfoMap() const
{
	return const_cast<std::map<std::string, cb_field_ptr>&>(main_module_driver.field_map);
}

std::map<std::string, srcLocation> TPESQLProcessing::getParagraphs()
{
	return main_module_driver.paragraphs;
}

std::map<std::string, std::vector<std::string>> TPESQLProcessing::getFileDependencies()
{
	return file_dependencies;
}

bool check_sql_type_compatibility(uint64_t type_info, cb_field_ptr var)
{
	// TODO: implement this
	return true;
}
