/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021,2022 Marco Ridoni

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
#include "limits.h"
#include "linq/linq.hpp"

#include "cobol_var_types.h"

#if defined(_WIN32) && defined(_DEBUG)
#include <Windows.h>
#endif

#define ESQL_CONNECT					"CONNECT"
#define ESQL_CONNECT_RESET				"CONNECT_RESET"
#define ESQL_DISCONNECT					"DISCONNECT"
#define ESQL_CLOSE						"CLOSE"
#define ESQL_COMMIT						"COMMIT"
#define ESQL_ROLLBACK					"ROLLBACK"
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
#define ESQL_WHENEVER					"WHENEVER"

#define BEGIN_DECLARE_SECTION			"HOST_BEGIN"
#define END_DECLARE_SECTION				"HOST_END"

#define AREA_A_PREFIX       "       " // 7 spaces
#define AREA_A_CPREFIX       "GIXSQL " // comment +  7 spaces
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

#define CBL_FIELD_FLAG_AUTOTRIM	(uint32_t)0x200

#define MAP_FILE_FMT_VER ((uint16_t) 0x0100)
#define FLAG_M_BASE					0

#define ERR_NOTDEF_CONVERSION -1

#define DEFAULT_VARLEN_SUFFIX_DATA		"ARR"
#define DEFAULT_VARLEN_SUFFIX_LENGTH	"LEN"

#define SQL_QUERY_BLOCK_SIZE	8191

#define DEFAULT_NO_REC_CODE	100

// These must be in sync with the ones in SqlVar.h
#ifdef USE_VARLEN_16
#define VARLEN_LENGTH_PIC		"9(4) COMP-5"
#define VARLEN_PIC_SZ			4
#define VARLEN_LENGTH_SZ		2
#define VARLEN_LENGTH_T			uint16_t
#define VARLEN_BSWAP			COB_BSWAP_16
#else
#define VARLEN_LENGTH_PIC		"9(8) COMP-5"
#define VARLEN_PIC_SZ			9
#define VARLEN_LENGTH_SZ		4
#define VARLEN_LENGTH_T			uint32_t
#define VARLEN_BSWAP			COB_BSWAP_32
#endif

enum class ESQL_Command
{
	Connect,
	ConnectReset,
	Disconnect,
	Close,
	Commit,
	Rollback,
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
	Whenever,

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
												 { ESQL_COMMIT, ESQL_Command::Commit }, { ESQL_ROLLBACK, ESQL_Command::Rollback },
												 { ESQL_FETCH, ESQL_Command::Fetch }, { ESQL_DELETE, ESQL_Command::Delete },
												 { ESQL_INCFILE, ESQL_Command::Incfile }, { ESQL_INCSQLCA, ESQL_Command::IncSQLCA }, { ESQL_INSERT, ESQL_Command::Insert },
												 { ESQL_OPEN, ESQL_Command::Open }, { ESQL_SELECT, ESQL_Command::Select }, { ESQL_UPDATE, ESQL_Command::Update },
												 { ESQL_WORKING_BEGIN, ESQL_Command::WorkingBegin }, { ESQL_WORKING_END, ESQL_Command::WorkingEnd } ,
												 { ESQL_LINKAGE_BEGIN, ESQL_Command::LinkageBegin }, { ESQL_LINKAGE_END, ESQL_Command::WorkingEnd } ,
												 { ESQL_FILE_BEGIN, ESQL_Command::FileBegin }, { ESQL_FILE_END, ESQL_Command::FileEnd } ,
												 { ESQL_PROCEDURE_DIVISION, ESQL_Command::ProcedureDivision }, { ESQL_DECLARE_TABLE, ESQL_Command::DeclareTable },
												 { ESQL_PREPARE, ESQL_Command::PrepareStatement }, { ESQL_EXEC_PREPARED, ESQL_Command::ExecPrepared },
												 { ESQL_EXEC_IMMEDIATE, ESQL_Command::ExecImmediate}, { ESQL_WHENEVER, ESQL_Command::Whenever}, { ESQL_DECLARE_VAR, ESQL_Command::DeclareVar },
												 { BEGIN_DECLARE_SECTION, ESQL_Command::BeginDeclareSection}, { END_DECLARE_SECTION, ESQL_Command::EndDeclareSection },
												 { ESQL_COMMENT, ESQL_Command::Comment } , { ESQL_IGNORE, ESQL_Command::Ignore }, { ESQL_PASSTHRU, ESQL_Command::PassThru } };

#define CALL_PREFIX	"GIXSQL"
#define TAG_PREFIX	"GIXSQL"

struct esql_whenever_clause_handler_t {
	int action = WHENEVER_ACTION_CONTINUE;
	std::string host_label;
};

static struct esql_whenever_handler_t
{
	esql_whenever_clause_handler_t not_found;
	esql_whenever_clause_handler_t sqlwarning;
	esql_whenever_clause_handler_t sqlerror;
} esql_whenever_handler;

inline std::string TPESQLProcessing::get_call_id(const std::string s)
{
	return CALL_PREFIX + s;
}

TPESQLProcessing::TPESQLProcessing(GixPreProcessor* gpp) : ITransformationStep(gpp)
{
	std::string ps = std::get<std::string>(gpp->getOpt("params_style", std::string("d")));
	if (ps == "d")
		opt_params_style = ESQL_ParameterStyle::DollarPrefix;
	else
		if (ps == "c")
			opt_params_style = ESQL_ParameterStyle::ColonPrefix;
		else
			if (ps == "a")
				opt_params_style = ESQL_ParameterStyle::Anonymous;
			else
				opt_params_style = ESQL_ParameterStyle::Unknown;

	opt_preprocess_copy_files = std::get<bool>(gpp->getOpt("preprocess_copy_files", false));
	opt_emit_static_calls = std::get<bool>(gpp->getOpt("emit_static_calls", false));
	opt_emit_debug_info = std::get<bool>(gpp->getOpt("emit_debug_info", false));
	opt_emit_compat = std::get<bool>(gpp->getOpt("emit_compat", false));
	opt_consolidated_map = std::get<bool>(gpp->getOpt("consolidated_map", false));
	opt_no_output = std::get<bool>(gpp->getOpt("no_output", false));
	opt_emit_map_file = std::get<bool>(gpp->getOpt("emit_map_file", false));
	opt_emit_cobol85 = std::get<bool>(gpp->getOpt("emit_cobol85", false));
	opt_picx_as_varchar = std::get<bool>(gpp->getOpt("picx_as_varchar", false));

	auto vsfxs = std::get<std::string>(gpp->getOpt("varlen_suffixes", std::string()));
	if (vsfxs.empty()) {
		opt_varlen_suffix_len = DEFAULT_VARLEN_SUFFIX_LENGTH;
		opt_varlen_suffix_data = DEFAULT_VARLEN_SUFFIX_DATA;
	}
	else {
		int p = vsfxs.find(",");
		opt_varlen_suffix_len = vsfxs.substr(0, p);
		opt_varlen_suffix_data = vsfxs.substr(p + 1);
	}

	opt_norec_sqlcode = std::get<int>(gpp->getOpt("no_rec_code", DEFAULT_NO_REC_CODE));

	output_line = 0;
	working_begin_line = 0;
	working_end_line = 0;
	current_input_line = 0;
}

bool TPESQLProcessing::run(ITransformationStep* prev_step)
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

	if (opt_params_style == ESQL_ParameterStyle::Unknown) {
		main_module_driver.error("Unsupported or invalid paramter style", ERR_PP_PARAM_ERROR);
		return false;
	}

	main_module_driver.opt_params_style = opt_params_style;
	main_module_driver.opt_preprocess_copy_files = opt_preprocess_copy_files;

	main_module_driver.setCaller(this);

	code_tag = TAG_PREFIX;

	int rc = main_module_driver.parse(owner, input_file);
	if (!rc) {
		rc = outputESQL();
		if (rc == 0)
		add_preprocessed_blocks();
	}



	owner->err_data.err_code = rc;

	return rc == 0;
}

std::string TPESQLProcessing::getOutput(ITransformationStep* me)
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
	std::vector<cb_exec_sql_stmt_ptr>* p = main_module_driver.exec_list;
	for (auto e : *p) {
		char bfr[1024];
		sprintf(bfr, "%04d-%04d : %s\n", e->startLine, e->endLine, e->commandName.c_str());
		OutputDebugStringA(bfr);
	}
#endif

	if (!processNextFile())
		return 1;

	// If we are using "smart" cursor initialization, the block containing the initialization code goes at the end of the program
	// otherwise it has already been output at the start of the PROCEDURE DIVISION
	input_file_stack.push(filename_clean_path(input_file));
	if (!put_cursor_declarations()) {
		main_module_driver.error("An error occurred while generating ESQL cursor declarations", ERR_CRSR_GEN);
		return 1;
	}
	input_file_stack.pop();


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

	std::string f1 = filename_absolute_path(the_file);
	for (int input_line = 1; input_line <= input_lines.size(); input_line++) {
		current_input_line = input_line;

#if defined(_WIN32) && defined(_DEBUG) && defined(VERBOSE)
		char bfr[512];
		sprintf(bfr, "Processing line %d of file %s\n", input_line, the_file.c_str());
		OutputDebugStringA(bfr);
#endif
		std::string cur_line = input_lines.at(input_line - 1);

		bool in_ws = (input_line >= working_begin_line) && (input_line <= working_end_line);

		cb_exec_sql_stmt_ptr exec_sql_stmt = find_exec_sql_stmt(f1, input_line);
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

		case ESQL_Command::ProcedureDivision:

			// PROCEDURE DIVISION can be string_split across several lines if a USING clause is added
			for (int iline = exec_sql_stmt->startLine; iline <= exec_sql_stmt->endLine; iline++) {
				put_output_line(input_lines.at(iline - 1));
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
				main_module_driver.error("Error in ESQL statement", ERR_ALREADY_SET, exec_sql_stmt->src_file, exec_sql_stmt->startLine);
				return false;
			}
		}

		// Special case
		if (exec_sql_stmt->endLine == working_end_line) {
			if (!handle_esql_stmt(ESQL_Command::WorkingEnd, find_esql_cmd(ESQL_WORKING_END, 0), 0)) {
				main_module_driver.error("Error in ESQL statement", ERR_ALREADY_SET, exec_sql_stmt->src_file, exec_sql_stmt->startLine);
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

std::string take_max(std::string& s, int n)
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

static int str_count(const std::string& obj, const std::string& tgt)
{
	int occs = 0;
	std::string::size_type pos = 0;
	while ((pos = obj.find(tgt, pos)) != std::string::npos) {
		++occs;
		pos += tgt.length();
	}
	return occs;
}

bool TPESQLProcessing::put_query_defs()
{
	if (emitted_query_defs)
		return true;

	for (int i = 1; i <= ws_query_list.size(); i++) {
		std::string qry = ws_query_list.at(i - 1);
		int qry_len = qry.length();
		qry = string_replace(qry, "\"", "\"\"");

		put_output_line(code_tag + string_format(" 01  SQ%04d.", i));

		if (!opt_emit_cobol85) {

			int pos = 0;
			int max_sec_len = 30;

			int cur_out_char = 0;

			std::string s;

			int nblocks = qry.size() / SQL_QUERY_BLOCK_SIZE;
			int remainder = qry.size() % SQL_QUERY_BLOCK_SIZE;
			std::vector<std::string> sub_blocks;

			while (!qry.empty()) {
				std::string block = take_max(qry, SQL_QUERY_BLOCK_SIZE);
				int block_size = block.size();
				int block_size_a = block_size - str_count(block, "\"\"");

				std::string first_block = take_max(block, 29);
				put_output_line(code_tag + string_format("     02  FILLER PIC X(%04d) VALUE \"%s\"", block_size_a, first_block));

				while (!block.empty()) {
					std::string sub_block = take_max(block, 59);
					sub_blocks.push_back(sub_block);
				}

				if (sub_blocks.size() > 0 && sub_blocks.back().size() == 59) {
					std::string last_block = sub_blocks.back();
					std::string t = last_block.substr(58, 1);	// get last char
					last_block = last_block.substr(0, 58);		// cut last_block
					sub_blocks.pop_back();
					sub_blocks.push_back(last_block);
					sub_blocks.push_back(t);
				}

				for (auto sub_block : sub_blocks) {
					put_output_line(code_tag + string_format("  &  \"%s\"", sub_block));
				}
				output_lines.back() += ".";

				sub_blocks.clear();
			}
			put_output_line(code_tag + std::string("     02  FILLER PIC X(1) VALUE X\"00\"."));
		}
		else {
			int pos = 0;
			int max_sec_len = 30;

			std::string s;

			while (true) {
				std::string sub_block = take_max(qry, 256);
				int sb_size = sub_block.size();
				if (sub_block.empty())
					break;

				s = take_max(sub_block, 34);
				put_output_line(code_tag + string_format("  02  FILLER PIC X(%04d) VALUE \"%s", sb_size, s));

				while (true) {
					s = take_max(sub_block, 60);
					if (s.empty())
						break;

					put_output_line(code_tag + string_format("-    \"%s", s));
				}

				output_lines.back() += "\".";
			}

			put_output_line(code_tag + std::string("     02  FILLER PIC X(1) VALUE X\"00\"."));
		}
	}

	emitted_query_defs = true;
	return true;
}

void TPESQLProcessing::put_working_storage()
{
	put_output_line(code_tag + std::string(" WORKING-STORAGE SECTION."));
}

bool TPESQLProcessing::put_cursor_declarations()
{
	int f_type, f_size, f_scale;
	bool emit_static = opt_emit_static_calls;
	const char* _areab = AREA_B_CPREFIX;

	put_output_line(code_tag + "*");
	put_output_line(code_tag + "*   ESQL CURSOR DECLARATIONS (START)");

	put_output_line(std::string(_areab) + "GO TO GIX-SKIP-CRSR-INIT.");

	auto cursor_list = startup_items;
	auto other_crsrs = cpplinq::from(*(main_module_driver.exec_list)).where([](cb_exec_sql_stmt_ptr p) { return p->startup_item == 0 && p->commandName == ESQL_SELECT && !p->cursorName.empty(); }).to_vector();
	cursor_list.insert(cursor_list.end(), other_crsrs.begin(), other_crsrs.end());

	for (cb_exec_sql_stmt_ptr stmt : cursor_list) {
		bool has_params = stmt->host_list->size() > 0;

		//if (stmt->statementSource && !stmt->statementSource->is_literal) {
		//	main_module_driver.error("Cursors declared in WORKING-STORAGE cannot use a field as source: " + stmt->cursorName, ERR_CRSR_GEN, stmt->src_abs_path, stmt->startLine);
		//	return false;
		//}

		put_output_line(string_format(AREA_A_CPREFIX "GIXSQL-CI-P-%s.", string_replace(stmt->cursorName, "_", "-")));

		if (has_params) {
			put_start_exec_sql(false);

			if (!put_host_parameters(stmt))
				return false;

			ESQLCall cd_call(get_call_id("CursorDeclareParams"), emit_static);
			cd_call.addParameter("SQLCA", BY_REFERENCE);
			cd_call.addParameter(&main_module_driver, stmt->connectionId);
			cd_call.addParameter("\"" + stmt->cursorName + "\" & x\"00\"", BY_REFERENCE); //& x\"00\"
			cd_call.addParameter(std::to_string(stmt->cursor_hold ? 1 : 0), BY_VALUE);

			//cd_call.addParameter(stmt->sqlName, BY_REFERENCE); //& x\"00\"
			std::string sql_content = this->ws_query_list.at(stmt->sql_query_list_id - 1);
			if (sql_content.size() < 3 || !starts_with(sql_content, "@") || sql_content.at(1) != ':') {
				cd_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
				cd_call.addParameter(0, BY_VALUE);
			}
			else {
				std::string var_name = sql_content.substr(2);
				if (!main_module_driver.field_exists(var_name)) {
					main_module_driver.error("Cannot find host variable: " + var_name, ERR_MISSING_HOSTVAR, stmt->src_abs_path, stmt->startLine);
					return false;
				}
				cd_call.addParameter(var_name, BY_REFERENCE);
				auto hr = main_module_driver.field_map[var_name];
				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);
				cd_call.addParameter(f_size * (is_varlen ? -1 : 1), BY_VALUE);
			}

			cd_call.addParameter(std::to_string(stmt->host_list->size()), BY_VALUE);

			if (!put_call(cd_call, false))
				return false;

			put_end_exec_sql(false);

			put_whenever_handler(stmt->period);
		}
		else {
			ESQLCall cd_call(get_call_id("CursorDeclare"), emit_static);
			cd_call.addParameter("SQLCA", BY_REFERENCE);
			cd_call.addParameter(&main_module_driver, stmt->connectionId);
			cd_call.addParameter("\"" + stmt->cursorName + "\" & x\"00\"", BY_REFERENCE);
			cd_call.addParameter(stmt->cursor_hold, BY_VALUE);

			std::string sql_content = this->ws_query_list.at(stmt->sql_query_list_id - 1);
			if (sql_content.size() < 3 || !starts_with(sql_content, "@") || sql_content.at(1) != ':') {
				cd_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
				cd_call.addParameter(0, BY_VALUE);
			}
			else {
				std::string var_name = sql_content.substr(2);
				if (!main_module_driver.field_exists(var_name)) {
					main_module_driver.error("Cannot find host variable: " + var_name, ERR_MISSING_HOSTVAR, stmt->src_abs_path, stmt->startLine);
					return false;
				}
				cd_call.addParameter(var_name, BY_REFERENCE);
				auto hr = main_module_driver.field_map[var_name];
				bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);
				cd_call.addParameter(f_size * (is_varlen ? -1 : 1), BY_VALUE);
			}

			if (!put_call(cd_call, false))
				return false;

			put_whenever_handler(stmt->period);
		}
	}

	put_output_line(AREA_A_CPREFIX "GIX-SKIP-CRSR-INIT.");

	put_output_line(code_tag + "*");
	put_output_line(code_tag + "*   ESQL CURSOR DECLARATIONS (END)");
	return true;
}

bool TPESQLProcessing::put_call(const ESQLCall& c, bool terminate_with_period, int indent_level)
{

	if (c.hasError()) {
		owner->err_data.err_messages.push_back(c.error());
		return false;
	}

	auto lines = c.format(indent_level);

	// The END-CALL statement was added by the format method, we just need (in case) to add the period
	if (terminate_with_period) {
		lines.back() = lines.back() + ".";
	}

	for (auto ln : lines) {
		put_output_line(ln);
	}

	return true;
}

cb_exec_sql_stmt_ptr TPESQLProcessing::find_exec_sql_stmt(const std::string f1, int i)
{
	std::vector<cb_exec_sql_stmt_ptr>* p = main_module_driver.exec_list;
	for (auto e : *p) {
		std::string f2 = e->src_abs_path;
		if (f1 == f2 && (e->startLine <= i && e->endLine >= i))
			return e;
	}

	return NULL;
}

cb_exec_sql_stmt_ptr TPESQLProcessing::find_esql_cmd(std::string cmd, int idx)
{
	int n = 0;
	std::vector<cb_exec_sql_stmt_ptr>* p = main_module_driver.exec_list;
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

void TPESQLProcessing::put_output_line(const std::string& line)
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
	int f_type, f_size, f_scale;
	bool emit_static = opt_emit_static_calls;

	if (stmt->startup_item)
		return true;

	int gen_block_start = output_line;

	switch (cmd) {

	case ESQL_Command::WorkingBegin:
		put_working_storage();
		break;

	case ESQL_Command::DeclareTable:
		// Do nothing
		break;

	case ESQL_Command::WorkingEnd:
		if (!put_query_defs())
			return false;

		// Cursor initialization flags (if requested)
		put_smart_cursor_init_flags();
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
		connect_call.addParameter(&main_module_driver, stmt->conninfo->dbname);
		connect_call.addParameter(&main_module_driver, stmt->conninfo->username);
		connect_call.addParameter(&main_module_driver, stmt->conninfo->password);

		if (!put_call(connect_call, false))
			return false;

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::ConnectReset:
	{
		ESQLCall connect_call(get_call_id("ConnectReset"), emit_static);
		connect_call.addParameter("SQLCA", BY_REFERENCE);
		connect_call.addParameter(&main_module_driver, stmt->connectionId);

		if (!put_call(connect_call, false))
			return false;

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::Disconnect:
	{
		ESQLCall connect_call(get_call_id("ConnectReset"), emit_static);
		connect_call.addParameter("SQLCA", BY_REFERENCE);
		connect_call.addParameter(&main_module_driver, stmt->connectionId);

		if (!put_call(connect_call, false))
			return false;

		put_whenever_handler(stmt->period);
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

		if (stmt->cursorName.empty()) {
			int res_params_count = 0;

			put_start_exec_sql(false);

			if (!put_res_host_parameters(stmt, &res_params_count))
				return false;

			if (!put_host_parameters(stmt))
				return false;

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
				select_call.addParameter(res_params_count, BY_VALUE);
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
				select_call.addParameter(0, BY_VALUE);
				if (stmt->host_list->size())
					select_call.addParameter(stmt->host_list->size(), BY_VALUE);

				if (!put_call(select_call, false))
					return false;
			}
			put_end_exec_sql(false);

			put_whenever_handler(stmt->period);
		}
	}
	break;

	case ESQL_Command::Open:
	{
		bool is_crsr_startup_item = cpplinq::from(startup_items).where([stmt](cb_exec_sql_stmt_ptr p) { return p->cursorName == stmt->cursorName; }).to_vector().size() > 0;

		// We need to add a check only if the cursor has been declared in the WORKING-STORAGE section
		std::string crsr_init_var = "GIXSQL-CI-F-" + string_replace(stmt->cursorName, "_", "-");
		put_smart_cursor_init_check(stmt->cursorName);
		put_output_line(string_format(AREA_B_CPREFIX "IF %s = 'X' THEN", crsr_init_var));


		std::string cursor_id = stmt->cursorName;
		ESQLCall open_call(get_call_id("CursorOpen"), emit_static);
		open_call.addParameter("SQLCA", BY_REFERENCE);
		open_call.addParameter("\"" + cursor_id + "\" & x\"00\"", BY_REFERENCE);

		if (!put_call(open_call, false, 1))
			return false;

		put_output_line(string_format(AREA_B_CPREFIX "END-IF"));

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::Close:
	{
		//if (opt_smart_crsr_init) {
		//	std::string crsr_init_var = "GIXSQL-CI-F-" + string_replace(stmt->cursorName, "_", "-");
		//	put_smart_cursor_init_check(stmt->cursorName);
		//	put_output_line(string_format(AREA_B_CPREFIX "IF %s = 'X' THEN", crsr_init_var));
		//}

		std::string cursor_id = stmt->cursorName;
		ESQLCall close_call(get_call_id("CursorClose"), emit_static);
		close_call.addParameter("SQLCA", BY_REFERENCE);
		close_call.addParameter("\"" + cursor_id + "\" & x\"00\"", BY_REFERENCE);

		if (!put_call(close_call, false))
			return false;

		//if (opt_smart_crsr_init) {
		//	put_output_line(string_format(AREA_B_CPREFIX "END-IF"));
		//}

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::Fetch:
	{
		//if (opt_smart_crsr_init) {
		//	std::string crsr_init_var = "GIXSQL-CI-F-" + string_replace(stmt->cursorName, "_", "-");
		//	put_smart_cursor_init_check(stmt->cursorName);
		//	put_output_line(string_format(AREA_B_CPREFIX "IF %s = 'X' THEN", crsr_init_var));
		//}

		put_start_exec_sql(false);

		int res_params_count = 0;

		for (cb_res_hostreference_ptr rp : *stmt->res_host_list) {

			if (!main_module_driver.field_exists(rp->hostreference.substr(1))) {
				main_module_driver.error("Cannot find host variable: " + rp->hostreference.substr(1), ERR_MISSING_HOSTVAR, stmt->src_abs_path, rp->lineno);
				return false;
			}

			cb_field_ptr hr = main_module_driver.field_map[rp->hostreference.substr(1)];
			bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

			// Support for group items used as host variables in SELECT statements
			// They are decomposed into their sub-elements
			if (f_type == COBOL_TYPE_GROUP && !is_varlen) {

				if (hr->group_levels_count != 1) {
					main_module_driver.error("Nested levels not allowed in group variable: " + rp->hostreference.substr(1), ERR_MISSING_HOSTVAR, stmt->src_abs_path, rp->lineno);
					return false;
				}

				cb_field_ptr pp = hr->children;
				if (!pp) {
					main_module_driver.error("Inconsistent data for group field : " + hr->sname, ERR_MISSING_HOSTVAR, stmt->src_abs_path, rp->lineno);
					return false;
				}

				while (pp) {

					ESQLCall pp_call(get_call_id("SetResultParams"), emit_static);
					int pp_flags = (pp->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

					int pp_type = 0, pp_size = 0, pp_scale = 0;
					bool pp_is_varlen = get_actual_field_data(pp, &pp_type, &pp_size, &pp_scale);
					if (pp_is_varlen) {
						main_module_driver.error("Inconsistent data for group field member: " + pp->sname, ERR_MISSING_HOSTVAR, stmt->src_abs_path, rp->lineno);
						return false;
					}

					pp_call.addParameter(pp_type, BY_VALUE);
					pp_call.addParameter(pp_size, BY_VALUE);
					pp_call.addParameter(pp_scale > 0 ? -pp_scale : 0, BY_VALUE);
					pp_call.addParameter(pp_flags, BY_VALUE);
					pp_call.addParameter(pp->sname, BY_REFERENCE);

					if (!put_call(pp_call, false))
						return false;

					res_params_count++;

					pp = pp->sister;
				}

			}
			else {
				ESQLCall rp_call(get_call_id("SetResultParams"), emit_static);

				int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
				flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

				rp_call.addParameter(f_type, BY_VALUE);
				rp_call.addParameter(f_size, BY_VALUE);
				rp_call.addParameter(f_scale > 0 ? -f_scale : 0, BY_VALUE);
				rp_call.addParameter(flags, BY_VALUE);
				rp_call.addParameter(rp->hostreference.substr(1), BY_REFERENCE);

				if (!put_call(rp_call, false))
					return false;

				res_params_count++;

			}
		}

		std::string cursor_id = stmt->cursorName;
		ESQLCall fetch_call(get_call_id("CursorFetchOne"), emit_static);
		fetch_call.addParameter("SQLCA", BY_REFERENCE);
		fetch_call.addParameter("\"" + cursor_id + "\" & x\"00\"", BY_REFERENCE);

		if (!put_call(fetch_call, false))
			return false;

		put_end_exec_sql(false);

		//if (opt_smart_crsr_init) {
		//	put_output_line(string_format(AREA_B_CPREFIX "END-IF"));
		//}

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::Commit:
	{
		// Note: RELEASE not supported, in case check the stmt->transaction_release flag
		put_start_exec_sql(false);
		ESQLCall fetch_call(get_call_id("Exec"), emit_static);
		fetch_call.addParameter("SQLCA", BY_REFERENCE);
		fetch_call.addParameter(&main_module_driver, stmt->connectionId);
		fetch_call.addParameter("\"COMMIT\" & x\"00\"", BY_REFERENCE);

		if (!put_call(fetch_call, false))
			return false;

		put_end_exec_sql(false);

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::Rollback:
	{
		// Note: RELEASE not supported, in case check the stmt->transaction_release flag
		put_start_exec_sql(false);
		ESQLCall fetch_call(get_call_id("Exec"), emit_static);
		fetch_call.addParameter("SQLCA", BY_REFERENCE);
		fetch_call.addParameter(&main_module_driver, stmt->connectionId);
		fetch_call.addParameter("\"ROLLBACK\" & x\"00\"", BY_REFERENCE);

		if (!put_call(fetch_call, false))
			return false;

		put_end_exec_sql(false);

		put_whenever_handler(stmt->period);
	}
	break;


	case ESQL_Command::Update:
	case ESQL_Command::Delete:
	case ESQL_Command::Insert:
	{
		put_start_exec_sql(false);

		int sql_params_count = 0;

		// Special case: we cannot use the put_host_parameters method because we need to handle group variables
		for (cb_hostreference_ptr p : *stmt->host_list) {

			if (!main_module_driver.field_exists(p->hostreference.substr(1))) {
				main_module_driver.error("Cannot find host variable: " + p->hostreference.substr(1), ERR_MISSING_HOSTVAR, stmt->src_abs_path, p->lineno);
				return false;
			}

			cb_field_ptr hr = main_module_driver.field_map[p->hostreference.substr(1)];
			bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

			// Support for group items used as host variables in INSERT statements
			// They are decomposed into their sub-elements
			if (cmd == ESQL_Command::Insert && f_type == COBOL_TYPE_GROUP && !is_varlen) {
				if (hr->group_levels_count != 1) {
					main_module_driver.error("Nested levels not allowed in group variable: " + p->hostreference.substr(1), ERR_MISSING_HOSTVAR, stmt->src_abs_path, p->lineno);
					return false;
				}

				cb_field_ptr pp = hr->children;
				if (!pp) {
					main_module_driver.error("Inconsistent data for group field : " + hr->sname, ERR_MISSING_HOSTVAR, stmt->src_abs_path, p->lineno);
					return false;
				}

				while (pp) {

					ESQLCall pp_call(get_call_id("SetSQLParams"), emit_static);
					int pp_flags = (pp->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

					uint32_t _type, _precision;
					uint16_t _scale;
					uint8_t _flags;
					decode_sql_type_info(hr->sql_type, &_type, &_precision, &_scale, &_flags);
					if (HAS_PICX_AS_VARCHAR(_flags) || opt_picx_as_varchar)
						pp_flags |= CBL_FIELD_FLAG_AUTOTRIM;

					int pp_type = 0, pp_size = 0, pp_scale = 0;
					bool pp_is_varlen = get_actual_field_data(pp, &pp_type, &pp_size, &pp_scale);
					if (pp_is_varlen) {
						main_module_driver.error("Inconsistent data for group field member: " + pp->sname, ERR_MISSING_HOSTVAR, stmt->src_abs_path, p->lineno);
						return false;
					}

					pp_call.addParameter(pp_type, BY_VALUE);
					pp_call.addParameter(pp_size, BY_VALUE);
					pp_call.addParameter(pp_scale > 0 ? -pp_scale : 0, BY_VALUE);
					pp_call.addParameter(pp_flags, BY_VALUE);

					pp_call.addParameter(pp->sname, BY_REFERENCE);

					if (!put_call(pp_call, false))
						return false;

					sql_params_count++;

					pp = pp->sister;
				}

			}
			else {
				ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);
				int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
				flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

				uint32_t _type, _precision;
				uint16_t _scale;
				uint8_t _flags;
				decode_sql_type_info(hr->sql_type, &_type, &_precision, &_scale, &_flags);
				if (HAS_PICX_AS_VARCHAR(_flags) || opt_picx_as_varchar)
					flags |= CBL_FIELD_FLAG_AUTOTRIM;

				p_call.addParameter(f_type, BY_VALUE);
				p_call.addParameter(f_size, BY_VALUE);
				p_call.addParameter(f_scale > 0 ? -f_scale : 0, BY_VALUE);
				p_call.addParameter(flags, BY_VALUE);
				p_call.addParameter(p->hostreference.substr(1), BY_REFERENCE);

				if (!put_call(p_call, false))
					return false;

				sql_params_count++;
			}
		}

		ESQLCall dml_call(get_call_id(stmt->host_list->size() == 0 ? "Exec" : "ExecParams"), emit_static);
		dml_call.addParameter("SQLCA", BY_REFERENCE);
		dml_call.addParameter(&main_module_driver, stmt->connectionId);
		dml_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
		dml_call.addParameter(sql_params_count, BY_VALUE);

		if (!put_call(dml_call, false))
			return false;

		put_end_exec_sql(false);

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::DeclareVar:
	{
		if (stmt->host_list->size() != 1) {
			main_module_driver.error("Cannot find host variable (invalid declaration)", ERR_MISSING_HOSTVAR, stmt->src_abs_path, stmt->startLine);
			return false;
		}

		std::string var_name = stmt->host_list->at(0)->hostreference;
		if (!main_module_driver.field_exists(var_name)) {
			main_module_driver.error("Cannot find host variable: " + var_name, ERR_MISSING_HOSTVAR, stmt->src_abs_path, stmt->startLine);
			return false;
		}

		cb_field_ptr var = main_module_driver.field_map[var_name];
		if (var->level != 1) {
			main_module_driver.error(string_format("Invalid level for SQL variable, it is %02d, should be 01", var->level), ERR_INVALID_LEVEL, var->defined_at_source_file, var->defined_at_source_line);
			return false;
		}

		uint64_t type_info = var->sql_type;

		uint32_t sql_type, precision;
		uint16_t scale;
		uint8_t flags;
		decode_sql_type_info(type_info, &sql_type, &precision, &scale, &flags);

#ifdef USE_VARLEN_16
		if (precision > USHRT_MAX) {
			std::string msg = string_format("Unsupported field length (%d > %d)", precision, USHRT_MAX);
			main_module_driver.error(msg, ERR_INCOMPATIBLE_TYPES, stmt->src_abs_path, stmt->startLine);
			return false;
		}
#endif

		if (!check_sql_type_compatibility(type_info, var)) {
			std::string msg = string_format("SQL type definition for %s (%s) is not compatible with the COBOL one (%s)", var->sname, "N/A", "N/A");
			main_module_driver.error(msg, ERR_INCOMPATIBLE_TYPES, stmt->src_abs_path, stmt->startLine);
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

			if (HAS_FLAG_EMIT_VAR(flags)) {
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
					put_output_line(AREA_B_CPREFIX + string_format("    49 %s-%s PIC %s.", var->sname, opt_varlen_suffix_len, VARLEN_LENGTH_PIC));
					put_output_line(AREA_B_CPREFIX + string_format("    49 %s-%s PIC X(%d).", var->sname, opt_varlen_suffix_data, cbl_int_part_len));

					cb_field_ptr flength = new cb_field_t();
					flength->level = 49;
					flength->sname = var->sname + "-" + opt_varlen_suffix_len;
					flength->pictype = PIC_NUMERIC;
					flength->usage = var->usage;
					flength->picnsize = VARLEN_PIC_SZ;
					flength->parent = var;
					var->children = flength;

					cb_field_ptr fdata = new cb_field_t();
					fdata->level = 49;
					fdata->sql_type = sql_type;
					fdata->sname = var->sname + "-" + opt_varlen_suffix_data;
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
			else {
				var->pictype = PIC_ALPHANUMERIC;
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

			if (HAS_FLAG_EMIT_VAR(flags)) {
				put_output_line(pic);
			}
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
			main_module_driver.error(string_format("Unsupported SQL type (ID: %d)", sql_type), ERR_INVALID_TYPE, var->defined_at_source_file, var->defined_at_source_line);
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
		if (stmt->statementSource) {	// statement source is a variable, we must check its type
			auto sv_name = stmt->statementSource->name.substr(1);
			if (!main_module_driver.field_exists(sv_name)) {
				main_module_driver.error("Cannot find host variable: " + sv_name, ERR_MISSING_HOSTVAR, stmt->src_abs_path, stmt->startLine);
				return false;
			}
			cb_field_ptr sv = main_module_driver.field_map[sv_name];
			bool is_varlen = get_actual_field_data(sv, &f_type, &f_size, &f_scale);
			// If is_varlen is true, we are pointing to a "variable length group", which is fine.
			// We pass the group and the runtime library will handle the "actual" data.
			if (!is_varlen) {
				if (sv->pictype != PIC_ALPHANUMERIC) {
					main_module_driver.error("Unsupported type for host variable: " + sv_name, ERR_INVALID_TYPE, stmt->src_abs_path, stmt->startLine);
					return false;
				}
			}

			int sz = !is_varlen ? 0 : f_size;
			ps_call.addParameter(&main_module_driver, stmt->statementSource, sz);
		}
		else {
			ps_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
			ps_call.addParameter(0, BY_VALUE);
		}

		if (!put_call(ps_call, false))
			return false;

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::ExecPrepared:
	{
		put_start_exec_sql(false);

		bool is_exec_into = stmt->res_host_list->size() > 0;
		int res_params_count = 0;

		if (!put_host_parameters(stmt))
			return false;

		if (!put_res_host_parameters(stmt, &res_params_count))
			return false;

		ESQLCall ep_call(get_call_id(!is_exec_into ? "ExecPrepared" : "ExecPreparedInto"), emit_static);
		ep_call.addParameter("SQLCA", BY_REFERENCE);
		ep_call.addParameter(&main_module_driver, stmt->connectionId);
		ep_call.addParameter("\"" + stmt->statementName + "\" & x\"00\"", BY_REFERENCE);
		ep_call.addParameter(stmt->host_list->size(), BY_VALUE);
		if (is_exec_into)
			ep_call.addParameter(res_params_count, BY_VALUE);

		if (!put_call(ep_call, false))
			return false;

		put_end_exec_sql(false);

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::ExecImmediate:
	{
		ESQLCall ei_call(get_call_id("ExecImmediate"), emit_static);
		ei_call.addParameter("SQLCA", BY_REFERENCE);
		ei_call.addParameter(&main_module_driver, stmt->connectionId);
		if (stmt->statementSource) {	// statement source is a variable, we must check its type
			auto sv_name = stmt->statementSource->name.substr(1);
			if (!main_module_driver.field_exists(sv_name)) {
				main_module_driver.error("Cannot find host variable: " + sv_name, ERR_MISSING_HOSTVAR, stmt->src_abs_path, stmt->startLine);
				return false;
			}
			cb_field_ptr sv = main_module_driver.field_map[sv_name];
			bool is_varlen = get_actual_field_data(sv, &f_type, &f_size, &f_scale);
			// If is_varlen is true, we are pointing to a "variable length group", which is fine.
			// We pass the group and the runtime library will handle the "actual" data.
			if (!is_varlen) {
				if (sv->pictype != PIC_ALPHANUMERIC) {
					main_module_driver.error("Unsupported type for host variable: " + sv_name, ERR_INVALID_TYPE, stmt->src_abs_path, stmt->startLine);
					return false;
				}
			}

			int sz = !is_varlen ? 0 : f_size;
			ei_call.addParameter(&main_module_driver, stmt->statementSource, sz);
		}
		else {
			ei_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
			ei_call.addParameter(0, BY_VALUE);
		}

		if (!put_call(ei_call, false))
			return false;

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::BeginDeclareSection:
	case ESQL_Command::EndDeclareSection:
		/* Do nothing (for now) */
		break;

	case ESQL_Command::PassThru:
	{
		put_start_exec_sql(false);

		if (!put_host_parameters(stmt))
			return false;

		ESQLCall dml_call(get_call_id(stmt->host_list->size() == 0 ? "Exec" : "ExecParams"), emit_static);
		dml_call.addParameter("SQLCA", BY_REFERENCE);
		dml_call.addParameter(&main_module_driver, stmt->connectionId);
		dml_call.addParameter(string_format("SQ%04d", stmt->sql_query_list_id), BY_REFERENCE);
		dml_call.addParameter(stmt->host_list->size(), BY_VALUE);

		if (!put_call(dml_call, false))
			return false;

		put_end_exec_sql(false);

		put_whenever_handler(stmt->period);
	}
	break;

	case ESQL_Command::Whenever:
	{
		if (!stmt->whenever_data) {
			main_module_driver.error("Inconsistent data for WHENEVER statement", ERR_INVALID_DATA, stmt->src_abs_path, stmt->startLine);
			return false;
		}

		esql_whenever_clause_handler_t* clause_handler = nullptr;

		switch (stmt->whenever_data->clause) {
		case WHENEVER_CLAUSE_NOT_FOUND:
			clause_handler = &esql_whenever_handler.not_found;
			break;

		case WHENEVER_CLAUSE_SQLWARNING:
			clause_handler = &esql_whenever_handler.sqlwarning;
			break;

		case WHENEVER_CLAUSE_SQLERROR:
			clause_handler = &esql_whenever_handler.sqlerror;
			break;
		}

		if (!clause_handler) {
			main_module_driver.error("Inconsistent data for WHENEVER statement", ERR_INVALID_DATA, stmt->src_abs_path, stmt->startLine);
			return false;
		}

		clause_handler->action = stmt->whenever_data->action;
		clause_handler->host_label = stmt->whenever_data->host_label;
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

	int gen_block_end = output_line;

	generated_blocks[stmt] = std::tuple(gen_block_start + 1, gen_block_end);

	return true;
}

bool TPESQLProcessing::find_working_storage(int* working_begin_line, int* working_end_line)
{
	std::vector<cb_exec_sql_stmt_ptr>* p = main_module_driver.exec_list;

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

std::string TPESQLProcessing::comment_line(const std::string& comment, const std::string& line)
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

int gethostvarianttype(cb_field_ptr p, int* type)
{
	int tmp_type = ERR_NOTDEF_CONVERSION;

	if (p->pictype != 0) {
		switch (p->pictype) {
		case PIC_ALPHANUMERIC:
			tmp_type = COBOL_TYPE_ALPHANUMERIC;
			break;
		case PIC_NATIONAL:
			tmp_type = COBOL_TYPE_NATIONAL;
			break;
		case PIC_NUMERIC:
			if (p->have_sign) {
				if (p->usage != Usage::None) {
					switch (p->usage) {
					case Usage::Packed:
						tmp_type = COBOL_TYPE_SIGNED_NUMBER_PD;
						break;
					case Usage::Binary:
					case Usage::NativeBinary:
						tmp_type = COBOL_TYPE_SIGNED_BINARY;
						break;
					default:
						return ERR_NOTDEF_CONVERSION;
					}
				}
				else if (p->sign_leading) {
					if (p->separate) {
						tmp_type = COBOL_TYPE_SIGNED_NUMBER_LS;
					}
					else {
						tmp_type = COBOL_TYPE_SIGNED_NUMBER_LC;
					}
				}
				else {
					if (p->separate) {
						tmp_type = COBOL_TYPE_SIGNED_NUMBER_TS;
					}
					else {
						tmp_type = COBOL_TYPE_SIGNED_NUMBER_TC;
					}
				}
			}
			else {
				if (p->usage != Usage::None) {
					switch (p->usage) {
					case Usage::Packed:
						tmp_type = COBOL_TYPE_UNSIGNED_NUMBER_PD;
						break;
					case Usage::Binary:
					case Usage::NativeBinary:
						tmp_type = COBOL_TYPE_UNSIGNED_BINARY;
						break;
					default:
						return ERR_NOTDEF_CONVERSION;
					}
				}
				else {
					tmp_type = COBOL_TYPE_UNSIGNED_NUMBER;
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
			tmp_type = COBOL_TYPE_FLOAT;
			break;
		case Usage::Double:
			tmp_type = COBOL_TYPE_DOUBLE;
			break;
		default:
			return ERR_NOTDEF_CONVERSION;
		}
		*type = tmp_type;
		return 0;
	}

	if (p->children) {
		*type = COBOL_TYPE_GROUP;
		return 0;
	}

	return ERR_NOTDEF_CONVERSION;
}

unsigned int compute_field_size(cb_field_ptr f)
{
	if (!f || !f->pictype)
		return 0;

	switch (f->usage) {
	case Usage::None:
		return f->picnsize;

	case Usage::Packed:
		return (f->picnsize / 2) + 1;

	case Usage::Float:
		return sizeof(float);

	case Usage::Double:
		return sizeof(double);

	case Usage::NativeBinary:
	case Usage::Binary:
		if (f->picnsize == 1 || f->picnsize == 2) {	// 1 byte
			return 1;
		}
		else {

			if (f->picnsize == 3 || f->picnsize == 4) {	// 2 bytes
				return 2;
			}
			else {
				if (f->picnsize >= 5 || f->picnsize <= 9) {	// 4 bytes
					return 4;
				}
				else {
					if (f->picnsize >= 10 || f->picnsize <= 18) {	// 8 bytes
						return 8;
					}
					else {
						// Should never happen
						return 0;
					}
				}

			}
		}
		break;

	}
	return 0;
}

void compute_group_size(cb_field_ptr root, cb_field_ptr f, int* size)
{
	if (!f)
		return;

	if (f->pictype != 0) {
		*size += compute_field_size(f);
	}

	if (f->parent) {	// only if we are not a top-level group
		cb_field_ptr s = f->sister;

		if (s) {
			compute_group_size(root, s, size);
		}
	}

	cb_field_ptr c = f->children;
	if (c)
		root->group_levels_count++;

	while (c) {
		compute_group_size(root, c, size);
		c = c->children;
	}

}

bool TPESQLProcessing::get_actual_field_data(cb_field_ptr f, int* type, int* size, int* scale)
{
	bool is_varlen = is_var_len_group(f);
	bool is_explicit_varlen = f->is_varlen;

	if (!is_varlen && !is_explicit_varlen) {
		gethostvarianttype(f, type);
		if (*type == COBOL_TYPE_GROUP) {
			*size = 0;
			f->group_levels_count = 0;
			compute_group_size(f, f, size);
		}
		else {
			*size = f->picnsize;
		}
		*scale = f->scale;
	}
	else {
		std::string f_actual_name;
		if (is_varlen) {
			f_actual_name = f->children->sister->sname;

		}
		else {	// is_implicit_varlen
			f_actual_name = f->sname + "-" + opt_varlen_suffix_data;
		}

		cb_field_ptr f_actual = main_module_driver.field_map[f_actual_name];

		if (f_actual) {
			gethostvarianttype(f_actual, type);
			*size = f_actual->picnsize + VARLEN_LENGTH_SZ;
			*scale = f_actual->scale;
		}
		else {
			*type = PIC_ALPHANUMERIC;
			*size = f->picnsize + VARLEN_LENGTH_SZ;
			*scale = f->scale;
		}

	}
	return is_varlen;
}

std::string TPESQLProcessing::process_sql_query_item(const std::vector<std::string>& input_sql_list)
{
	bool in_single_quoted_string = false;
	bool in_double_quoted_string = false;

	std::string sql = "";
	std::vector <std::string> items;

	// We need to handle placeholders for group items passed as host variables
	for (std::vector< std::string>::const_iterator it = input_sql_list.begin(); it != input_sql_list.end(); ++it) {
		std::string item = *it;

		if (starts_with(item, "@[") && ends_with(item, "]")) {
			item = item.substr(2);
			item = item.substr(0, item.length() - 1);
		}

		items.push_back(item);
	}

	for (std::vector<std::string>::const_iterator it = items.begin(); it != items.end(); ++it) {
		std::string item = *it;

		for (auto itc = item.begin(); itc != item.end(); ++itc) {
			char c = *itc;

			if (c == '"')
				in_double_quoted_string = !in_double_quoted_string;

			if (c == '\'')
				in_single_quoted_string = !in_single_quoted_string;

			sql += c;
		}

		if (!in_single_quoted_string && !in_double_quoted_string)
			sql += ' ';
	}
	trim(sql);
	return sql;
}

void TPESQLProcessing::process_sql_query_list()
{
	for (cb_exec_sql_stmt_ptr p : *main_module_driver.exec_list) {
		if (p->sql_list->size()) {
			std::string sql = process_sql_query_item(*p->sql_list);
			sql = string_replace_regex(sql, "[\\r\\n\\t]", " ");
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

		uint32_t sql_type, precision;
		uint16_t scale;
		uint8_t flags;
		decode_sql_type_info(type_info, &sql_type, &precision, &scale, &flags);

		if (HAS_FLAG_EMIT_VAR(flags)) {

			if (!main_module_driver.field_exists(var_name)) {
				if (precision == 0) {
					main_module_driver.error("Cannot find host variable: " + var_name, ERR_MISSING_HOSTVAR, orig_src_file, orig_start_line);
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
		}

		if (!main_module_driver.field_exists(var_name)) {
			main_module_driver.error("Cannot find host variable: " + var_name, ERR_MISSING_HOSTVAR, orig_src_file, orig_start_line);
			return false;
		}

		var = main_module_driver.field_map[var_name];
		if (var->level != 1) {
			std::string msg = string_format("Host variable %s has level %02d, should be 01", var_name, var->level);
			//owner->err_data.err_messages.push_back(msg);
			main_module_driver.error(msg, ERR_INVALID_LEVEL);
			return false;
		}

		if (!HAS_FLAG_EMIT_VAR(flags) && IS_VARLEN(sql_type) && !IS_BINARY(sql_type)) {
			var->sql_type = encode_sql_type_info(sql_type, precision, scale, flags | FLAG_PICX_AS_VARCHAR);
		}
		else {
			var->sql_type = type_info;
		}
		var->is_varlen = HAS_FLAG_EMIT_VAR(flags);
		var->usage = IS_BINARY(sql_type) ? Usage::Binary : Usage::None;
		var->picnsize = precision;
		var->scale = scale;

		cb_exec_sql_stmt_ptr stmt = nullptr;
		if (HAS_FLAG_EMIT_VAR(flags)) {
			stmt = new cb_exec_sql_stmt_t();
			stmt->commandName = ESQL_DECLARE_VAR;
			stmt->src_file = filename_clean_path(var->defined_at_source_file);
			stmt->src_abs_path = filename_absolute_path(var->defined_at_source_file);
			stmt->startLine = var->defined_at_source_line;
			stmt->endLine = var->defined_at_source_line;

			cb_hostreference_ptr p = new cb_hostreference_t();
			p->hostno = n++;
			p->hostreference = var_name;
			p->lineno = var->defined_at_source_line;

			stmt->host_list->push_back(p);
			main_module_driver.exec_list->push_back(stmt);
		}
		stmt = new cb_exec_sql_stmt_t();
		stmt->commandName = ESQL_COMMENT;
		stmt->src_file = filename_clean_path(var->defined_at_source_file);
		stmt->src_abs_path = filename_absolute_path(var->defined_at_source_file);
		stmt->startLine = orig_start_line;
		stmt->endLine = orig_end_line;

		main_module_driver.exec_list->push_back(stmt);

	}

	return true;
}

std::map<uint64_t, uint64_t>& TPESQLProcessing::getBinarySrcLineMap() const
{
	return const_cast<std::map<uint64_t, uint64_t>&>(b_in_to_out);
}

std::map<uint64_t, uint64_t>& TPESQLProcessing::getBinarySrcLineMapReverse() const
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

bool TPESQLProcessing::write_map_file(const std::string& preprocd_file)
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

void TPESQLProcessing::add_dependency(const std::string& parent, const std::string& dep_path)
{
	std::vector<std::string> deps = (map_contains< std::string, std::vector<std::string>>(file_dependencies, parent) ? file_dependencies.at(parent) : std::vector<std::string>());
	deps.push_back(dep_path);
	file_dependencies[parent] = deps;
}

bool TPESQLProcessing::is_current_file_included()
{
	return input_file_stack.size() > 1;
}

void TPESQLProcessing::put_whenever_clause_handler(esql_whenever_clause_handler_t* ch)
{
	const char* lp = AREA_B_CPREFIX;

	switch (ch->action) {
	case WHENEVER_ACTION_CONTINUE:
		put_output_line(lp + string_format("    CONTINUE"));
		break;

	case WHENEVER_ACTION_GOTO:
		put_output_line(lp + string_format("    GO TO %s", ch->host_label));
		break;

	case WHENEVER_ACTION_PERFORM:
		put_output_line(lp + string_format("    PERFORM %s", ch->host_label));
		break;
	}
}

void TPESQLProcessing::put_smart_cursor_init_flags()
{
	const char* lp = AREA_B_CPREFIX;

	if (emitted_smart_cursor_init_flags)
		return;

	put_output_line(code_tag + "*   ESQL CURSOR INIT FLAGS (START)");
	// First we generate the flags for cursors declared in WORKING-STORAGE
	for (cb_exec_sql_stmt_ptr stmt : startup_items) {
		std::string cname = string_replace(stmt->cursorName, "_", "-");
		put_output_line(code_tag + string_format(" 01  GIXSQL-CI-F-%s PIC X.", cname));
	}
	// Then the other cursors
	auto other_crsrs = cpplinq::from(*(main_module_driver.exec_list)).where([](cb_exec_sql_stmt_ptr p) { return p->startup_item == 0 && p->commandName == ESQL_SELECT && !p->cursorName.empty(); }).to_vector();
	for (cb_exec_sql_stmt_ptr stmt : other_crsrs) {
		std::string cname = string_replace(stmt->cursorName, "_", "-");
		put_output_line(code_tag + string_format(" 01  GIXSQL-CI-F-%s PIC X.", cname));
	}
	put_output_line(code_tag + "*   ESQL CURSOR INIT FLAGS (END)");

	emitted_smart_cursor_init_flags = true;
}

void TPESQLProcessing::put_smart_cursor_init_check(const std::string& crsr_name)
{
	std::string cname = string_replace(crsr_name, "_", "-");
	std::string crsr_init_var = "GIXSQL-CI-F-" + cname;
	std::string crsr_init_st = "GIXSQL-CI-P-" + cname;

	put_output_line(string_format(AREA_B_CPREFIX "IF %s = ' ' THEN", crsr_init_var));
	put_output_line(string_format(AREA_B_CPREFIX "    PERFORM %s", crsr_init_st));
	put_output_line(string_format(AREA_B_CPREFIX "    IF SQLCODE = 0"));
	put_output_line(string_format(AREA_B_CPREFIX "      MOVE 'X' TO %s", crsr_init_var));
	put_output_line(string_format(AREA_B_CPREFIX "   END-IF"));
	put_output_line(string_format(AREA_B_CPREFIX "END-IF"));
}

bool TPESQLProcessing::put_res_host_parameters(const cb_exec_sql_stmt_ptr stmt, int* res_params_count)
{
	int rp_count = 0;
	int f_type, f_size, f_scale;
	bool emit_static = opt_emit_static_calls;

	for (cb_res_hostreference_ptr rp : *stmt->res_host_list) {

		if (!main_module_driver.field_exists(rp->hostreference.substr(1))) {
			main_module_driver.error("Cannot find host variable: " + rp->hostreference.substr(1), ERR_MISSING_HOSTVAR, stmt->src_abs_path, rp->lineno);
			return false;
		}

		cb_field_ptr hr = main_module_driver.field_map[rp->hostreference.substr(1)];
		bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

		// Support for group items used as host variables in SELECT statements
		// They are decomposed into their sub-elements
		if (f_type == COBOL_TYPE_GROUP && !is_varlen) {

			if (hr->group_levels_count != 1) {
				main_module_driver.error("Nested levels not allowed in group variable: " + rp->hostreference.substr(1), ERR_MISSING_HOSTVAR, stmt->src_abs_path, rp->lineno);
				return false;
			}

			cb_field_ptr pp = hr->children;
			if (!pp) {
				main_module_driver.error("Inconsistent data for group field : " + hr->sname, ERR_MISSING_HOSTVAR, stmt->src_abs_path, rp->lineno);
				return false;
			}

			while (pp) {

				ESQLCall pp_call(get_call_id("SetResultParams"), emit_static);
				int pp_flags = (pp->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

				int pp_type = 0, pp_size = 0, pp_scale = 0;
				bool pp_is_varlen = get_actual_field_data(pp, &pp_type, &pp_size, &pp_scale);
				if (pp_is_varlen) {
					main_module_driver.error("Inconsistent data for group field member: " + pp->sname, ERR_MISSING_HOSTVAR, stmt->src_abs_path, rp->lineno);
					return false;
				}

				pp_call.addParameter(pp_type, BY_VALUE);
				pp_call.addParameter(pp_size, BY_VALUE);
				pp_call.addParameter(pp_scale > 0 ? -pp_scale : 0, BY_VALUE);
				pp_call.addParameter(pp_flags, BY_VALUE);
				pp_call.addParameter(pp->sname, BY_REFERENCE);

				if (!put_call(pp_call, false))
					return false;

				rp_count++;

				pp = pp->sister;
			}

		}
		else {
			ESQLCall rp_call(get_call_id("SetResultParams"), emit_static);

			int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
			flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

			rp_call.addParameter(f_type, BY_VALUE);
			rp_call.addParameter(f_size, BY_VALUE);
			rp_call.addParameter(f_scale > 0 ? -f_scale : 0, BY_VALUE);
			rp_call.addParameter(flags, BY_VALUE);
			rp_call.addParameter(rp->hostreference.substr(1), BY_REFERENCE);

			if (!put_call(rp_call, false))
				return false;

			rp_count++;
		}
	}

	*res_params_count = rp_count;
	return true;
}

bool TPESQLProcessing::put_host_parameters(const cb_exec_sql_stmt_ptr stmt)
{
	int f_type, f_size, f_scale;
	bool emit_static = opt_emit_static_calls;

	for (cb_hostreference_ptr p : *stmt->host_list) {
		ESQLCall p_call(get_call_id("SetSQLParams"), emit_static);
		if (!main_module_driver.field_exists(p->hostreference.substr(1))) {
			main_module_driver.error("Cannot find host variable: " + p->hostreference.substr(1), ERR_MISSING_HOSTVAR, stmt->src_abs_path, p->lineno);
			return false;
		}

		cb_field_ptr hr = main_module_driver.field_map[p->hostreference.substr(1)];
		bool is_varlen = get_actual_field_data(hr, &f_type, &f_size, &f_scale);

		int flags = is_varlen ? CBL_FIELD_FLAG_VARLEN : CBL_FIELD_FLAG_NONE;
		flags |= (hr->usage == Usage::Binary) ? CBL_FIELD_FLAG_BINARY : CBL_FIELD_FLAG_NONE;

		uint32_t hr_type, hr_precision;
		uint16_t hr_scale;
		uint8_t hr_flags;
		decode_sql_type_info(hr->sql_type, &hr_type, &hr_precision, &hr_scale, &hr_flags);
		if (HAS_PICX_AS_VARCHAR(hr_flags) || opt_picx_as_varchar)
			flags |= CBL_FIELD_FLAG_AUTOTRIM;

		p_call.addParameter(f_type, BY_VALUE);
		p_call.addParameter(f_size, BY_VALUE);
		p_call.addParameter(f_scale > 0 ? -f_scale : 0, BY_VALUE);
		p_call.addParameter(flags, BY_VALUE);
		p_call.addParameter(p->hostreference.substr(1), BY_REFERENCE);

		if (!put_call(p_call, false))
			return false;
	}
	return true;
}

void TPESQLProcessing::add_preprocessed_blocks()
{
	std::vector<cb_exec_sql_stmt_ptr>* p = main_module_driver.exec_list;
	for (auto e : *p) {
		PreprocessedBlockInfo *bi = new PreprocessedBlockInfo();
		bi->type = PreprocessedBlockType::ESQL;
		bi->command = e->commandName;

		bi->orig_source_file = e->src_file;
		bi->orig_start_line = e->startLine;
		bi->orig_end_line = e->endLine;

		auto in_out_start_key = std::to_string(bi->orig_start_line) + "@" + bi->orig_source_file;
		auto in_out_end_key = std::to_string(bi->orig_end_line) + "@" + bi->orig_source_file;
		if (in_to_out.find(in_out_start_key) == in_to_out.end() || in_to_out.find(in_out_end_key) == in_to_out.end()) {
			delete bi;
			continue;
		}

		if (generated_blocks.find(e) == generated_blocks.end()) {
			delete bi;
			continue;
		}

		auto in_out_start_val = in_to_out[in_out_start_key];
		auto in_out_end_val = in_to_out[in_out_end_key];

		bi->pp_source_file = in_out_start_val.substr(in_out_start_val.find("@") + 1);
		bi->pp_start_line = atoi(in_out_start_val.substr(0, in_out_start_val.find("@")).c_str());

		bi->pp_end_line = atoi(in_out_end_val.substr(0, in_out_end_val.find("@")).c_str());

		bi->module_name = main_module_driver.program_id;
		
		auto gb = generated_blocks[e];
		bi->pp_gen_start_line = std::get<0>(gb);
		bi->pp_gen_end_line = std::get<1>(gb);

		preprocessed_blocks.push_back(bi);
	}

}

void TPESQLProcessing::put_whenever_handler(bool terminate_with_period)
{
	const char* lp = AREA_B_CPREFIX;

	// Optimization (sort of) if we have no handlers defined, we treat it as a special case
	if (esql_whenever_handler.not_found.action == WHENEVER_ACTION_CONTINUE &&
		esql_whenever_handler.sqlwarning.action == WHENEVER_ACTION_CONTINUE &&
		esql_whenever_handler.sqlerror.action == WHENEVER_ACTION_CONTINUE) {
		// We always need to output something to handle statement-terminating periods
		// and expect the COBOL compiler to optimize this out
		//put_output_line(lp + string_format("CONTINUE"));
		if (terminate_with_period) {
			output_lines.back() += ".";
		}
		return;
	}

	put_output_line(lp + std::string("EVALUATE TRUE"));

	// Not found
	put_output_line(lp + std::string("WHEN SQLCODE = ") + std::to_string(opt_norec_sqlcode));
	put_whenever_clause_handler(&esql_whenever_handler.not_found);

	// SQLWARNING
	put_output_line(lp + std::string("WHEN SQLCODE = 1"));
	put_whenever_clause_handler(&esql_whenever_handler.sqlwarning);

	// SQLERROR
	put_output_line(lp + std::string("WHEN SQLCODE < 0"));
	put_whenever_clause_handler(&esql_whenever_handler.sqlerror);

	put_output_line(lp + std::string("END-EVALUATE"));

	if (terminate_with_period) {
		output_lines.back() += ".";
	}
}

std::string TPESQLProcessing::getModuleName()
{
	return main_module_driver.program_id;
}

void TPESQLProcessing::splitLineEntry(const std::string& k, std::string& s, int* i)
{
	int p = k.find("@");
	s = k.substr(p + 1);
	*i = std::stoi(k.substr(0, p));
}

void TPESQLProcessing::map_collect_files(std::map<std::string, int>& filemap)
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

std::map<std::string, std::string>& TPESQLProcessing::getSrcLineMap() const
{
	return const_cast<std::map<std::string, std::string>&>(in_to_out);
}

std::map<std::string, std::string>& TPESQLProcessing::getSrcLineMapReverse() const
{
	return const_cast<std::map<std::string, std::string>&>(out_to_in);
}

std::map<std::string, int>& TPESQLProcessing::getFileMap() const
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

std::map<std::string, cb_field_ptr>& TPESQLProcessing::getVariableDeclarationInfoMap() const
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

std::vector<PreprocessedBlockInfo*> TPESQLProcessing::getPreprocessedBlocks()
{
	return preprocessed_blocks;
}

bool check_sql_type_compatibility(uint64_t type_info, cb_field_ptr var)
{
	// TODO: implement this
	return true;
}
