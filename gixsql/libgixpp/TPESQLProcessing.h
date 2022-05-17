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
#include <map>
#include <stack>

#include "ITransformationStep.h"
#include "ESQLDefinitions.h"
#include "gix_esql_driver.hh"
#include "ESQLCall.h"

class gix_esql_driver;

enum class ESQL_Command;

class TPESQLProcessing : public ITransformationStep
{
public:
	TPESQLProcessing(GixPreProcessor *gpp);

	std::string getModuleName();

	std::map<std::string, std::string> &getSrcLineMap() const;
	std::map<std::string, std::string> &getSrcLineMapReverse() const;

	std::map<uint64_t, uint64_t> &getBinarySrcLineMap() const;
	std::map<uint64_t, uint64_t> &getBinarySrcLineMapReverse() const;

	std::map<std::string, int> &getFileMap() const;
	std::map<int, std::string> getReverseFileMap();
	std::map<std::string, cb_field_ptr> &getVariableDeclarationInfoMap() const;

	std::map<std::string, srcLocation> getParagraphs();
	std::map<std::string,  std::vector<std::string>> getFileDependencies();

	// Inherited via ITransformationStep
	virtual bool run(ITransformationStep *prev_step) override;
	virtual std::string getOutput(ITransformationStep *me = nullptr) override;


private:

	// ESQL options
	bool opt_anonymous_params;
	bool opt_preprocess_copy_files;
	bool opt_emit_static_calls;
	bool opt_emit_debug_info;
	bool opt_emit_compat;
	bool opt_consolidated_map;
	bool opt_no_output;
	bool opt_emit_map_file;
	bool opt_emit_cobol85;

	std::string opt_varlen_suffix_len;
	std::string opt_varlen_suffix_data;

	gix_esql_driver main_module_driver;

	 std::vector<std::string> output_lines;

	int output_line;

	std::map<std::string, std::string> in_to_out;
	std::map<std::string, std::string> out_to_in;

	std::map<uint64_t, uint64_t> b_in_to_out;
	std::map<uint64_t, uint64_t> b_out_to_in;

	int outputESQL();
	cb_exec_sql_stmt_ptr find_exec_sql_stmt(const std::string filename, int i);
	cb_exec_sql_stmt_ptr find_esql_cmd(std::string cmd, int idx);
	std::string comment_line(const std::string &comment, const std::string &line);

	void put_output_line(const std::string &line);
	bool handle_esql_stmt(const ESQL_Command cmd, const cb_exec_sql_stmt_ptr stmt, bool is_in_ws);

	bool find_working_storage(int *working_begin_line, int *working_end_line);

	bool processNextFile();

	std::string get_call_id(const std::string s);

	void put_start_exec_sql(bool with_period);
	void put_end_exec_sql(bool with_period);
	bool put_query_defs();
	void put_working_storage();
	bool put_cursor_declarations();
	bool put_call(const ESQLCall &call, bool terminate_with_period);

	bool is_var_len_group(cb_field_ptr f);
	bool get_actual_field_data(cb_field_ptr f, int *type, int *size, int *scale);
	void process_sql_query_list();
	bool fixup_declared_vars();

	bool write_map_file(const std::string &preprocd_file);
	bool build_map_data();

	void map_collect_files(std::map<std::string, int> &filemap);

	void splitLineEntry(const std::string &k, std::string &s, int *i);

	bool generate_consolidated_map();

	void add_dependency(const std::string &parent, const std::string &dep_path);

	bool is_current_file_included();

	std::stack<std::string> input_file_stack;
	int working_begin_line;
	int working_end_line;
	std::string code_tag;

	 std::vector<std::string> ws_query_list;
	std::vector<cb_exec_sql_stmt_ptr> startup_items;

	std::map<std::string, int> filemap;

	std::map<std::string,  std::vector<std::string>> file_dependencies;

	int current_input_line;

	bool emitted_query_defs = false;
};

