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

#include "gix_esql_driver.hh"

#include <vector>

#include "gix_esql_parser.hh"
#include "libcpputils.h"
#include "cobol_var_types.h"

#include "TPESQLProcessing.h"

#if _DEBUG
#if defined (VERBOSE)
#define DEBUG_PARSER true
#else
#define DEBUG_PARSER false
#endif
#else
#define DEBUG_PARSER false
#endif

#if (defined(_WIN32) || defined(_WIN64)) && !defined(__MINGW32__)
#define PATH_SEP "\\"
#else
#define PATH_SEP "/"
#endif

gix_esql_driver::gix_esql_driver ()
    : trace_scanning (DEBUG_PARSER), trace_parsing (DEBUG_PARSER)
{
	lexer.setDriver(this);
	
	opt_params_style = ESQL_ParameterStyle::DollarPrefix;
	opt_preprocess_copy_files = false;	// if true, copybooks outside EXEC SQL INCLUDE... are preprocessed
	has_esql_in_cbl_copybooks = false;

	currenthostno = 0;
	hostlineno = 0;
	sqlnum = 0;
	cursor_hold = false;
	commandname = "";
	cursorname = "";
	sqlname = "";
	incfilename = "";
	transaction_release = false;
	hostreferenceCount = 0;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	sql_list = NULL;
	exec_list = NULL;

	current_field = NULL;
	description_field = NULL;

	pp_inst = nullptr;

	host_reference_list = new std::vector<cb_hostreference_ptr>();
	res_host_reference_list = new std::vector<cb_res_hostreference_ptr>();
	sql_list = new std::vector<cb_sql_token_t>();
	exec_list = new std::vector<cb_exec_sql_stmt_ptr>();
	hostref_or_literal_list = new std::vector<hostref_or_literal_t *>();

}

gix_esql_driver::~gix_esql_driver ()
{
	delete host_reference_list;
	delete res_host_reference_list;
	delete sql_list;
	delete exec_list;
	delete hostref_or_literal_list;
}

void gix_esql_driver::setCaller(TPESQLProcessing* p)
{
	pp_caller = p;
}

int gix_esql_driver::parse (GixPreProcessor *gpp, const std::string &f)
{
	pp_inst = gpp;

	std::string tf = filename_change_ext(f, "");

	filenameID = filename_get_name(tf);
	file = f;

	lexer.src_location_stack.push({ filename_absolute_path(f), 1 });

    scan_begin ();
    yy::gix_esql_parser parser (*this);
    parser.set_debug_level (trace_parsing);
    int res = parser.parse ();

    scan_end ();

	if (!res && !pp_inst->err_data.err_code)
		return 0;
	else {
		return -1;
	}
}

// The location from Bison is not actually used. The specific error code (for now) is not actually used elsewhere.

void gix_esql_driver::error (const yy::location& l, const std::string& m, int err_code, std::string filename, int line)
{   
	if (filename.empty())
		filename = this->lexer.src_location_stack.top().filename;

	if (line == -1)
		line = lexer.getLineNo();

	std::string msg = string_format("%s:%d: error: %s", filename, line, m);
	this->pp_inst->err_data.err_messages.push_back(msg);
	if (!this->pp_inst->err_data.err_code || err_code != ERR_ALREADY_SET)
		this->pp_inst->err_data.err_code = err_code;
}

void gix_esql_driver::error (const std::string& m, int err_code, std::string filename, int line)
{
	yy::location loc;
	error(loc, m, err_code, filename, line);
}

void gix_esql_driver::warning(const yy::location &l, const std::string &m)
{
	std::string msg = string_format("%s:%d: warning: %s", this->lexer.src_location_stack.top().filename, lexer.getLineNo(), m);
	this->pp_inst->err_data.warnings.push_back(msg);
}

void gix_esql_driver::warning(const std::string & m)
{
	yy::location loc;
	warning(loc, m);
}

void gix_esql_driver::scan_begin()
{
    lexer.set_debug( trace_scanning );

    // Try to open the file:
    instream.open(file);

    if( instream.good() ) {
        lexer.switch_streams(&instream, 0);
    } else if( file == "-" ) { 
        lexer.switch_streams(&std::cin, 0);
    } else {
		yy::location loc;	// FIXME
        error (loc, "Cannot open file '" + file + "'.", ERR_FILE_NOT_FOUND);
        exit (EXIT_FAILURE);
    }
}

void gix_esql_driver::scan_end ()
{
    instream.close();
}


std::vector<cb_sql_token_t> *cb_sql_list_dup(const std::vector<cb_sql_token_t> *orig)
{
	return new std::vector<cb_sql_token_t>(*orig);
}

std::vector<cb_sql_token_t> *gix_esql_driver::cb_text_list_add(std::vector<cb_sql_token_t> *list, std::string text)
{
	std::vector<cb_sql_token_t> *l = (list != nullptr) ? list : new std::vector<cb_sql_token_t>();
	l->push_back(text);
	return l;
}

std::vector<cb_sql_token_t> *gix_esql_driver::cb_concat_text_list(std::vector<cb_sql_token_t> *list, std::vector<cb_sql_token_t> *targetlist)
{
	if (targetlist) {
		list->insert(list->end(), targetlist->begin(), targetlist->end());
	}
	sql_list = list;
	return list;
}

std::string gix_esql_driver::cb_host_list_add(std::vector<cb_hostreference_ptr> *list, std::string text)
{
	// Handle placeholders for group items passed as host variables
	if (map_contains(field_map, text.substr(1))) {
		int f_type = 0, f_size = 0, f_scale = 0;
		cb_field_ptr f = field_map[text.substr(1)];
		bool is_varlen = pp_caller->get_actual_field_data(f, &f_type, &f_size, &f_scale);

		if ((this->commandname == "INSERT" || this->commandname == "SELECT") && 
				f_type == COBOL_TYPE_GROUP && !is_varlen) {
			cb_field_ptr c = f->children;
			std::string s;
			while (c) {
				s += cb_host_list_add(list, ":" + c->sname);
				c = c->sister;
				if (c) s += ",";
			}
			return "@[" + s + "]";
		}
	}

	int hostno = cb_search_list(text);

	switch (opt_params_style) {
		case ESQL_ParameterStyle::DollarPrefix:
			return "$" + std::to_string(hostno);

		case ESQL_ParameterStyle::ColonPrefix:
			return ":" + std::to_string(hostno);

		case ESQL_ParameterStyle::Anonymous:
			return "?";
	}
}

std::string gix_esql_driver::cb_host_list_add_force(std::vector<cb_hostreference_ptr> *list, std::string text)
{
	// Handle placeholders for group items passed as host variables
	if (map_contains(field_map, text.substr(1))) {
		int f_type = 0, f_size = 0, f_scale = 0;
		cb_field_ptr f = field_map[text.substr(1)];
		bool is_varlen = pp_caller->get_actual_field_data(f, &f_type, &f_size, &f_scale);

		if ((this->commandname == "INSERT" || this->commandname == "SELECT") &&
			f_type == COBOL_TYPE_GROUP && !is_varlen) {
			cb_field_ptr c = f->children;
			std::string s;
			while (c) {
				s += cb_host_list_add_force(list, ":" + c->sname);
				c = c->sister;
				if (c) s += ",";
			}
			return "@[" + s + "]";
		}
	}

	int hostno = list->size() + 1;

	cb_hostreference_ptr p = new cb_hostreference_t();
	p->hostreference = text;
	p->hostno = hostno;
	p->lineno = hostlineno;

	list->push_back(p);
	switch (opt_params_style) {
	case ESQL_ParameterStyle::DollarPrefix:
		return "$" + std::to_string(hostno);

	case ESQL_ParameterStyle::ColonPrefix:
		return ":" + std::to_string(hostno);

	case ESQL_ParameterStyle::Anonymous:
		return "?";
	}
}


void gix_esql_driver::cb_res_host_list_add(std::vector<cb_res_hostreference_ptr> *list, std::string text)
{
	cb_res_hostreference_ptr p = new cb_res_hostreference_t();
	p->hostreference = text;
	p->lineno = hostlineno;

	res_host_reference_list->push_back(p);
}

int
gix_esql_driver::cb_search_list(std::string text)
{
	if (opt_params_style != ESQL_ParameterStyle::Anonymous) {
		for (auto it = host_reference_list->begin(); it != host_reference_list->end(); ++it) {
			if ((*it)->hostreference == text)
				return (*it)->hostno;
		}
	}

	int hostno = host_reference_list->size() + 1;

	// execute
	cb_hostreference_ptr p = new cb_hostreference_t();
	p->hostreference = text;
	p->hostno = hostno;
	p->lineno = hostlineno;

	host_reference_list->push_back(p);

	// return
	return hostno;
}

void
gix_esql_driver::cb_set_cursorname(std::string text)
{
	cursorname = filenameID + "_" + text;
}

void
gix_esql_driver::cb_set_commandname(std::string text)
{
	commandname = text;
}

void
gix_esql_driver::cb_set_cursor_hold(bool h)
{
	cursor_hold = h;
}


void gix_esql_driver::put_startup_exec_list()
{
	put_exec_list();
	exec_list->back()->startup_item = true;
}

void gix_esql_driver::put_exec_list()
{
	cb_exec_sql_stmt_ptr l = new cb_exec_sql_stmt_t();

	l->startLine = startlineno;
	l->endLine = endlineno;
	l->period = period;
	l->host_list = host_reference_list;
	l->res_host_list = res_host_reference_list;
	l->sql_list = sql_list;
	l->hostref_or_literal_list = hostref_or_literal_list;
	l->cursorName = cursorname;
	l->commandName = commandname;
	l->textContent = text_content;
	l->command_putother = command_putother;
	l->transaction_release = transaction_release;
	l->sqlName = sqlname;
	l->incfileName = incfilename;
	l->statementName = statement_name;
	l->statementSource = statement_source;
	l->cursor_hold = cursor_hold;
	l->src_file = filename_clean_path(lexer.src_location_stack.top().filename);
	l->src_abs_path = filename_absolute_path(l->src_file);

	l->startup_item = 0;
	l->sql_query_list_id = sqlnum;

	l->conninfo = conninfo;
	l->connectionId = connectionid;
	l->whenever_data = whenever_data;

	host_reference_list = new std::vector<cb_hostreference_ptr>();
	res_host_reference_list = new std::vector<cb_res_hostreference_ptr>();
	sql_list = new std::vector<cb_sql_token_t>();
	hostref_or_literal_list = new std::vector<hostref_or_literal_t *>();
	conninfo = new esql_connection_info_t();

	statement_name.clear();
	statement_source = nullptr;

	exec_list->push_back(l);

}

bool gix_esql_driver::field_exists(const std::string &f)
{
	return (field_map.find(f) != field_map.end());
}

int cb_get_level(int val)
{
	int level = val;

	/* check level */
	switch (level) {
		case 66:
		case 77:
		case 78:
		case 88:
			break;
		default:
			if (level < 1 || level > 49) {
				goto level_error;
			}
			break;
	}

	return level;

level_error:

	return 0;
}

cb_field_ptr cb_field_founder(cb_field_ptr f)
{
	while (f->parent) {
		f = f->parent;
	}
	return f;
}

cb_field_ptr gix_esql_driver::cb_build_field_tree(int level, std::string name, cb_field_ptr last_field)
{
	int lv;
	cb_field_ptr f, p;

	if (name.empty())
		return NULL;

	lv = cb_get_level(level);
	if (!lv) {
		return NULL;
	}
	
	f = new cb_field_t();

	f->sname = name;

	if (lv == 78) {
		f->level = 1;
	}
	else {
		f->level = lv;
	}

	if (last_field) {
		if (last_field->level == 77 && f->level != 01 &&
			f->level != 77 && f->level != 66 && f->level != 88) {
			return NULL;
		}
	}

	if (f->level == 1 || f->level == 77) {
		/* top level */
		if (last_field) {
			cb_field_founder(last_field)->sister = f;
		}
	}
	else {
		if (last_field == NULL) {
			fprintf(stderr, "parse error: %s level should start from 01 or 66 or 77 or 88\n", name.c_str());
			//exit(-1);
			return NULL;
		}

		if (f->level == 66) {
			/* level 66 */
			f->parent = cb_field_founder(last_field);
			for (p = f->parent->children; p && p->sister; p = p->sister);
			if (p) {
				p->sister = f;
			}
		}
		else if (f->level == 88) {
			/* level 88 */
			f->parent = last_field;
		}
		else if (f->level > last_field->level) {
			/* lower level */
			last_field->children = f;
			f->parent = last_field;
		}
		else if (f->level == last_field->level) {
			/* same level */
		same_level:
			last_field->sister = f;
			f->parent = last_field->parent;
		}
		else {
			/* upper level */
			for (p = last_field->parent; p; p = p->parent) {
				if (p->level == f->level) {
					last_field = p;
					goto same_level;
				}
				if (p->level < f->level) {
					break;
				}
			}
			return NULL;
		}
	}

	if (f) {
		std::string path = f->sname;
		cb_field_ptr p = f;
		while (p->parent) {
			path = p->parent->sname + ":" + path;
			p = p->parent;
		}

		switch (this->data_division_section) {
			case DD_SECTION_WS:
				f->path = "WS:" + path;
				f->data_section = DataSectionType::WorkingStorage;
				break;

			case DD_SECTION_LS:
				f->path = "LS:" + path;
				f->data_section = DataSectionType::LinkageSection;
				break;

			case DD_SECTION_LL:
				f->path = "LL:" + path;
				f->data_section = DataSectionType::LocalStorage;
				break;


			case DD_SECTION_FS:
				f->path = "FS:" + path;
				f->data_section = DataSectionType::FileSection;
				break;

		}
		field_map[f->sname] = f;
	}

	f->defined_at_source_line = lexer.getLineNo();
	f->defined_at_source_file = lexer.driver->file;

	return f;
}

int gix_esql_driver::build_picture(const std::string str, cb_field_ptr pic)
{
	const char *p = str.c_str();

	int			i;
	int			n;
	unsigned char		c;

	int	category = 0;
	int s_count = 0;
	int v_count = 0;
	int idx = 0;
	int digits = 0;
	int scale = 0;
	int allocated = 0;

	if (str.length() > 50) {
		return 0;
	}

	for (; *p; p++) {
		n = 1;
		c = *p;

		while (1) {
			while (p[1] == c) {
				p++; n++;
			}

			if (p[1] == '(') {
				i = 0;
				p += 2;
				allocated = 0;
				for (; *p == '0'; p++) {
					;
				}
				for (; *p != ')'; p++) {
					if (!isdigit(*p)) {
						return 0;
					}
					else {
						allocated++;
						if (allocated > 9) {
							return 0;
						}
						i = i * 10 + (*p - '0');
					}
				}
				if (i == 0) {
					return 0;
				}
				n += i - 1;
				continue;
			}
			break;
		}


		switch (c) {
			case 'X':
				if (s_count | v_count) {
					return 0;
				}
				category |= PIC_ALPHANUMERIC;
				digits += n;
				break;
			case '9':
				category |= PIC_NUMERIC;
				digits += n;
				if (v_count) {
					scale += n;
				}
				break;
			case 'N':
				if (s_count | v_count) {
					return 0;
				}
				category |= PIC_NATIONAL;
				digits += n;
				break;
			case 'S':
				category |= PIC_NUMERIC;
				if (category & PIC_ALPHABETIC) {
					return 0;
				}
				s_count += n;
				if (s_count > 1 || idx != 0) {
					return 0;
				}
				continue;
			case 'V':
				category |= PIC_NUMERIC;
				if (category & PIC_ALPHABETIC) {
					return 0;
				}
				v_count += n;
				if (v_count > 1) {
					return 0;
				}
				break;
			default:
				break;
		}
		idx += sizeof(int);
	}

	pic->picnsize = digits;
	pic->scale = scale;
	pic->have_sign = (unsigned char)s_count;
	pic->pictype = category;
	return 1;
}

/*
	SQL TYPE INFO is a 64 bit unsigned int:
	bits 00-15: scale (16 bit unsigned int)
	bits 16-47: precision (32 bit unsigned int)
	bits 48-51: misc flags
	bits 52-59: unused/reserved
	bits 60-63: type (encoded with the TYPE_SQL_* consts)
*/
void decode_sql_type_info(uint64_t type_info, uint32_t* sql_type, uint32_t* precision, uint16_t* scale, uint8_t* flags)
{
	uint64_t length = type_info & 0xffffffffffff;	// 48 bits
	*precision = (length >> 16);
	*scale = (length & 0xffff);
	*sql_type = (type_info >> 60);
	*flags = (type_info >> 48) & 0x0f;
}

uint64_t encode_sql_type_info(uint32_t sql_type, uint32_t precision, uint16_t scale, uint8_t flags)
{
	uint64_t type_info = 0ULL;
	type_info |= ((uint64_t)sql_type << 60);
	type_info |= (((uint64_t)precision << 16));
	type_info |= (((uint64_t)scale));
	flags &= 0x0f;
	uint64_t flag_mask = 0xf000000000000;
	uint64_t flag_val = (((uint64_t)flags << 48));
	type_info = (type_info & ~flag_mask) | (flag_val & flag_mask);
	return type_info;
}