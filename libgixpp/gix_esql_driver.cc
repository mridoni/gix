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

#include "gix_esql_driver.hh"
#include "gix_esql_parser.hh"
#include "PathUtils.h"

#include <QFileInfo>

#if _DEBUG
#define DEBUG_PARSER true
#else
#define DEBUG_PARSER false
#endif

#define PIC_ALPHABETIC 		0x01
#define PIC_NUMERIC 		0x02
#define PIC_NATIONAL		0x04
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)

#if (defined(_WIN32) || defined(_WIN64)) && !defined(__MINGW32__)
#define PATH_SEP "\\"
#else
#define PATH_SEP "/"
#endif

gix_esql_driver::gix_esql_driver ()
    : trace_scanning (DEBUG_PARSER), trace_parsing (DEBUG_PARSER)
{
	lexer.setDriver(this);
	
	opt_use_anonymous_params = false;
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
	hostreferenceCount = 0;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	sql_list = NULL;
	exec_list = NULL;

	current_field = NULL;
	description_field = NULL;

	pp_inst = nullptr;

	host_reference_list = new QList<cb_hostreference_ptr>();
	res_host_reference_list = new QList<cb_res_hostreference_ptr>();
	sql_list = new QList<cb_sql_token_t>();
	exec_list = new QList<cb_exec_sql_stmt_ptr>();

}

gix_esql_driver::~gix_esql_driver ()
{
	delete host_reference_list;
	delete res_host_reference_list;
	delete sql_list;
	delete exec_list;
}

int gix_esql_driver::parse (GixPreProcessor *gpp, const QString &f)
{
	pp_inst = gpp;

	QString tf = PathUtils::changeExtension(f, "");
	tf = PathUtils::getFilename(tf);

	filenameID = strdup(tf.toLocal8Bit().constData());
    
	file = f.toStdString();

	lexer.src_location_stack.push({ QFileInfo(f).absoluteFilePath() , 1 });

    scan_begin ();
    yy::gix_esql_parser parser (*this);
    parser.set_debug_level (trace_parsing);
    int res = parser.parse ();

    scan_end ();

    return res;
}

void gix_esql_driver::error (const yy::location& l, const std::string& m)
{
	QString msg = QString("ESQL parse error at line %1 of file %2: %3").arg(lexer.getLineNo()).arg(this->lexer.src_location_stack.top().filename).arg(QString::fromStdString(m));
    //std::cerr << l << ": " << m << std::endl;
	this->pp_inst->err_messages << msg;
	this->pp_inst->err_code = 1;
}

void gix_esql_driver::error (const std::string& m)
{
    std::cerr << m << std::endl;
}

// CHANGE: functions moved from the bottom of `gix_esql_scanner.ll`

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
        error ("Cannot open file '" + file + "'.");
        exit (EXIT_FAILURE);
    }
}

void gix_esql_driver::scan_end ()
{
    instream.close();
}


QList<cb_sql_token_t> *cb_sql_list_dup(const QList<cb_sql_token_t> *orig)
{
	return new QList<cb_sql_token_t>(*orig);
}

QList<cb_sql_token_t> *gix_esql_driver::cb_text_list_add(QList<cb_sql_token_t> *list, QString text)
{
	QList<cb_sql_token_t> *l = (list != nullptr) ? list : new QList<cb_sql_token_t>();
	l->append(text);
	return l;
}

QList<cb_sql_token_t> *gix_esql_driver::cb_concat_text_list(QList<cb_sql_token_t> *list, QList<cb_sql_token_t> *targetlist)
{
	list->append(*targetlist);
	sql_list = list;
	return list;
}

QString gix_esql_driver::cb_host_list_add(QList<cb_hostreference_ptr> *list, QString text)
{
	int hostno = cb_search_list(text);

	if (!opt_use_anonymous_params)
		return "$" + QString::number(hostno);
	else
		return "?";
}

QString gix_esql_driver::cb_host_list_add_force(QList<cb_hostreference_ptr> *list, QString text)
{
	int hostno = list->size() + 1;

	cb_hostreference_ptr p = new cb_hostreference_t();
	p->hostreference = text;
	p->hostno = hostno;
	p->lineno = hostlineno;

	list->append(p);

	if (!opt_use_anonymous_params)
		return "$" + QString::number(hostno);
	else
		return "?";
}


void gix_esql_driver::cb_res_host_list_add(QList<cb_res_hostreference_ptr> *list, QString text)
{
	cb_res_hostreference_ptr p = new cb_res_hostreference_t();
	p->hostreference = text;
	p->lineno = hostlineno;

	res_host_reference_list->append(p);
}

int
gix_esql_driver::cb_search_list(QString text)
{
	if (!opt_use_anonymous_params) {
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

	host_reference_list->append(p);

	// return
	return hostno;
}

void
gix_esql_driver::cb_set_cursorname(QString text)
{
	cursorname = filenameID + "_" + text;
}

void
gix_esql_driver::cb_set_commandname(QString text)
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
	exec_list->last()->startup_item = true;
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
	l->cursorName = cursorname;
	l->commandName = commandname;
	l->command_putother = command_putother;
	l->sqlName = sqlname;
	l->incfileName = incfilename;
	l->cursor_hold = cursor_hold;
	l->src_file = lexer.src_location_stack.top().filename;

	l->startup_item = 0;
	l->sql_query_list_id = sqlnum;

	host_reference_list = new QList<cb_hostreference_ptr>();
	res_host_reference_list = new QList<cb_res_hostreference_ptr>();
	sql_list = new QList<cb_sql_token_t>();

	exec_list->append(l);

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

cb_field_ptr gix_esql_driver::cb_build_field_tree(int level, QString name, cb_field_ptr last_field)
{
	int lv;
	cb_field_ptr f, p;

	if (name == NULL)
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
			fprintf(stderr, "parse error: %s level should start from 01 or 66 or 77 or 88\n", name.toUtf8().constData());
			exit(-1);
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
		QString path = f->sname;
		cb_field_ptr p = f;
		while (p->parent) {
			path = p->parent->sname + ":" + path;
			p = p->parent;
		}

		if (in_ws_section) {
			f->path = "WS:" + path;
			f->data_section = DataSectionType::WorkingStorage;
		}
		else 
			if (in_linkage_section) {
				f->path = "LS:" + path;
				f->data_section = DataSectionType::LinkageSection;
			}
			else
				if (in_file_section) {
					f->path = "FS:" + path;
					f->data_section = DataSectionType::FileSection;
				}

		field_map[f->sname] = f;
	}

	f->defined_at_source_line = lexer.getLineNo();
	f->defined_at_source_file = QString::fromStdString(lexer.driver->file);

	return f;
}

int gix_esql_driver::build_picture(const QString str, cb_field_ptr pic)
{
	auto ba = str.toLocal8Bit();
    const char *p = ba.data();

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
