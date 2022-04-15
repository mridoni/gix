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

#include "GixEsqlLexer.hh"
#include "gix_esql_driver.hh"
#include "libcpputils.h"

#include <istream>
#include <filesystem>
#include <regex>

#define YY_NULL 0

/* Size of default input buffer. */
#ifndef YY_BUF_SIZE
#ifdef __ia64__
/* On IA-64, the buffer size is 16k, not 8k.
 * Moreover, YY_BUF_SIZE is 2*YY_READ_BUF_SIZE in the general case.
 * Ditto for the __ia64__ case accordingly.
 */
#define YY_BUF_SIZE 32768
#else
#define YY_BUF_SIZE 16384
#endif /* __ia64__ */
#endif

//static QRegularExpression rxUserDefinedCobolWord(R"([A-Za-z0-9]+ ([\-]+ [A-Za-z0-9]+)*)");
static std::regex rxUserDefinedCobolWord(R"(^[A-Za-z0-9]+([\-]+[A-Za-z0-9]+)*$)");

int GixEsqlLexer::LexerInput(char *buff, int max_size)
{
	char *bp;
	char *comment;

	memset(buff, 0, max_size);
	
	while (yyin.getline(buff, max_size)) {

		cur_line_content = buff;

		if (driver->pp_inst->verbose_debug)
			printf("%05d : %s\n", yylineno + 1, buff);

		// This is needed to properly consume EOLs (yyin.getline discards them)
#if (defined(_WIN32) || defined(_WIN64)) && !defined(__MINGW32__)
		strcat(buff, "\r\n");
#else
		strcat(buff, "\n");
#endif

		if (strlen(buff) > 7) {
			bp = buff + 7;

			switch (buff[6]) {
				case ' ':
					break;
				case '-':
					break;

				case '\r':
				case '\n':
				case '\0':
					/* ignore line */
					strcpy(buff, "\n");
					return strlen(buff);

				case '*':
					/* comment line */
					strcpy(buff, "\n");
					return strlen(buff);

				case '/':
					/* comment line */
					strcpy(buff, "\n");
					return strlen(buff);

				case 'D':
					/* comment line */
					strcpy(buff, "\n");
					return strlen(buff);

				case 'd':
					/* comment line */
					strcpy(buff, "\n");
					return strlen(buff);

				case '$':
					/* comment line */
					strcpy(buff, "\n");
					return strlen(buff);

				case '>':
					/* preprocessor line, treat as comment */
					strcpy(buff, "\n");
					return strlen(buff);

				default:
					printf("EOF:%s\n", buff);
					return YY_NULL;
			}
			if (strlen(buff) > 72) {
				memmove(buff, bp, 65);
				strcpy(buff + 65, "\n");
			}
			else {
				memmove(buff, bp, strlen(bp) + 1);
			}

			comment = strstr(buff, "*>");
			if (comment) strcpy(comment, "\n");
			return strlen(buff);
		}
		strcpy(buff, "\n");
		
		return strlen(buff);
	}

	return 0;
}

void GixEsqlLexer::pushNewFile(const std::string file_name, gix_esql_driver *driver, bool resolve_as_copy, bool is_included)
{
	std::string file_full_name = file_name;

	if (driver->pp_inst->verbose_debug)
		printf("Resolving %s\n", file_name.c_str());

	if (resolve_as_copy) {
		if (!driver->pp_inst->getCopyResolver()->resolveCopyFile(file_name, file_full_name)) {
			driver->error("Cannot resolve copy file " + file_name, ERR_MISSING_COPYFILE);
			return;
		}
	}

	std::istream *in_file = new std::ifstream(file_full_name);
	yy_buffer_state *new_buffer = yy_create_buffer(in_file, YY_BUF_SIZE);

	if (driver->pp_inst->verbose_debug)
		printf("Switching to file %s\n", file_full_name.c_str());

	yypush_buffer_state(new_buffer);

	srcLocation *loc = new srcLocation();
	std::filesystem::path file_full_path(file_full_name);
	loc->filename = std::filesystem::absolute(file_full_path).string();
	loc->line = yylineno;
	loc->is_included = is_included;

	this->src_location_stack.push(*loc);

	driver->file = file_full_name;
	driver->hostlineno = 1;
	yylineno = 1;
}

bool GixEsqlLexer::isParagraph(const std::string &text)
{
	if (!driver->procedure_division_started)
		return false;

	std::string t = trim_copy(string_chop(trim_copy(text), 1));

	bool b = std::regex_match(t, rxUserDefinedCobolWord) && std::find(reserved_words_list.begin(), reserved_words_list.end(), t) == reserved_words_list.end();
	return b;
}


int yyFlexLexer::yywrap()
{
	GixEsqlLexer *p = (GixEsqlLexer *)this;
	if (yy_buffer_stack_top > 0) {
		yypop_buffer_state();

		srcLocation loc = p->driver->lexer.src_location_stack.top();
		p->driver->lexer.src_location_stack.pop();

		p->driver->hostlineno = loc.line;
		// Used to be:
		// p->driver->file = loc.filename.toStdString();
		p->driver->file = p->driver->lexer.src_location_stack.top().filename;
		yylineno = loc.line;

		if (p->driver->pp_inst->verbose_debug)
			printf("Switching to file %s\n", p->driver->lexer.src_location_stack.top().filename.c_str());

		return 0;
	}

	return 1;
}