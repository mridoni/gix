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

#ifndef YY_DECL
#define YY_DECL                                                         \
    yy::gix_esql_parser::symbol_type GixEsqlLexer::yylex(gix_esql_driver& driver)
#endif

// We need this for yyFlexLexer. If we don't #undef yyFlexLexer, the
// preprocessor chokes on the line `#define yyFlexLexer yyFlexLexer`
// in `FlexLexer.h`:
#undef yyFlexLexer
#include "FlexLexer.h"

#include <stack>
#include <map>
#include <string>

#include "gix_esql_parser.hh"

struct srcLocation
{
    std::string filename;
    int line;
    bool is_included;
};

class GixEsqlLexer : public yyFlexLexer {
public:
    // Use the superclass's constructor:
    //using yyFlexLexer::yyFlexLexer;

    GixEsqlLexer() : yyFlexLexer()
    {
        driver = nullptr;
    }

    gix_esql_driver *driver;

    // Provide the interface to `yylex`; `flex` will emit the
    // definition into `gix_esql_scanner.cc`:
    yy::gix_esql_parser::symbol_type yylex(gix_esql_driver& driver);
    
    void setDriver(gix_esql_driver *_driver) { driver = _driver;  }
    void setReservedWordsList(const std::vector<std::string> &rwl) { reserved_words_list = rwl; }

    int LexerInput(char *buf, int max_size);

    void push_state(int s) { this->yy_push_state(s); }

    void pushNewFile(const std::string file_name, gix_esql_driver *driver, bool resolve_as_copy, bool is_included);

    std::stack<srcLocation> src_location_stack;

    int getLineNo() { return yylineno;  }

    std::string cur_line_content;




private:
    bool isParagraph(const std::string &text);

    std::vector<std::string> reserved_words_list;

#if defined (_DEBUG) && defined (VERBOSE)

    #define __YY_START (((yy_start) - 1) / 2)

    const char *yy_state_desc(int i)
    {
        if (i >= 0 && i < NUM_YY_STATES) {
            return yy_state_descs[i];
        }

        return "(UNKNOWN_STATE)";
    }

    void __yy_push_state(int newstate)
    {
        int oldstate = __YY_START;
        yy_push_state(newstate);
        fprintf(stderr, "%04d =============>>> PUSHING STATE %s -> %s\n", yylineno, yy_state_desc(oldstate), yy_state_desc(newstate));
    }

    void __yy_pop_state()
    {
        //int oldstate = (!yy_start_stack) ? 0 : yy_top_state();
        int oldstate = __YY_START;
        yy_pop_state();
        //int newstate = (!yy_start_stack) ? 0 : yy_top_state();
        int newstate = __YY_START;
        fprintf(stderr, "%04d =============<<< POPPING STATE: %s -> %s\n", yylineno, yy_state_desc(oldstate), yy_state_desc(newstate));
    }

    static const int NUM_YY_STATES = 17;
    static const char *yy_state_descs[NUM_YY_STATES];
    
#else
#define __yy_push_state(s) yy_push_state(s)
#define __yy_pop_state() yy_pop_state()
#endif
};
