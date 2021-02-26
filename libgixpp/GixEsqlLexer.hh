#pragma once

#ifndef YY_DECL
#define YY_DECL                                                         \
    yy::gix_esql_parser::symbol_type GixEsqlLexer::yylex(gix_esql_driver& driver)
#endif

// We need this for yyFlexLexer. If we don't #undef yyFlexLexer, the
// preprocessor chokes on the line `#define yyFlexLexer yyFlexLexer`
// in `FlexLexer.h`:
#undef yyFlexLexer
#include <FlexLexer.h>
#include <QStack>
#include <QMap>
#include <QString>

// We need this for the yy::calcxx_parser::symbol_type:
#include "gix_esql_parser.hh"

#define BUFFSIZE    512

struct srcLocation
{
    QString filename;
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

    int LexerInput(char *buf, int max_size);

    void push_state(int s) { this->yy_push_state(s); }

    void pushNewFile(const QString file_name, gix_esql_driver *driver, bool resolve_as_copy, bool is_included);

    QStack<srcLocation> src_location_stack;

    int getLineNo() { return yylineno;  }

    QString cur_line_content;


private:
    bool isParagraph(const QString &text);

};
