// A Bison parser, made by GNU Bison 3.7.4.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2020 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.





#include "gix_esql_parser.hh"


// Unqualified %code blocks.
#line 62 "gix_esql_parser.yy"

#include "gix_esql_driver.hh"
#define yylex driver.lexer.yylex

static std::string to_std_string(QString *sp) { return (sp != NULL) ? sp->toStdString() : "(NULL)"; }
static std::string to_std_string(const QString s) { return s.toStdString(); }
static std::string to_std_string(const QList<QString> *slp) { if (!slp) return "(NULL-LIST)"; int n = slp->size() > 3 ? 3 : slp->size(); QString res; for (int i = 0; i < n; i++) res += slp->at(i); return (res + " ...").toStdString(); }
static std::string to_std_string(const int i) { return ""; }


#line 57 "gix_esql_parser.cc"


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif


// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
# if defined __GNUC__ && !defined __EXCEPTIONS
#  define YY_EXCEPTIONS 0
# else
#  define YY_EXCEPTIONS 1
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (false)
# endif


// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << '\n';                       \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yy_stack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE (Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void> (0)
# define YY_STACK_PRINT()                static_cast<void> (0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

namespace yy {
#line 149 "gix_esql_parser.cc"

  /// Build a parser object.
  gix_esql_parser::gix_esql_parser (gix_esql_driver& driver_yyarg)
#if YYDEBUG
    : yydebug_ (false),
      yycdebug_ (&std::cerr),
#else
    :
#endif
      driver (driver_yyarg)
  {}

  gix_esql_parser::~gix_esql_parser ()
  {}

  gix_esql_parser::syntax_error::~syntax_error () YY_NOEXCEPT YY_NOTHROW
  {}

  /*---------------.
  | symbol kinds.  |
  `---------------*/



  // by_state.
  gix_esql_parser::by_state::by_state () YY_NOEXCEPT
    : state (empty_state)
  {}

  gix_esql_parser::by_state::by_state (const by_state& that) YY_NOEXCEPT
    : state (that.state)
  {}

  void
  gix_esql_parser::by_state::clear () YY_NOEXCEPT
  {
    state = empty_state;
  }

  void
  gix_esql_parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  gix_esql_parser::by_state::by_state (state_type s) YY_NOEXCEPT
    : state (s)
  {}

  gix_esql_parser::symbol_kind_type
  gix_esql_parser::by_state::kind () const YY_NOEXCEPT
  {
    if (state == empty_state)
      return symbol_kind::S_YYEMPTY;
    else
      return YY_CAST (symbol_kind_type, yystos_[+state]);
  }

  gix_esql_parser::stack_symbol_type::stack_symbol_type ()
  {}

  gix_esql_parser::stack_symbol_type::stack_symbol_type (YY_RVREF (stack_symbol_type) that)
    : super_type (YY_MOVE (that.state), YY_MOVE (that.location))
  {
    switch (that.kind ())
    {
      case symbol_kind::S_updatesql: // updatesql
      case symbol_kind::S_update: // update
      case symbol_kind::S_disconnectsql: // disconnectsql
      case symbol_kind::S_disconnect: // disconnect
      case symbol_kind::S_deletesql: // deletesql
      case symbol_kind::S_delete: // delete
      case symbol_kind::S_insertsql: // insertsql
      case symbol_kind::S_insert: // insert
      case symbol_kind::S_opensql: // opensql
      case symbol_kind::S_othersql: // othersql
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
      case symbol_kind::S_declare_cursor: // declare_cursor
        value.YY_MOVE_OR_COPY< QList<QString> * > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_SELECT: // SELECT
      case symbol_kind::S_SELECTFROM: // SELECTFROM
      case symbol_kind::S_TOKEN: // TOKEN
      case symbol_kind::S_HOSTTOKEN: // HOSTTOKEN
      case symbol_kind::S_WORD: // WORD
      case symbol_kind::S_PICTURE: // PICTURE
      case symbol_kind::S_INSERT: // INSERT
      case symbol_kind::S_UPDATE: // UPDATE
      case symbol_kind::S_DISCONNECT: // DISCONNECT
      case symbol_kind::S_DELETE: // DELETE
      case symbol_kind::S_EXECUTE: // EXECUTE
      case symbol_kind::S_OTHERFUNC: // OTHERFUNC
      case symbol_kind::S_INTO: // INTO
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_declare_table: // declare_table
      case symbol_kind::S_declare_special: // declare_special
        value.YY_MOVE_OR_COPY< QString > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.YY_MOVE_OR_COPY< long > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
  }

  gix_esql_parser::stack_symbol_type::stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) that)
    : super_type (s, YY_MOVE (that.location))
  {
    switch (that.kind ())
    {
      case symbol_kind::S_updatesql: // updatesql
      case symbol_kind::S_update: // update
      case symbol_kind::S_disconnectsql: // disconnectsql
      case symbol_kind::S_disconnect: // disconnect
      case symbol_kind::S_deletesql: // deletesql
      case symbol_kind::S_delete: // delete
      case symbol_kind::S_insertsql: // insertsql
      case symbol_kind::S_insert: // insert
      case symbol_kind::S_opensql: // opensql
      case symbol_kind::S_othersql: // othersql
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
      case symbol_kind::S_declare_cursor: // declare_cursor
        value.move< QList<QString> * > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_SELECT: // SELECT
      case symbol_kind::S_SELECTFROM: // SELECTFROM
      case symbol_kind::S_TOKEN: // TOKEN
      case symbol_kind::S_HOSTTOKEN: // HOSTTOKEN
      case symbol_kind::S_WORD: // WORD
      case symbol_kind::S_PICTURE: // PICTURE
      case symbol_kind::S_INSERT: // INSERT
      case symbol_kind::S_UPDATE: // UPDATE
      case symbol_kind::S_DISCONNECT: // DISCONNECT
      case symbol_kind::S_DELETE: // DELETE
      case symbol_kind::S_EXECUTE: // EXECUTE
      case symbol_kind::S_OTHERFUNC: // OTHERFUNC
      case symbol_kind::S_INTO: // INTO
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_declare_table: // declare_table
      case symbol_kind::S_declare_special: // declare_special
        value.move< QString > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.move< long > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

    // that is emptied.
    that.kind_ = symbol_kind::S_YYEMPTY;
  }

#if YY_CPLUSPLUS < 201103L
  gix_esql_parser::stack_symbol_type&
  gix_esql_parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    switch (that.kind ())
    {
      case symbol_kind::S_updatesql: // updatesql
      case symbol_kind::S_update: // update
      case symbol_kind::S_disconnectsql: // disconnectsql
      case symbol_kind::S_disconnect: // disconnect
      case symbol_kind::S_deletesql: // deletesql
      case symbol_kind::S_delete: // delete
      case symbol_kind::S_insertsql: // insertsql
      case symbol_kind::S_insert: // insert
      case symbol_kind::S_opensql: // opensql
      case symbol_kind::S_othersql: // othersql
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
      case symbol_kind::S_declare_cursor: // declare_cursor
        value.copy< QList<QString> * > (that.value);
        break;

      case symbol_kind::S_SELECT: // SELECT
      case symbol_kind::S_SELECTFROM: // SELECTFROM
      case symbol_kind::S_TOKEN: // TOKEN
      case symbol_kind::S_HOSTTOKEN: // HOSTTOKEN
      case symbol_kind::S_WORD: // WORD
      case symbol_kind::S_PICTURE: // PICTURE
      case symbol_kind::S_INSERT: // INSERT
      case symbol_kind::S_UPDATE: // UPDATE
      case symbol_kind::S_DISCONNECT: // DISCONNECT
      case symbol_kind::S_DELETE: // DELETE
      case symbol_kind::S_EXECUTE: // EXECUTE
      case symbol_kind::S_OTHERFUNC: // OTHERFUNC
      case symbol_kind::S_INTO: // INTO
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_declare_table: // declare_table
      case symbol_kind::S_declare_special: // declare_special
        value.copy< QString > (that.value);
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.copy< long > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    return *this;
  }

  gix_esql_parser::stack_symbol_type&
  gix_esql_parser::stack_symbol_type::operator= (stack_symbol_type& that)
  {
    state = that.state;
    switch (that.kind ())
    {
      case symbol_kind::S_updatesql: // updatesql
      case symbol_kind::S_update: // update
      case symbol_kind::S_disconnectsql: // disconnectsql
      case symbol_kind::S_disconnect: // disconnect
      case symbol_kind::S_deletesql: // deletesql
      case symbol_kind::S_delete: // delete
      case symbol_kind::S_insertsql: // insertsql
      case symbol_kind::S_insert: // insert
      case symbol_kind::S_opensql: // opensql
      case symbol_kind::S_othersql: // othersql
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
      case symbol_kind::S_declare_cursor: // declare_cursor
        value.move< QList<QString> * > (that.value);
        break;

      case symbol_kind::S_SELECT: // SELECT
      case symbol_kind::S_SELECTFROM: // SELECTFROM
      case symbol_kind::S_TOKEN: // TOKEN
      case symbol_kind::S_HOSTTOKEN: // HOSTTOKEN
      case symbol_kind::S_WORD: // WORD
      case symbol_kind::S_PICTURE: // PICTURE
      case symbol_kind::S_INSERT: // INSERT
      case symbol_kind::S_UPDATE: // UPDATE
      case symbol_kind::S_DISCONNECT: // DISCONNECT
      case symbol_kind::S_DELETE: // DELETE
      case symbol_kind::S_EXECUTE: // EXECUTE
      case symbol_kind::S_OTHERFUNC: // OTHERFUNC
      case symbol_kind::S_INTO: // INTO
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_declare_table: // declare_table
      case symbol_kind::S_declare_special: // declare_special
        value.move< QString > (that.value);
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
        value.move< int > (that.value);
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.move< long > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    // that is emptied.
    that.state = empty_state;
    return *this;
  }
#endif

  template <typename Base>
  void
  gix_esql_parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);
  }

#if YYDEBUG
  template <typename Base>
  void
  gix_esql_parser::yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    if (yysym.empty ())
      yyo << "empty symbol";
    else
      {
        symbol_kind_type yykind = yysym.kind ();
        yyo << (yykind < YYNTOKENS ? "token" : "nterm")
            << ' ' << yysym.name () << " ("
            << yysym.location << ": ";
        switch (yykind)
    {
      case symbol_kind::S_SELECT: // SELECT
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 492 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_SELECTFROM: // SELECTFROM
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 498 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_TOKEN: // TOKEN
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 504 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_HOSTTOKEN: // HOSTTOKEN
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 510 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_WORD: // WORD
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 516 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_PICTURE: // PICTURE
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 522 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_INSERT: // INSERT
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 528 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_UPDATE: // UPDATE
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 534 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_DISCONNECT: // DISCONNECT
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 540 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_DELETE: // DELETE
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 546 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_EXECUTE: // EXECUTE
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 552 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_OTHERFUNC: // OTHERFUNC
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 558 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_INTO: // INTO
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 564 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < long > ()); }
#line 570 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < int > ()); }
#line 576 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_updatesql: // updatesql
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 582 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_update: // update
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 588 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_disconnectsql: // disconnectsql
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 594 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_disconnect: // disconnect
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 600 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_deletesql: // deletesql
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 606 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_delete: // delete
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 612 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_insertsql: // insertsql
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 618 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_insert: // insert
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 624 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opensql: // opensql
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 630 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_othersql: // othersql
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 636 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_incfile: // incfile
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 642 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_includesql: // includesql
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 648 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_selectintosql: // selectintosql
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 654 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_declaresql: // declaresql
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 660 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_select: // select
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 666 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_token_list: // token_list
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 672 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_host_reference: // host_reference
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 678 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_expr: // expr
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 684 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_declare_cursor: // declare_cursor
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QList<QString> * > ()); }
#line 690 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_declare_table: // declare_table
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 696 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_declare_special: // declare_special
#line 161 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < QString > ()); }
#line 702 "gix_esql_parser.cc"
        break;

      default:
        break;
    }
        yyo << ')';
      }
  }
#endif

  void
  gix_esql_parser::yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym)
  {
    if (m)
      YY_SYMBOL_PRINT (m, sym);
    yystack_.push (YY_MOVE (sym));
  }

  void
  gix_esql_parser::yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym)
  {
#if 201103L <= YY_CPLUSPLUS
    yypush_ (m, stack_symbol_type (s, std::move (sym)));
#else
    stack_symbol_type ss (s, sym);
    yypush_ (m, ss);
#endif
  }

  void
  gix_esql_parser::yypop_ (int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  gix_esql_parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  gix_esql_parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  gix_esql_parser::debug_level_type
  gix_esql_parser::debug_level () const
  {
    return yydebug_;
  }

  void
  gix_esql_parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  gix_esql_parser::state_type
  gix_esql_parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - YYNTOKENS] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - YYNTOKENS];
  }

  bool
  gix_esql_parser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  bool
  gix_esql_parser::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  gix_esql_parser::operator() ()
  {
    return parse ();
  }

  int
  gix_esql_parser::parse ()
  {
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The locations where the error started and ended.
    stack_symbol_type yyerror_range[3];

    /// The return value of parse ().
    int yyresult;

#if YY_EXCEPTIONS
    try
#endif // YY_EXCEPTIONS
      {
    YYCDEBUG << "Starting parse\n";


    // User initialization code.
#line 51 "gix_esql_parser.yy"
{
    // Initialize the initial location.
    yyla.location.begin.filename = yyla.location.end.filename = &driver.file;
}

#line 827 "gix_esql_parser.cc"


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, YY_MOVE (yyla));

  /*-----------------------------------------------.
  | yynewstate -- push a new symbol on the stack.  |
  `-----------------------------------------------*/
  yynewstate:
    YYCDEBUG << "Entering state " << int (yystack_[0].state) << '\n';
    YY_STACK_PRINT ();

    // Accept?
    if (yystack_[0].state == yyfinal_)
      YYACCEPT;

    goto yybackup;


  /*-----------.
  | yybackup.  |
  `-----------*/
  yybackup:
    // Try to take a decision without lookahead.
    yyn = yypact_[+yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token\n";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
          {
            symbol_type yylookahead (yylex (driver));
            yyla.move (yylookahead);
          }
#if YY_EXCEPTIONS
        catch (const syntax_error& yyexc)
          {
            YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
            error (yyexc);
            goto yyerrlab1;
          }
#endif // YY_EXCEPTIONS
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    if (yyla.kind () == symbol_kind::S_YYerror)
    {
      // The scanner already issued an error message, process directly
      // to error recovery.  But do not keep the error token as
      // lookahead, it is too special and may lead us to an endless
      // loop in error recovery. */
      yyla.kind_ = symbol_kind::S_YYUNDEF;
      goto yyerrlab1;
    }

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.kind ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.kind ())
      {
        goto yydefault;
      }

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", state_type (yyn), YY_MOVE (yyla));
    goto yynewstate;


  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[+yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;


  /*-----------------------------.
  | yyreduce -- do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_ (yystack_[yylen].state, yyr1_[yyn]);
      /* Variants are always initialized to an empty instance of the
         correct type. The default '$$ = $1' action is NOT applied
         when using variants.  */
      switch (yyr1_[yyn])
    {
      case symbol_kind::S_updatesql: // updatesql
      case symbol_kind::S_update: // update
      case symbol_kind::S_disconnectsql: // disconnectsql
      case symbol_kind::S_disconnect: // disconnect
      case symbol_kind::S_deletesql: // deletesql
      case symbol_kind::S_delete: // delete
      case symbol_kind::S_insertsql: // insertsql
      case symbol_kind::S_insert: // insert
      case symbol_kind::S_opensql: // opensql
      case symbol_kind::S_othersql: // othersql
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
      case symbol_kind::S_declare_cursor: // declare_cursor
        yylhs.value.emplace< QList<QString> * > ();
        break;

      case symbol_kind::S_SELECT: // SELECT
      case symbol_kind::S_SELECTFROM: // SELECTFROM
      case symbol_kind::S_TOKEN: // TOKEN
      case symbol_kind::S_HOSTTOKEN: // HOSTTOKEN
      case symbol_kind::S_WORD: // WORD
      case symbol_kind::S_PICTURE: // PICTURE
      case symbol_kind::S_INSERT: // INSERT
      case symbol_kind::S_UPDATE: // UPDATE
      case symbol_kind::S_DISCONNECT: // DISCONNECT
      case symbol_kind::S_DELETE: // DELETE
      case symbol_kind::S_EXECUTE: // EXECUTE
      case symbol_kind::S_OTHERFUNC: // OTHERFUNC
      case symbol_kind::S_INTO: // INTO
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_declare_table: // declare_table
      case symbol_kind::S_declare_special: // declare_special
        yylhs.value.emplace< QString > ();
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
        yylhs.value.emplace< int > ();
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        yylhs.value.emplace< long > ();
        break;

      default:
        break;
    }


      // Default location.
      {
        stack_type::slice range (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, range, yylen);
        yyerror_range[1].location = yylhs.location;
      }

      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
#if YY_EXCEPTIONS
      try
#endif // YY_EXCEPTIONS
        {
          switch (yyn)
            {
  case 21: // updatesql: EXECSQL update token_list END_EXEC
#line 189 "gix_esql_parser.yy"
{
	yylhs.value.as < QList<QString> * > () = driver.cb_concat_text_list (yystack_[2].value.as < QList<QString> * > (), yystack_[1].value.as < QList<QString> * > ());
	driver.put_exec_list();
}
#line 1016 "gix_esql_parser.cc"
    break;

  case 22: // updatesql: EXECSQL update token_list WHERE_CURRENT_OF expr END_EXEC
#line 194 "gix_esql_parser.yy"
{
	driver.cb_set_cursorname(yystack_[1].value.as < QString > ());
	yylhs.value.as < QList<QString> * > () = driver.cb_concat_text_list (yystack_[4].value.as < QList<QString> * > (), yystack_[3].value.as < QList<QString> * > ());
	driver.cb_concat_text_list(yylhs.value.as < QList<QString> * > (), driver.cb_text_list_add(NULL, "WHERE CURRENT OF"));
	driver.cb_concat_text_list(yylhs.value.as < QList<QString> * > (), driver.cb_text_list_add(NULL, driver.cursorname));
	driver.put_exec_list();
}
#line 1028 "gix_esql_parser.cc"
    break;

  case 23: // update: UPDATE
#line 203 "gix_esql_parser.yy"
       {yylhs.value.as < QList<QString> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < QString > ());}
#line 1034 "gix_esql_parser.cc"
    break;

  case 24: // disconnectsql: EXECSQL disconnect token_list END_EXEC
#line 208 "gix_esql_parser.yy"
{
	yylhs.value.as < QList<QString> * > () = driver.cb_concat_text_list (yystack_[2].value.as < QList<QString> * > (), yystack_[1].value.as < QList<QString> * > ());
	driver.put_exec_list();
}
#line 1043 "gix_esql_parser.cc"
    break;

  case 25: // disconnect: DISCONNECT
#line 214 "gix_esql_parser.yy"
           {yylhs.value.as < QList<QString> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < QString > ());}
#line 1049 "gix_esql_parser.cc"
    break;

  case 26: // deletesql: EXECSQL delete token_list END_EXEC
#line 218 "gix_esql_parser.yy"
{
	yylhs.value.as < QList<QString> * > () = driver.cb_concat_text_list (yystack_[2].value.as < QList<QString> * > (), yystack_[1].value.as < QList<QString> * > ());
	driver.put_exec_list();
}
#line 1058 "gix_esql_parser.cc"
    break;

  case 27: // delete: DELETE
#line 225 "gix_esql_parser.yy"
       {yylhs.value.as < QList<QString> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < QString > ());}
#line 1064 "gix_esql_parser.cc"
    break;

  case 28: // insertsql: EXECSQL insert token_list END_EXEC
#line 229 "gix_esql_parser.yy"
{
	yylhs.value.as < QList<QString> * > () = driver.cb_concat_text_list (yystack_[2].value.as < QList<QString> * > (), yystack_[1].value.as < QList<QString> * > ());
	driver.put_exec_list();
}
#line 1073 "gix_esql_parser.cc"
    break;

  case 29: // insert: INSERT
#line 235 "gix_esql_parser.yy"
       {yylhs.value.as < QList<QString> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < QString > ());}
#line 1079 "gix_esql_parser.cc"
    break;

  case 30: // insert: insert INTO
#line 236 "gix_esql_parser.yy"
              {yylhs.value.as < QList<QString> * > () = driver.cb_text_list_add (yystack_[1].value.as < QList<QString> * > (), yystack_[0].value.as < QString > ());}
#line 1085 "gix_esql_parser.cc"
    break;

  case 31: // rollbacksql: EXECSQL ROLLBACK_WORK END_EXEC
#line 241 "gix_esql_parser.yy"
                               {
	driver.put_exec_list();
}
#line 1093 "gix_esql_parser.cc"
    break;

  case 32: // commitsql: EXECSQL COMMIT_WORK END_EXEC
#line 246 "gix_esql_parser.yy"
                             {
	driver.put_exec_list();
}
#line 1101 "gix_esql_parser.cc"
    break;

  case 33: // fetchsql: EXECSQL fetch INTO res_host_references END_EXEC
#line 252 "gix_esql_parser.yy"
                                                {
	driver.put_exec_list();
}
#line 1109 "gix_esql_parser.cc"
    break;

  case 34: // fetch: FETCH expr
#line 256 "gix_esql_parser.yy"
           { driver.cb_set_cursorname(yystack_[0].value.as < QString > ());}
#line 1115 "gix_esql_parser.cc"
    break;

  case 35: // host_references: host_reference
#line 259 "gix_esql_parser.yy"
               {driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < QString > ());}
#line 1121 "gix_esql_parser.cc"
    break;

  case 37: // host_references: host_references host_reference
#line 261 "gix_esql_parser.yy"
                                 {driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < QString > ());}
#line 1127 "gix_esql_parser.cc"
    break;

  case 38: // res_host_references: host_reference
#line 264 "gix_esql_parser.yy"
               {driver.cb_res_host_list_add (driver.res_host_reference_list, yystack_[0].value.as < QString > ());}
#line 1133 "gix_esql_parser.cc"
    break;

  case 40: // res_host_references: res_host_references host_reference
#line 266 "gix_esql_parser.yy"
                                     {driver.cb_res_host_list_add (driver.res_host_reference_list, yystack_[0].value.as < QString > ());}
#line 1139 "gix_esql_parser.cc"
    break;

  case 41: // closesql: EXECSQL CLOSE expr END_EXEC
#line 269 "gix_esql_parser.yy"
                            {
	driver.cb_set_cursorname(yystack_[1].value.as < QString > ());
	driver.put_exec_list();
}
#line 1148 "gix_esql_parser.cc"
    break;

  case 42: // opensql: EXECSQL OPEN expr END_EXEC
#line 275 "gix_esql_parser.yy"
                           {
	driver.cb_set_cursorname(yystack_[1].value.as < QString > ());
	driver.put_exec_list();
}
#line 1157 "gix_esql_parser.cc"
    break;

  case 43: // opensql: EXECSQL OPEN expr USING host_references END_EXEC
#line 279 "gix_esql_parser.yy"
                                                   {
	driver.cb_set_cursorname(yystack_[3].value.as < QString > ());
	driver.put_exec_list();
}
#line 1166 "gix_esql_parser.cc"
    break;

  case 44: // connectsql: EXECSQL connect identified using END_EXEC
#line 285 "gix_esql_parser.yy"
                                          { driver.put_exec_list(); }
#line 1172 "gix_esql_parser.cc"
    break;

  case 45: // connectsql: EXECSQL connect END_EXEC
#line 286 "gix_esql_parser.yy"
                           { driver.put_exec_list(); }
#line 1178 "gix_esql_parser.cc"
    break;

  case 46: // connectsql: EXECSQL connect_to END_EXEC
#line 287 "gix_esql_parser.yy"
                              { driver.put_exec_list(); }
#line 1184 "gix_esql_parser.cc"
    break;

  case 47: // connect_to: CONNECT_TO HOSTTOKEN USER HOSTTOKEN
#line 290 "gix_esql_parser.yy"
                                    { 
	driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < QString > ());
	driver.cb_host_list_add_force (driver.host_reference_list, yystack_[0].value.as < QString > ());
	driver.cb_host_list_add (driver.host_reference_list, yystack_[2].value.as < QString > ());
}
#line 1194 "gix_esql_parser.cc"
    break;

  case 48: // resetsql: EXECSQL CONNECT_RESET END_EXEC
#line 298 "gix_esql_parser.yy"
                               { 
 driver.put_exec_list();
 }
#line 1202 "gix_esql_parser.cc"
    break;

  case 49: // othersql: EXECSQL OTHERFUNC token_list END_EXEC
#line 303 "gix_esql_parser.yy"
                                      {
	yylhs.value.as < QList<QString> * > () = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, yystack_[2].value.as < QString > ()), yystack_[1].value.as < QList<QString> * > ());
	driver.put_exec_list();
}
#line 1211 "gix_esql_parser.cc"
    break;

  case 50: // connect: CONNECT host_reference
#line 309 "gix_esql_parser.yy"
                       {
	driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < QString > ());
}
#line 1219 "gix_esql_parser.cc"
    break;

  case 51: // identified: IDENTIFIED_BY host_reference
#line 315 "gix_esql_parser.yy"
                             {
	driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < QString > ());
}
#line 1227 "gix_esql_parser.cc"
    break;

  case 52: // using: USING host_reference
#line 320 "gix_esql_parser.yy"
                     {
	driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < QString > ());
}
#line 1235 "gix_esql_parser.cc"
    break;

  case 53: // incfile: EXECSQL_INCLUDE INCLUDE_FILE END_EXEC
#line 326 "gix_esql_parser.yy"
                                     {
	driver.put_exec_list();
	driver.lexer.pushNewFile(driver.incfilename, &driver, true, true);
}
#line 1244 "gix_esql_parser.cc"
    break;

  case 54: // incfile: COPY
#line 330 "gix_esql_parser.yy"
       { 
	driver.put_exec_list(); 

	driver.lexer.pushNewFile(driver.incfilename, &driver, true, false);
}
#line 1254 "gix_esql_parser.cc"
    break;

  case 55: // includesql: EXECSQL_INCLUDE INCLUDE_SQLCA END_EXEC
#line 337 "gix_esql_parser.yy"
                                      {
	driver.put_exec_list();
}
#line 1262 "gix_esql_parser.cc"
    break;

  case 56: // selectintosql: EXECSQL SELECT token_list INTO res_host_references SELECTFROM token_list END_EXEC
#line 342 "gix_esql_parser.yy"
                                                                                   {
	yylhs.value.as < QList<QString> * > () = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, yystack_[6].value.as < QString > ()), yystack_[5].value.as < QList<QString> * > ());
	driver.cb_concat_text_list(yylhs.value.as < QList<QString> * > (), driver.cb_text_list_add(NULL, yystack_[2].value.as < QString > ()));
	driver.cb_concat_text_list(yylhs.value.as < QList<QString> * > (), yystack_[1].value.as < QList<QString> * > ());
	driver.put_exec_list();
}
#line 1273 "gix_esql_parser.cc"
    break;

  case 57: // selectintosql: EXECSQL SELECT token_list INTO res_host_references END_EXEC
#line 348 "gix_esql_parser.yy"
                                                               {
	yylhs.value.as < QList<QString> * > () = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, yystack_[4].value.as < QString > ()), yystack_[3].value.as < QList<QString> * > ());
	driver.put_exec_list();
}
#line 1282 "gix_esql_parser.cc"
    break;

  case 58: // declaresql: EXECSQL declare_for select END_EXEC
#line 355 "gix_esql_parser.yy"
                                    { driver.put_exec_list(); }
#line 1288 "gix_esql_parser.cc"
    break;

  case 59: // select: SELECT token_list
#line 358 "gix_esql_parser.yy"
                 { yylhs.value.as < QList<QString> * > () = driver.cb_concat_text_list (driver.cb_text_list_add (NULL, yystack_[1].value.as < QString > ()), yystack_[0].value.as < QList<QString> * > ());}
#line 1294 "gix_esql_parser.cc"
    break;

  case 60: // declare_for: DECLARE expr CURSOR FOR
#line 361 "gix_esql_parser.yy"
                        { driver.cb_set_cursorname(yystack_[2].value.as < QString > ()); driver.cb_set_cursor_hold(0); }
#line 1300 "gix_esql_parser.cc"
    break;

  case 61: // declare_for: DECLARE expr CURSOR WITH_HOLD FOR
#line 362 "gix_esql_parser.yy"
                                    { driver.cb_set_cursorname(yystack_[3].value.as < QString > ()); driver.cb_set_cursor_hold(1); }
#line 1306 "gix_esql_parser.cc"
    break;

  case 62: // token_list: expr
#line 365 "gix_esql_parser.yy"
                                {      yylhs.value.as < QList<QString> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < QString > ());}
#line 1312 "gix_esql_parser.cc"
    break;

  case 63: // token_list: token_list expr
#line 366 "gix_esql_parser.yy"
                        {      yylhs.value.as < QList<QString> * > () = driver.cb_text_list_add (yystack_[1].value.as < QList<QString> * > (), yystack_[0].value.as < QString > ());}
#line 1318 "gix_esql_parser.cc"
    break;

  case 64: // token_list: token_list host_reference
#line 367 "gix_esql_parser.yy"
                              {
	yylhs.value.as < QList<QString> * > () = driver.cb_text_list_add (yystack_[1].value.as < QList<QString> * > (), driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < QString > ()));
}
#line 1326 "gix_esql_parser.cc"
    break;

  case 65: // host_reference: HOSTTOKEN
#line 372 "gix_esql_parser.yy"
          { yylhs.value.as < QString > () = yystack_[0].value.as < QString > (); }
#line 1332 "gix_esql_parser.cc"
    break;

  case 66: // expr: TOKEN
#line 374 "gix_esql_parser.yy"
            { yylhs.value.as < QString > () = yystack_[0].value.as < QString > (); }
#line 1338 "gix_esql_parser.cc"
    break;

  case 67: // expr: SELECT
#line 375 "gix_esql_parser.yy"
       {}
#line 1344 "gix_esql_parser.cc"
    break;

  case 68: // expr: FOR
#line 376 "gix_esql_parser.yy"
     {}
#line 1350 "gix_esql_parser.cc"
    break;

  case 69: // expr: UPDATE
#line 377 "gix_esql_parser.yy"
        {}
#line 1356 "gix_esql_parser.cc"
    break;

  case 70: // $@1: %empty
#line 380 "gix_esql_parser.yy"
                               {
	driver.current_field = NULL;
	driver.description_field = NULL;
	driver.put_exec_list();
}
#line 1366 "gix_esql_parser.cc"
    break;

  case 71: // sqlvariantstates: WORKINGBEGIN $@1 sqlvariantstate_list WORKINGEND
#line 386 "gix_esql_parser.yy"
           {
	// check host_variable
	driver.put_exec_list();
}
#line 1375 "gix_esql_parser.cc"
    break;

  case 72: // $@2: %empty
#line 390 "gix_esql_parser.yy"
              {
	driver.current_field = NULL;
	driver.description_field = NULL;
	driver.put_exec_list();
}
#line 1385 "gix_esql_parser.cc"
    break;

  case 73: // sqlvariantstates: LINKAGEBEGIN $@2 sqlvariantstate_list LINKAGEEND
#line 396 "gix_esql_parser.yy"
           {
	// check host_variable
	driver.put_exec_list();
}
#line 1394 "gix_esql_parser.cc"
    break;

  case 74: // $@3: %empty
#line 400 "gix_esql_parser.yy"
           {
	driver.current_field = NULL;
	driver.description_field = NULL;
	driver.put_exec_list();
}
#line 1404 "gix_esql_parser.cc"
    break;

  case 75: // sqlvariantstates: FILEBEGIN $@3 sqlvariantstate_list FILEEND
#line 406 "gix_esql_parser.yy"
        {
	// check host_variable
	driver.put_exec_list();
}
#line 1413 "gix_esql_parser.cc"
    break;

  case 76: // sqlvariantstates: PROCEDURE_DIVISION
#line 410 "gix_esql_parser.yy"
                    {
	driver.put_exec_list();
}
#line 1421 "gix_esql_parser.cc"
    break;

  case 83: // sqlvariantstate_list: sqlvariantstate_list HOSTVARIANTBEGIN
#line 422 "gix_esql_parser.yy"
                                       { driver.put_exec_list(); }
#line 1427 "gix_esql_parser.cc"
    break;

  case 84: // sqlvariantstate_list: sqlvariantstate_list HOSTVARIANTEND
#line 423 "gix_esql_parser.yy"
                                     { driver.put_exec_list(); }
#line 1433 "gix_esql_parser.cc"
    break;

  case 85: // declare_cursor: declare_special CURSOR FOR select END_EXEC
#line 427 "gix_esql_parser.yy"
                                                    { 
		yylhs.value.as < QList<QString> * > () = yystack_[1].value.as < QList<QString> * > ();
		driver.cb_set_cursorname(yystack_[4].value.as < QString > ()); 
		driver.cb_set_cursor_hold(0); 
		driver.put_startup_exec_list(); 
}
#line 1444 "gix_esql_parser.cc"
    break;

  case 86: // declare_cursor: declare_special CURSOR WITH_HOLD FOR select END_EXEC
#line 432 "gix_esql_parser.yy"
                                                         { 
		yylhs.value.as < QList<QString> * > () = yystack_[1].value.as < QList<QString> * > ();
		driver.cb_set_cursorname(yystack_[5].value.as < QString > ()); 
		driver.cb_set_cursor_hold(1); 
		driver.put_startup_exec_list(); 
}
#line 1455 "gix_esql_parser.cc"
    break;

  case 87: // declare_table: declare_special TABLE token_list END_EXEC
#line 441 "gix_esql_parser.yy"
                                                  { 
	driver.cb_set_commandname("DECLARE_TABLE");
	driver.put_exec_list(); 
}
#line 1464 "gix_esql_parser.cc"
    break;

  case 88: // declare_special: BEGIN_DECLARE_SPECIAL expr
#line 448 "gix_esql_parser.yy"
                                   { yylhs.value.as < QString > () = yystack_[0].value.as < QString > (); }
#line 1470 "gix_esql_parser.cc"
    break;

  case 89: // $@4: %empty
#line 451 "gix_esql_parser.yy"
             {
	cb_field_ptr x;

	x =  driver.cb_build_field_tree( yystack_[1].value.as < long > (), yystack_[0].value.as < QString > () , driver.current_field);
	if( x != NULL)
	{
		if( x->level != 78)
			driver.current_field = x;
	}
}
#line 1485 "gix_esql_parser.cc"
    break;

  case 90: // sqlvariantstate: NUMERIC WORD $@4 data_description_clause_sequence
#line 462 "gix_esql_parser.yy"
{
	if (driver.description_field == NULL)
		driver.description_field = driver.current_field;
}
#line 1494 "gix_esql_parser.cc"
    break;

  case 91: // $@5: %empty
#line 466 "gix_esql_parser.yy"
         {
	cb_field_ptr x;

	x =  driver.cb_build_field_tree( yystack_[0].value.as < long > (), "" , driver.current_field); // regist dummy name
	if( x != NULL){
	}
}
#line 1506 "gix_esql_parser.cc"
    break;

  case 92: // sqlvariantstate: NUMERIC $@5 data_description_clause_sequence
#line 474 "gix_esql_parser.yy"
{
	if (driver.description_field == NULL)
		driver.description_field = driver.current_field;
}
#line 1515 "gix_esql_parser.cc"
    break;

  case 93: // data_description_clause_sequence: %empty
#line 482 "gix_esql_parser.yy"
{}
#line 1521 "gix_esql_parser.cc"
    break;

  case 94: // data_description_clause_sequence: data_description_clause_sequence data_description_clause
#line 484 "gix_esql_parser.yy"
{}
#line 1527 "gix_esql_parser.cc"
    break;

  case 101: // picture_clause: PICTURE
#line 497 "gix_esql_parser.yy"
                {  driver.build_picture( yystack_[0].value.as < QString > (),driver.current_field);  }
#line 1533 "gix_esql_parser.cc"
    break;

  case 104: // usage: COMP
#line 506 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Binary;  }
#line 1539 "gix_esql_parser.cc"
    break;

  case 105: // usage: BINARY
#line 507 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Binary;  }
#line 1545 "gix_esql_parser.cc"
    break;

  case 106: // usage: COMP_1
#line 508 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Float;   }
#line 1551 "gix_esql_parser.cc"
    break;

  case 107: // usage: COMP_2
#line 509 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Double;  }
#line 1557 "gix_esql_parser.cc"
    break;

  case 108: // usage: COMP_3
#line 510 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Packed;  }
#line 1563 "gix_esql_parser.cc"
    break;

  case 109: // usage: WORD
#line 511 "gix_esql_parser.yy"
                    { driver.current_field->usage = Usage::None;    }
#line 1569 "gix_esql_parser.cc"
    break;

  case 110: // value_clause: VALUE _is_are _all const_clause
#line 514 "gix_esql_parser.yy"
                                              {}
#line 1575 "gix_esql_parser.cc"
    break;

  case 111: // const_clause: NUMERIC
#line 517 "gix_esql_parser.yy"
        {}
#line 1581 "gix_esql_parser.cc"
    break;

  case 112: // const_clause: WORD
#line 518 "gix_esql_parser.yy"
      { }
#line 1587 "gix_esql_parser.cc"
    break;

  case 113: // const_clause: CONST
#line 519 "gix_esql_parser.yy"
       {}
#line 1593 "gix_esql_parser.cc"
    break;

  case 114: // sign_clause: _sign_is LEADING flag_separate
#line 523 "gix_esql_parser.yy"
{
	driver.current_field->sign_leading = SIGNLEADING;
}
#line 1601 "gix_esql_parser.cc"
    break;

  case 115: // sign_clause: _sign_is TRAILING flag_separate
#line 527 "gix_esql_parser.yy"
{

}
#line 1609 "gix_esql_parser.cc"
    break;

  case 116: // _sign_is: SIGN
#line 532 "gix_esql_parser.yy"
                       {}
#line 1615 "gix_esql_parser.cc"
    break;

  case 117: // _sign_is: SIGN IS
#line 533 "gix_esql_parser.yy"
          {}
#line 1621 "gix_esql_parser.cc"
    break;

  case 119: // flag_separate: SEPARATE
#line 536 "gix_esql_parser.yy"
           { driver.current_field->separate = SIGN_SEPARATE; }
#line 1627 "gix_esql_parser.cc"
    break;

  case 120: // occurs_clause: OCCURS NUMERIC _times
#line 541 "gix_esql_parser.yy"
{
	driver.current_field->occurs = (int)yystack_[1].value.as < long > ();
}
#line 1635 "gix_esql_parser.cc"
    break;

  case 121: // external_clause: _is EXTERNAL
#line 547 "gix_esql_parser.yy"
             {}
#line 1641 "gix_esql_parser.cc"
    break;


#line 1645 "gix_esql_parser.cc"

            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error (yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, YY_MOVE (yylhs));
    }
    goto yynewstate;


  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        context yyctx (*this, yyla);
        std::string msg = yysyntax_error_ (yyctx);
        error (yyla.location, YY_MOVE (msg));
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.kind () == symbol_kind::S_YYEOF)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:
    /* Pacify compilers when the user code never invokes YYERROR and
       the label yyerrorlab therefore never appears in user code.  */
    if (false)
      YYERROR;

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    YY_STACK_PRINT ();
    goto yyerrlab1;


  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    // Pop stack until we find a state that shifts the error token.
    for (;;)
      {
        yyn = yypact_[+yystack_[0].state];
        if (!yy_pact_value_is_default_ (yyn))
          {
            yyn += symbol_kind::S_YYerror;
            if (0 <= yyn && yyn <= yylast_
                && yycheck_[yyn] == symbol_kind::S_YYerror)
              {
                yyn = yytable_[yyn];
                if (0 < yyn)
                  break;
              }
          }

        // Pop the current state because it cannot handle the error token.
        if (yystack_.size () == 1)
          YYABORT;

        yyerror_range[1].location = yystack_[0].location;
        yy_destroy_ ("Error: popping", yystack_[0]);
        yypop_ ();
        YY_STACK_PRINT ();
      }
    {
      stack_symbol_type error_token;

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = state_type (yyn);
      yypush_ ("Shifting", YY_MOVE (error_token));
    }
    goto yynewstate;


  /*-------------------------------------.
  | yyacceptlab -- YYACCEPT comes here.  |
  `-------------------------------------*/
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;


  /*-----------------------------------.
  | yyabortlab -- YYABORT comes here.  |
  `-----------------------------------*/
  yyabortlab:
    yyresult = 1;
    goto yyreturn;


  /*-----------------------------------------------------.
  | yyreturn -- parsing is finished, return the result.  |
  `-----------------------------------------------------*/
  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    YY_STACK_PRINT ();
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
#if YY_EXCEPTIONS
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
#endif // YY_EXCEPTIONS
  }

  void
  gix_esql_parser::error (const syntax_error& yyexc)
  {
    error (yyexc.location, yyexc.what ());
  }

  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
  gix_esql_parser::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr;
        char const *yyp = yystr;

        for (;;)
          switch (*++yyp)
            {
            case '\'':
            case ',':
              goto do_not_strip_quotes;

            case '\\':
              if (*++yyp != '\\')
                goto do_not_strip_quotes;
              else
                goto append;

            append:
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }

  std::string
  gix_esql_parser::symbol_name (symbol_kind_type yysymbol)
  {
    return yytnamerr_ (yytname_[yysymbol]);
  }



  // gix_esql_parser::context.
  gix_esql_parser::context::context (const gix_esql_parser& yyparser, const symbol_type& yyla)
    : yyparser_ (yyparser)
    , yyla_ (yyla)
  {}

  int
  gix_esql_parser::context::expected_tokens (symbol_kind_type yyarg[], int yyargn) const
  {
    // Actual number of expected tokens
    int yycount = 0;

    int yyn = yypact_[+yyparser_.yystack_[0].state];
    if (!yy_pact_value_is_default_ (yyn))
      {
        /* Start YYX at -YYN if negative to avoid negative indexes in
           YYCHECK.  In other words, skip the first -YYN actions for
           this state because they are default actions.  */
        int yyxbegin = yyn < 0 ? -yyn : 0;
        // Stay within bounds of both yycheck and yytname.
        int yychecklim = yylast_ - yyn + 1;
        int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
        for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
          if (yycheck_[yyx + yyn] == yyx && yyx != symbol_kind::S_YYerror
              && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
            {
              if (!yyarg)
                ++yycount;
              else if (yycount == yyargn)
                return 0;
              else
                yyarg[yycount++] = YY_CAST (symbol_kind_type, yyx);
            }
      }

    if (yyarg && yycount == 0 && 0 < yyargn)
      yyarg[0] = symbol_kind::S_YYEMPTY;
    return yycount;
  }



  int
  gix_esql_parser::yy_syntax_error_arguments_ (const context& yyctx,
                                                 symbol_kind_type yyarg[], int yyargn) const
  {
    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state merging
         (from LALR or IELR) and default reductions corrupt the expected
         token list.  However, the list is correct for canonical LR with
         one exception: it will still contain any token that will not be
         accepted due to an error action in a later state.
    */

    if (!yyctx.lookahead ().empty ())
      {
        if (yyarg)
          yyarg[0] = yyctx.token ();
        int yyn = yyctx.expected_tokens (yyarg ? yyarg + 1 : yyarg, yyargn - 1);
        return yyn + 1;
      }
    return 0;
  }

  // Generate an error message.
  std::string
  gix_esql_parser::yysyntax_error_ (const context& yyctx) const
  {
    // Its maximum.
    enum { YYARGS_MAX = 5 };
    // Arguments of yyformat.
    symbol_kind_type yyarg[YYARGS_MAX];
    int yycount = yy_syntax_error_arguments_ (yyctx, yyarg, YYARGS_MAX);

    char const* yyformat = YY_NULLPTR;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
      default: // Avoid compiler warnings.
        YYCASE_ (0, YY_("syntax error"));
        YYCASE_ (1, YY_("syntax error, unexpected %s"));
        YYCASE_ (2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_ (3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_ (4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_ (5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    std::string yyres;
    // Argument number.
    std::ptrdiff_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += symbol_name (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const short gix_esql_parser::yypact_ninf_ = -149;

  const signed char gix_esql_parser::yytable_ninf_ = -123;

  const short
  gix_esql_parser::yypact_[] =
  {
    -149,     2,  -149,    73,   -16,  -149,  -149,  -149,  -149,  -149,
    -149,  -149,  -149,  -149,  -149,  -149,  -149,  -149,  -149,  -149,
    -149,  -149,  -149,  -149,  -149,  -149,  -149,   221,  -149,  -149,
    -149,    21,  -149,   221,   221,    31,    41,    36,   221,   221,
     221,    59,   221,   221,   221,   209,    52,    69,   -12,    74,
      80,  -149,  -149,  -149,  -149,  -149,  -149,  -149,   155,  -149,
    -149,   163,    76,  -149,  -149,  -149,  -149,    14,    85,  -149,
      40,    10,   172,   183,  -149,   192,    36,  -149,  -149,    36,
      64,   221,   102,  -149,    34,    24,    77,    36,  -149,  -149,
    -149,   -17,  -149,    36,  -149,    94,  -149,   221,  -149,  -149,
    -149,   120,  -149,  -149,    36,   106,   229,  -149,   133,    11,
    -149,  -149,  -149,   221,  -149,  -149,  -149,  -149,    -4,   125,
    -149,  -149,   117,  -149,   119,   141,  -149,  -149,   130,  -149,
    -149,  -149,  -149,  -149,  -149,  -149,   131,  -149,    -7,   221,
    -149,   221,  -149,  -149,  -149,  -149,  -149,  -149,  -149,    82,
    -149,    74,   126,   203,   212,    82,  -149,  -149,  -149,  -149,
    -149,  -149,  -149,    96,    97,  -149,    18,   145,  -149,  -149,
    -149,  -149,  -149,  -149,   -10,  -149,  -149,   103,   146,    74,
    -149,  -149,   107,  -149,  -149,  -149,   109,   111,   116,   116,
    -149,  -149,   156,  -149,  -149,     7,  -149,  -149,  -149,  -149,
    -149,  -149,  -149,  -149,  -149,  -149
  };

  const unsigned char
  gix_esql_parser::yydefact_[] =
  {
       2,     0,     1,     0,     0,    70,    72,    74,    76,    54,
       3,    16,    17,    15,    14,    12,    11,    10,     9,     8,
       7,    18,    19,     6,    13,    20,     5,     0,    29,    23,
      25,     0,    27,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,    77,    77,    67,    66,    69,    68,     0,    62,
      48,     0,     0,    32,    31,    65,    50,     0,     0,    34,
       0,     0,     0,     0,    30,     0,     0,    46,    45,     0,
       0,     0,     0,    53,     0,     0,     0,     0,    64,    63,
      49,     0,    42,     0,    41,     0,    21,     0,    24,    26,
      28,     0,    38,    51,     0,     0,    59,    58,    91,     0,
      71,    83,    84,     0,    78,    79,    80,    81,     0,     0,
      73,    75,     0,    60,     0,     0,    35,    47,     0,    39,
      33,    40,    52,    44,    89,    93,     0,    88,     0,     0,
      82,     0,    57,    61,    36,    43,    37,    22,    93,    92,
      55,     0,     0,     0,     0,    90,   109,   101,   106,   107,
     108,   104,   105,   122,   116,   123,   124,     0,    94,    95,
      96,   102,    99,    97,     0,    98,   100,     0,     0,     0,
      87,    56,     0,   117,   125,   126,   127,   129,   118,   118,
     121,    85,     0,   103,   128,     0,   130,   120,   119,   115,
     114,    86,   112,   111,   113,   110
  };

  const short
  gix_esql_parser::yypgoto_[] =
  {
    -149,  -149,  -149,  -149,  -149,  -149,  -149,  -149,  -149,  -149,
    -149,  -149,  -149,  -149,  -149,  -149,    90,  -149,  -149,  -149,
    -149,  -149,  -149,  -149,  -149,  -149,   180,  -149,  -149,  -149,
    -148,  -149,   -32,   -28,   -34,  -149,  -149,  -149,  -149,    28,
    -149,  -149,  -149,  -149,  -149,  -149,    37,  -149,  -149,  -149,
       4,  -149,  -149,  -149,  -149,    -5,  -149,  -149,    29,  -149,
    -149,  -149
  };

  const short
  gix_esql_parser::yydefgoto_[] =
  {
      -1,     1,    10,    11,    42,    12,    43,    13,    44,    14,
      45,    15,    16,    17,    46,   125,   101,    18,    19,    20,
      47,    21,    22,    48,    80,   105,   114,   115,    24,    25,
      82,    49,    58,    88,    59,    26,    51,    52,    53,    84,
     116,   117,   118,   119,   148,   135,   149,   168,   169,   170,
     171,   172,   205,   173,   174,   199,   175,   176,   177,   186,
     195,   197
  };

  const short
  gix_esql_parser::yytable_[] =
  {
      62,    61,     2,   178,    67,    68,    69,    78,   123,    66,
      71,    72,    73,    75,    54,   202,    55,    65,   151,    50,
     138,    56,     3,     4,    89,   203,    79,    89,     5,    96,
       6,   192,     7,    92,     8,    57,   188,    89,    89,    89,
      60,    89,   108,    65,   189,   109,    50,   136,   102,   106,
      63,   103,   108,   120,   124,   109,    93,   111,   112,   102,
      64,   110,   139,   128,   152,   126,    70,   111,   112,    76,
     204,     9,    89,   131,   184,   185,   132,    27,    81,   137,
      85,    86,    97,    28,    29,    30,    31,    32,    77,    33,
     156,   157,   113,     9,   131,   108,    34,   146,   109,    83,
      91,   127,   113,     9,    94,    95,   104,   153,   121,   154,
     111,   112,    35,    36,    37,   156,    38,    39,    40,    89,
      89,   107,   141,   129,    65,   133,   129,    65,   140,   158,
     159,   160,   161,   162,   163,   164,   142,    41,   165,   130,
     166,   134,   167,  -122,   143,   113,     9,   144,    65,   147,
     150,   179,   165,   183,   158,   159,   160,   161,   162,    54,
     145,    55,    65,   187,   190,   191,    56,    54,   194,    55,
      65,   198,    87,   196,    56,   201,    54,   122,    55,    65,
      57,    23,    90,    56,   200,   155,   193,    54,    57,    55,
      65,    98,   182,     0,    56,     0,    54,    57,    55,    65,
       0,     0,    99,    56,     0,     0,     0,    54,    57,    55,
      65,   100,     0,    54,    56,    55,    54,    57,    55,    65,
      56,     0,   180,    56,     0,    54,    74,    55,    57,     0,
       0,   181,    56,    54,    57,    55,    65,    57,     0,     0,
      56,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,    57
  };

  const short
  gix_esql_parser::yycheck_[] =
  {
      34,    33,     0,   151,    38,    39,    40,    19,    25,    37,
      42,    43,    44,    45,     4,     8,     6,     7,    25,    35,
      24,    11,    20,    21,    58,    18,    38,    61,    26,    19,
      28,   179,    30,    19,    32,    25,    46,    71,    72,    73,
      19,    75,    18,     7,    54,    21,    35,    36,    76,    81,
      19,    79,    18,    29,    71,    21,    42,    33,    34,    87,
      19,    27,    66,    97,    71,    93,     7,    33,    34,    17,
      63,    69,   106,   101,    56,    57,   104,     4,     4,   113,
      52,    53,    72,    10,    11,    12,    13,    14,    19,    16,
       8,     9,    68,    69,   122,    18,    23,   125,    21,    19,
      24,     7,    68,    69,    19,    65,    42,   139,    31,   141,
      33,    34,    39,    40,    41,     8,    43,    44,    45,   153,
     154,    19,     5,     6,     7,    19,     6,     7,     3,    47,
      48,    49,    50,    51,    52,    53,    19,    64,    56,    19,
      58,     8,    60,    61,    25,    68,    69,     6,     7,    19,
      19,    25,    56,    56,    47,    48,    49,    50,    51,     4,
      19,     6,     7,    18,    61,    19,    11,     4,    59,     6,
       7,    55,    17,    62,    11,    19,     4,    87,     6,     7,
      25,     1,    19,    11,   189,   148,   182,     4,    25,     6,
       7,    19,   163,    -1,    11,    -1,     4,    25,     6,     7,
      -1,    -1,    19,    11,    -1,    -1,    -1,     4,    25,     6,
       7,    19,    -1,     4,    11,     6,     4,    25,     6,     7,
      11,    -1,    19,    11,    -1,     4,    17,     6,    25,    -1,
      -1,    19,    11,     4,    25,     6,     7,    25,    -1,    -1,
      11,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    25
  };

  const unsigned char
  gix_esql_parser::yystos_[] =
  {
       0,    74,     0,    20,    21,    26,    28,    30,    32,    69,
      75,    76,    78,    80,    82,    84,    85,    86,    90,    91,
      92,    94,    95,    99,   101,   102,   108,     4,    10,    11,
      12,    13,    14,    16,    23,    39,    40,    41,    43,    44,
      45,    64,    77,    79,    81,    83,    87,    93,    96,   104,
      35,   109,   110,   111,     4,     6,    11,    25,   105,   107,
      19,   105,   107,    19,    19,     7,   106,   107,   107,   107,
       7,   105,   105,   105,    17,   105,    17,    19,    19,    38,
      97,     4,   103,    19,   112,   112,   112,    17,   106,   107,
      19,    24,    19,    42,    19,    65,    19,    72,    19,    19,
      19,    89,   106,   106,    42,    98,   105,    19,    18,    21,
      27,    33,    34,    68,    99,   100,   113,   114,   115,   116,
      29,    31,    89,    25,    71,    88,   106,     7,   107,     6,
      19,   106,   106,    19,     8,   118,    36,   107,    24,    66,
       3,     5,    19,    25,     6,    19,   106,    19,   117,   119,
      19,    25,    71,   105,   105,   119,     8,     9,    47,    48,
      49,    50,    51,    52,    53,    56,    58,    60,   120,   121,
     122,   123,   124,   126,   127,   129,   130,   131,   103,    25,
      19,    19,   131,    56,    56,    57,   132,    18,    46,    54,
      61,    19,   103,   123,    59,   133,    62,   134,    55,   128,
     128,    19,     8,    18,    63,   125
  };

  const unsigned char
  gix_esql_parser::yyr1_[] =
  {
       0,    73,    74,    74,    75,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    76,    76,    77,    78,    79,    80,    81,    82,    83,
      83,    84,    85,    86,    87,    88,    88,    88,    89,    89,
      89,    90,    91,    91,    92,    92,    92,    93,    94,    95,
      96,    97,    98,    99,    99,   100,   101,   101,   102,   103,
     104,   104,   105,   105,   105,   106,   107,   107,   107,   107,
     109,   108,   110,   108,   111,   108,   108,   112,   112,   112,
     112,   112,   112,   112,   112,   113,   113,   114,   115,   117,
     116,   118,   116,   119,   119,   120,   120,   120,   120,   120,
     120,   121,   122,   122,   123,   123,   123,   123,   123,   123,
     124,   125,   125,   125,   126,   126,   127,   127,   128,   128,
     129,   130,   131,   131,   132,   132,   132,   133,   133,   134,
     134
  };

  const signed char
  gix_esql_parser::yyr2_[] =
  {
       0,     2,     0,     2,     0,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     4,     6,     1,     4,     1,     4,     1,     4,     1,
       2,     3,     3,     5,     2,     1,     2,     2,     1,     2,
       2,     4,     4,     6,     5,     3,     3,     4,     3,     4,
       2,     2,     2,     3,     1,     3,     8,     6,     4,     2,
       4,     5,     1,     2,     2,     1,     1,     1,     1,     1,
       0,     4,     0,     4,     0,     4,     1,     0,     2,     2,
       2,     2,     3,     2,     2,     5,     6,     4,     2,     0,
       4,     0,     3,     0,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     1,     1,     1,     1,     1,
       4,     1,     1,     1,     3,     3,     1,     2,     0,     1,
       3,     2,     0,     1,     0,     1,     1,     0,     1,     0,
       1
  };


#if YYDEBUG || 1
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char*
  const gix_esql_parser::yytname_[] =
  {
  "\"end of file\"", "error", "\"invalid token\"", "\".\"", "SELECT",
  "SELECTFROM", "TOKEN", "HOSTTOKEN", "WORD", "PICTURE", "INSERT",
  "UPDATE", "DISCONNECT", "CONNECT_RESET", "DELETE", "EXECUTE",
  "OTHERFUNC", "INTO", "NUMERIC", "END_EXEC", "EXECSQL", "EXECSQL_INCLUDE",
  "FROM", "DECLARE", "CURSOR", "FOR", "WORKINGBEGIN", "WORKINGEND",
  "LINKAGEBEGIN", "LINKAGEEND", "FILEBEGIN", "FILEEND",
  "PROCEDURE_DIVISION", "HOSTVARIANTBEGIN", "HOSTVARIANTEND",
  "INCLUDE_FILE", "INCLUDE_SQLCA", "SQLCA", "IDENTIFIED_BY", "COMMIT_WORK",
  "ROLLBACK_WORK", "CONNECT", "USING", "OPEN", "CLOSE", "FETCH",
  "TRAILING", "COMP_1", "COMP_2", "COMP_3", "COMP", "BINARY", "USAGE",
  "SIGN", "LEADING", "SEPARATE", "IS", "ARE", "VALUE", "ALL", "OCCURS",
  "EXTERNAL", "TIMES", "CONST", "CONNECT_TO", "USER", "TABLE", "TO",
  "BEGIN_DECLARE_SPECIAL", "COPY", "COPY_FILE", "WITH_HOLD",
  "WHERE_CURRENT_OF", "$accept", "sqlstate_list", "sqlstate", "updatesql",
  "update", "disconnectsql", "disconnect", "deletesql", "delete",
  "insertsql", "insert", "rollbacksql", "commitsql", "fetchsql", "fetch",
  "host_references", "res_host_references", "closesql", "opensql",
  "connectsql", "connect_to", "resetsql", "othersql", "connect",
  "identified", "using", "incfile", "includesql", "selectintosql",
  "declaresql", "select", "declare_for", "token_list", "host_reference",
  "expr", "sqlvariantstates", "$@1", "$@2", "$@3", "sqlvariantstate_list",
  "declare_cursor", "declare_table", "declare_special", "sqlvariantstate",
  "$@4", "$@5", "data_description_clause_sequence",
  "data_description_clause", "picture_clause", "usage_clause", "usage",
  "value_clause", "const_clause", "sign_clause", "_sign_is",
  "flag_separate", "occurs_clause", "external_clause", "_is", "_is_are",
  "_all", "_times", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  gix_esql_parser::yyrline_[] =
  {
       0,   165,   165,   166,   168,   169,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,   183,
     184,   188,   193,   203,   207,   214,   217,   225,   228,   235,
     236,   241,   246,   252,   256,   259,   260,   261,   264,   265,
     266,   269,   275,   279,   285,   286,   287,   290,   298,   303,
     309,   315,   320,   326,   330,   337,   342,   348,   355,   358,
     361,   362,   365,   366,   367,   372,   374,   375,   376,   377,
     380,   380,   390,   390,   400,   400,   410,   416,   417,   418,
     419,   420,   421,   422,   423,   427,   432,   441,   448,   451,
     451,   466,   466,   482,   483,   488,   489,   490,   491,   492,
     493,   497,   501,   502,   506,   507,   508,   509,   510,   511,
     514,   517,   518,   519,   522,   526,   532,   533,   535,   536,
     540,   547,   550,   550,   551,   551,   551,   552,   552,   553,
     553
  };

  void
  gix_esql_parser::yy_stack_print_ () const
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << int (i->state);
    *yycdebug_ << '\n';
  }

  void
  gix_esql_parser::yy_reduce_print_ (int yyrule) const
  {
    int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG


} // yy
#line 2287 "gix_esql_parser.cc"

#line 556 "gix_esql_parser.yy"


// Register errors to the driver:
void yy::gix_esql_parser::error (const location_type& l, const std::string& m)
{
    driver.error(l, m);
}
