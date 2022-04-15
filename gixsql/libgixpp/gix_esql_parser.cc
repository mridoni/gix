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

static std::string to_std_string(std::string *sp) { return (sp != NULL) ? *sp : "(NULL)"; }
static std::string to_std_string(const std::string s) { return s; }
static std::string to_std_string(const std::vector<std::string> *slp) { if (!slp) return "(NULL-LIST)"; int n = slp->size() > 3 ? 3 : slp->size(); std::string res; for (int i = 0; i < n; i++) res += slp->at(i); return (res + " ..."); }
static std::string to_std_string(const int i) { char buffer [33]; sprintf(buffer, "%d", i); char *res = (char*) malloc(strlen(buffer) + 1); strcpy (res, buffer); return res; }
static std::string to_std_string(hostref_or_literal_t *hl) { return hl->name; }
static std::string to_std_string(connect_to_info_t *i) { char buffer [33]; sprintf(buffer, "%d", i->type); char *res = (char*) malloc(strlen(buffer) + 1); strcpy (res, buffer); return res; }


#line 59 "gix_esql_parser.cc"


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
#line 151 "gix_esql_parser.cc"

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
      case symbol_kind::S_opt_auth_info: // opt_auth_info
      case symbol_kind::S_opt_identified_by: // opt_identified_by
        value.YY_MOVE_OR_COPY< connect_to_info_t * > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_dbid: // dbid
      case symbol_kind::S_opt_using: // opt_using
      case symbol_kind::S_opt_connect_as: // opt_connect_as
      case symbol_kind::S_opt_at: // opt_at
      case symbol_kind::S_opt_dbid: // opt_dbid
      case symbol_kind::S_strliteral_or_hostref: // strliteral_or_hostref
        value.YY_MOVE_OR_COPY< hostref_or_literal_t * > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
      case symbol_kind::S_opt_with_hold: // opt_with_hold
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.YY_MOVE_OR_COPY< long > (YY_MOVE (that.value));
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
      case symbol_kind::S_othersql_token: // othersql_token
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
        value.YY_MOVE_OR_COPY< std::string > (YY_MOVE (that.value));
        break;

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
      case symbol_kind::S_opt_othersql_tokens: // opt_othersql_tokens
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_cursor_declaration: // cursor_declaration
      case symbol_kind::S_executesql: // executesql
      case symbol_kind::S_ignoresql: // ignoresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
        value.YY_MOVE_OR_COPY< std::vector<std::string> * > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_CHAR: // CHAR
      case symbol_kind::S_VARCHAR: // VARCHAR
      case symbol_kind::S_BINARY: // BINARY
      case symbol_kind::S_VARBINARY: // VARBINARY
      case symbol_kind::S_FLOAT: // FLOAT
      case symbol_kind::S_INTEGER: // INTEGER
      case symbol_kind::S_DECIMAL: // DECIMAL
      case symbol_kind::S_opt_sql_type_def: // opt_sql_type_def
      case symbol_kind::S_sql_type: // sql_type
        value.YY_MOVE_OR_COPY< uint64_t > (YY_MOVE (that.value));
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
      case symbol_kind::S_opt_auth_info: // opt_auth_info
      case symbol_kind::S_opt_identified_by: // opt_identified_by
        value.move< connect_to_info_t * > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_dbid: // dbid
      case symbol_kind::S_opt_using: // opt_using
      case symbol_kind::S_opt_connect_as: // opt_connect_as
      case symbol_kind::S_opt_at: // opt_at
      case symbol_kind::S_opt_dbid: // opt_dbid
      case symbol_kind::S_strliteral_or_hostref: // strliteral_or_hostref
        value.move< hostref_or_literal_t * > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
      case symbol_kind::S_opt_with_hold: // opt_with_hold
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.move< long > (YY_MOVE (that.value));
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
      case symbol_kind::S_othersql_token: // othersql_token
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
        value.move< std::string > (YY_MOVE (that.value));
        break;

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
      case symbol_kind::S_opt_othersql_tokens: // opt_othersql_tokens
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_cursor_declaration: // cursor_declaration
      case symbol_kind::S_executesql: // executesql
      case symbol_kind::S_ignoresql: // ignoresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
        value.move< std::vector<std::string> * > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_CHAR: // CHAR
      case symbol_kind::S_VARCHAR: // VARCHAR
      case symbol_kind::S_BINARY: // BINARY
      case symbol_kind::S_VARBINARY: // VARBINARY
      case symbol_kind::S_FLOAT: // FLOAT
      case symbol_kind::S_INTEGER: // INTEGER
      case symbol_kind::S_DECIMAL: // DECIMAL
      case symbol_kind::S_opt_sql_type_def: // opt_sql_type_def
      case symbol_kind::S_sql_type: // sql_type
        value.move< uint64_t > (YY_MOVE (that.value));
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
      case symbol_kind::S_opt_auth_info: // opt_auth_info
      case symbol_kind::S_opt_identified_by: // opt_identified_by
        value.copy< connect_to_info_t * > (that.value);
        break;

      case symbol_kind::S_dbid: // dbid
      case symbol_kind::S_opt_using: // opt_using
      case symbol_kind::S_opt_connect_as: // opt_connect_as
      case symbol_kind::S_opt_at: // opt_at
      case symbol_kind::S_opt_dbid: // opt_dbid
      case symbol_kind::S_strliteral_or_hostref: // strliteral_or_hostref
        value.copy< hostref_or_literal_t * > (that.value);
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
      case symbol_kind::S_opt_with_hold: // opt_with_hold
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.copy< long > (that.value);
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
      case symbol_kind::S_othersql_token: // othersql_token
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
        value.copy< std::string > (that.value);
        break;

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
      case symbol_kind::S_opt_othersql_tokens: // opt_othersql_tokens
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_cursor_declaration: // cursor_declaration
      case symbol_kind::S_executesql: // executesql
      case symbol_kind::S_ignoresql: // ignoresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
        value.copy< std::vector<std::string> * > (that.value);
        break;

      case symbol_kind::S_CHAR: // CHAR
      case symbol_kind::S_VARCHAR: // VARCHAR
      case symbol_kind::S_BINARY: // BINARY
      case symbol_kind::S_VARBINARY: // VARBINARY
      case symbol_kind::S_FLOAT: // FLOAT
      case symbol_kind::S_INTEGER: // INTEGER
      case symbol_kind::S_DECIMAL: // DECIMAL
      case symbol_kind::S_opt_sql_type_def: // opt_sql_type_def
      case symbol_kind::S_sql_type: // sql_type
        value.copy< uint64_t > (that.value);
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
      case symbol_kind::S_opt_auth_info: // opt_auth_info
      case symbol_kind::S_opt_identified_by: // opt_identified_by
        value.move< connect_to_info_t * > (that.value);
        break;

      case symbol_kind::S_dbid: // dbid
      case symbol_kind::S_opt_using: // opt_using
      case symbol_kind::S_opt_connect_as: // opt_connect_as
      case symbol_kind::S_opt_at: // opt_at
      case symbol_kind::S_opt_dbid: // opt_dbid
      case symbol_kind::S_strliteral_or_hostref: // strliteral_or_hostref
        value.move< hostref_or_literal_t * > (that.value);
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
      case symbol_kind::S_opt_with_hold: // opt_with_hold
        value.move< int > (that.value);
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.move< long > (that.value);
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
      case symbol_kind::S_othersql_token: // othersql_token
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
        value.move< std::string > (that.value);
        break;

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
      case symbol_kind::S_opt_othersql_tokens: // opt_othersql_tokens
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_cursor_declaration: // cursor_declaration
      case symbol_kind::S_executesql: // executesql
      case symbol_kind::S_ignoresql: // ignoresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
        value.move< std::vector<std::string> * > (that.value);
        break;

      case symbol_kind::S_CHAR: // CHAR
      case symbol_kind::S_VARCHAR: // VARCHAR
      case symbol_kind::S_BINARY: // BINARY
      case symbol_kind::S_VARBINARY: // VARBINARY
      case symbol_kind::S_FLOAT: // FLOAT
      case symbol_kind::S_INTEGER: // INTEGER
      case symbol_kind::S_DECIMAL: // DECIMAL
      case symbol_kind::S_opt_sql_type_def: // opt_sql_type_def
      case symbol_kind::S_sql_type: // sql_type
        value.move< uint64_t > (that.value);
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
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 610 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_SELECTFROM: // SELECTFROM
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 616 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_TOKEN: // TOKEN
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 622 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_HOSTTOKEN: // HOSTTOKEN
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 628 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_WORD: // WORD
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 634 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_PICTURE: // PICTURE
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 640 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_INSERT: // INSERT
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 646 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_UPDATE: // UPDATE
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 652 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_DISCONNECT: // DISCONNECT
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 658 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_DELETE: // DELETE
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 664 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_EXECUTE: // EXECUTE
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 670 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_OTHERFUNC: // OTHERFUNC
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 676 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_INTO: // INTO
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 682 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < long > ()); }
#line 688 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_CHAR: // CHAR
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < uint64_t > ()); }
#line 694 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_VARCHAR: // VARCHAR
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < uint64_t > ()); }
#line 700 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_BINARY: // BINARY
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < uint64_t > ()); }
#line 706 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_VARBINARY: // VARBINARY
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < uint64_t > ()); }
#line 712 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_FLOAT: // FLOAT
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < uint64_t > ()); }
#line 718 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_INTEGER: // INTEGER
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < uint64_t > ()); }
#line 724 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_DECIMAL: // DECIMAL
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < uint64_t > ()); }
#line 730 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < int > ()); }
#line 736 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_updatesql: // updatesql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 742 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_update: // update
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 748 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_disconnectsql: // disconnectsql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 754 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_disconnect: // disconnect
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 760 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_deletesql: // deletesql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 766 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_delete: // delete
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 772 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_insertsql: // insertsql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 778 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_insert: // insert
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 784 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opensql: // opensql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 790 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opt_auth_info: // opt_auth_info
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < connect_to_info_t * > ()); }
#line 796 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opt_identified_by: // opt_identified_by
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < connect_to_info_t * > ()); }
#line 802 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_dbid: // dbid
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < hostref_or_literal_t * > ()); }
#line 808 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opt_using: // opt_using
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < hostref_or_literal_t * > ()); }
#line 814 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opt_connect_as: // opt_connect_as
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < hostref_or_literal_t * > ()); }
#line 820 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opt_at: // opt_at
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < hostref_or_literal_t * > ()); }
#line 826 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opt_dbid: // opt_dbid
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < hostref_or_literal_t * > ()); }
#line 832 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_strliteral_or_hostref: // strliteral_or_hostref
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < hostref_or_literal_t * > ()); }
#line 838 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_othersql: // othersql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 844 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opt_othersql_tokens: // opt_othersql_tokens
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 850 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_othersql_token: // othersql_token
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 856 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_incfile: // incfile
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 862 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_includesql: // includesql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 868 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_selectintosql: // selectintosql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 874 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_declaresql: // declaresql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 880 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_cursor_declaration: // cursor_declaration
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 886 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opt_with_hold: // opt_with_hold
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < int > ()); }
#line 892 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_executesql: // executesql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 898 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_ignoresql: // ignoresql
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 904 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_select: // select
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 910 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_token_list: // token_list
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::vector<std::string> * > ()); }
#line 916 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_host_reference: // host_reference
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 922 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_expr: // expr
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < std::string > ()); }
#line 928 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_opt_sql_type_def: // opt_sql_type_def
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < uint64_t > ()); }
#line 934 "gix_esql_parser.cc"
        break;

      case symbol_kind::S_sql_type: // sql_type
#line 191 "gix_esql_parser.yy"
                 { yyoutput << to_std_string(yysym.value.template as < uint64_t > ()); }
#line 940 "gix_esql_parser.cc"
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

#line 1065 "gix_esql_parser.cc"


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
      case symbol_kind::S_opt_auth_info: // opt_auth_info
      case symbol_kind::S_opt_identified_by: // opt_identified_by
        yylhs.value.emplace< connect_to_info_t * > ();
        break;

      case symbol_kind::S_dbid: // dbid
      case symbol_kind::S_opt_using: // opt_using
      case symbol_kind::S_opt_connect_as: // opt_connect_as
      case symbol_kind::S_opt_at: // opt_at
      case symbol_kind::S_opt_dbid: // opt_dbid
      case symbol_kind::S_strliteral_or_hostref: // strliteral_or_hostref
        yylhs.value.emplace< hostref_or_literal_t * > ();
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
      case symbol_kind::S_opt_with_hold: // opt_with_hold
        yylhs.value.emplace< int > ();
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        yylhs.value.emplace< long > ();
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
      case symbol_kind::S_othersql_token: // othersql_token
      case symbol_kind::S_host_reference: // host_reference
      case symbol_kind::S_expr: // expr
        yylhs.value.emplace< std::string > ();
        break;

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
      case symbol_kind::S_opt_othersql_tokens: // opt_othersql_tokens
      case symbol_kind::S_incfile: // incfile
      case symbol_kind::S_includesql: // includesql
      case symbol_kind::S_selectintosql: // selectintosql
      case symbol_kind::S_declaresql: // declaresql
      case symbol_kind::S_cursor_declaration: // cursor_declaration
      case symbol_kind::S_executesql: // executesql
      case symbol_kind::S_ignoresql: // ignoresql
      case symbol_kind::S_select: // select
      case symbol_kind::S_token_list: // token_list
        yylhs.value.emplace< std::vector<std::string> * > ();
        break;

      case symbol_kind::S_CHAR: // CHAR
      case symbol_kind::S_VARCHAR: // VARCHAR
      case symbol_kind::S_BINARY: // BINARY
      case symbol_kind::S_VARBINARY: // VARBINARY
      case symbol_kind::S_FLOAT: // FLOAT
      case symbol_kind::S_INTEGER: // INTEGER
      case symbol_kind::S_DECIMAL: // DECIMAL
      case symbol_kind::S_opt_sql_type_def: // opt_sql_type_def
      case symbol_kind::S_sql_type: // sql_type
        yylhs.value.emplace< uint64_t > ();
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
  case 24: // execsql_with_opt_at: EXECSQL opt_at
#line 222 "gix_esql_parser.yy"
                                    {
	driver.connectionid = yystack_[0].value.as < hostref_or_literal_t * > ();
}
#line 1282 "gix_esql_parser.cc"
    break;

  case 25: // updatesql: execsql_with_opt_at update token_list END_EXEC
#line 228 "gix_esql_parser.yy"
{
	yylhs.value.as < std::vector<std::string> * > () = driver.cb_concat_text_list (yystack_[2].value.as < std::vector<std::string> * > (), yystack_[1].value.as < std::vector<std::string> * > ());
	driver.put_exec_list();
}
#line 1291 "gix_esql_parser.cc"
    break;

  case 26: // updatesql: execsql_with_opt_at update token_list WHERE_CURRENT_OF expr END_EXEC
#line 233 "gix_esql_parser.yy"
{
	driver.cb_set_cursorname(yystack_[1].value.as < std::string > ());
	yylhs.value.as < std::vector<std::string> * > () = driver.cb_concat_text_list (yystack_[4].value.as < std::vector<std::string> * > (), yystack_[3].value.as < std::vector<std::string> * > ());
	driver.cb_concat_text_list(yylhs.value.as < std::vector<std::string> * > (), driver.cb_text_list_add(NULL, "WHERE CURRENT OF"));
	driver.cb_concat_text_list(yylhs.value.as < std::vector<std::string> * > (), driver.cb_text_list_add(NULL, driver.cursorname));
	driver.put_exec_list();
}
#line 1303 "gix_esql_parser.cc"
    break;

  case 27: // update: UPDATE
#line 242 "gix_esql_parser.yy"
       {yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < std::string > ());}
#line 1309 "gix_esql_parser.cc"
    break;

  case 28: // disconnectsql: EXECSQL disconnect opt_dbid END_EXEC
#line 247 "gix_esql_parser.yy"
{
	//$$ = driver.cb_concat_text_list ($2, $3);
	driver.connectionid = yystack_[1].value.as < hostref_or_literal_t * > ();
	driver.put_exec_list();
}
#line 1319 "gix_esql_parser.cc"
    break;

  case 29: // disconnect: DISCONNECT
#line 254 "gix_esql_parser.yy"
           {yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < std::string > ());}
#line 1325 "gix_esql_parser.cc"
    break;

  case 30: // deletesql: execsql_with_opt_at delete token_list END_EXEC
#line 258 "gix_esql_parser.yy"
{
	yylhs.value.as < std::vector<std::string> * > () = driver.cb_concat_text_list (yystack_[2].value.as < std::vector<std::string> * > (), yystack_[1].value.as < std::vector<std::string> * > ());
	driver.put_exec_list();
}
#line 1334 "gix_esql_parser.cc"
    break;

  case 31: // delete: DELETE
#line 265 "gix_esql_parser.yy"
       {yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < std::string > ());}
#line 1340 "gix_esql_parser.cc"
    break;

  case 32: // insertsql: execsql_with_opt_at insert token_list END_EXEC
#line 269 "gix_esql_parser.yy"
{
	yylhs.value.as < std::vector<std::string> * > () = driver.cb_concat_text_list (yystack_[2].value.as < std::vector<std::string> * > (), yystack_[1].value.as < std::vector<std::string> * > ());
	driver.put_exec_list();
}
#line 1349 "gix_esql_parser.cc"
    break;

  case 33: // insert: INSERT
#line 275 "gix_esql_parser.yy"
       {yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < std::string > ());}
#line 1355 "gix_esql_parser.cc"
    break;

  case 34: // insert: insert INTO
#line 276 "gix_esql_parser.yy"
              {yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (yystack_[1].value.as < std::vector<std::string> * > (), yystack_[0].value.as < std::string > ());}
#line 1361 "gix_esql_parser.cc"
    break;

  case 35: // rollbacksql: execsql_with_opt_at ROLLBACK_WORK END_EXEC
#line 281 "gix_esql_parser.yy"
                                           {
	driver.put_exec_list();
}
#line 1369 "gix_esql_parser.cc"
    break;

  case 36: // rollbacksql: execsql_with_opt_at ROLLBACK_WORK TO SAVEPOINT TOKEN END_EXEC
#line 284 "gix_esql_parser.yy"
                                                                {
	// We intercept the ROLLBACK TO SAVEPOINT and pass it back as an SQL statement
	// of type "other" (unknown).

	driver.commandname = "PASSTHRU";
	driver.sqlnum++;
	driver.sqlname = string_format("SQ%04d", driver.sqlnum);
	driver.sql_list->push_back("ROLLBACK");
	driver.sql_list->push_back("TO");
	driver.sql_list->push_back("SAVEPOINT");
	driver.sql_list->push_back(yystack_[1].value.as < std::string > ());
	driver.put_exec_list();
}
#line 1387 "gix_esql_parser.cc"
    break;

  case 37: // commitsql: execsql_with_opt_at COMMIT_WORK END_EXEC
#line 299 "gix_esql_parser.yy"
                                         {
	driver.put_exec_list();
}
#line 1395 "gix_esql_parser.cc"
    break;

  case 38: // fetchsql: EXECSQL unexpected_at fetch INTO res_host_references END_EXEC
#line 305 "gix_esql_parser.yy"
                                                              {
	driver.put_exec_list();
}
#line 1403 "gix_esql_parser.cc"
    break;

  case 39: // fetch: FETCH expr
#line 311 "gix_esql_parser.yy"
           { 
	driver.cb_set_cursorname(yystack_[0].value.as < std::string > ());
}
#line 1411 "gix_esql_parser.cc"
    break;

  case 40: // host_references: host_reference
#line 317 "gix_esql_parser.yy"
               {driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < std::string > ());}
#line 1417 "gix_esql_parser.cc"
    break;

  case 42: // host_references: host_references host_reference
#line 319 "gix_esql_parser.yy"
                                 {driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < std::string > ());}
#line 1423 "gix_esql_parser.cc"
    break;

  case 43: // res_host_references: host_reference
#line 322 "gix_esql_parser.yy"
               {driver.cb_res_host_list_add (driver.res_host_reference_list, yystack_[0].value.as < std::string > ());}
#line 1429 "gix_esql_parser.cc"
    break;

  case 45: // res_host_references: res_host_references host_reference
#line 324 "gix_esql_parser.yy"
                                     {driver.cb_res_host_list_add (driver.res_host_reference_list, yystack_[0].value.as < std::string > ());}
#line 1435 "gix_esql_parser.cc"
    break;

  case 46: // closesql: EXECSQL unexpected_at CLOSE expr END_EXEC
#line 327 "gix_esql_parser.yy"
                                          {
	driver.cb_set_cursorname(yystack_[1].value.as < std::string > ());
	driver.put_exec_list();
}
#line 1444 "gix_esql_parser.cc"
    break;

  case 47: // opensql: EXECSQL unexpected_at OPEN expr END_EXEC
#line 333 "gix_esql_parser.yy"
                                          {
	driver.cb_set_cursorname(yystack_[1].value.as < std::string > ());
	driver.put_exec_list();
}
#line 1453 "gix_esql_parser.cc"
    break;

  case 48: // opensql: EXECSQL unexpected_at OPEN expr USING host_references END_EXEC
#line 337 "gix_esql_parser.yy"
                                                                 {
	driver.cb_set_cursorname(yystack_[3].value.as < std::string > ());
	driver.put_exec_list();
}
#line 1462 "gix_esql_parser.cc"
    break;

  case 49: // connectsql: EXECSQL CONNECT TO strliteral_or_hostref opt_connect_as USER strliteral_or_hostref opt_auth_info END_EXEC
#line 346 "gix_esql_parser.yy"
                                                                                                          {
	driver.conninfo = new esql_connection_info_t();

	switch (yystack_[1].value.as < connect_to_info_t * > ()->type) {
		case 0:	// [ USING :password ] omitted
			driver.conninfo->id = yystack_[4].value.as < hostref_or_literal_t * > ();
			driver.conninfo->data_source = yystack_[5].value.as < hostref_or_literal_t * > ();
			driver.conninfo->username = yystack_[2].value.as < hostref_or_literal_t * > ();
			driver.conninfo->password = new hostref_or_literal_t();
			driver.conninfo->dbname = new hostref_or_literal_t();			
			break;

		case 1:	// USING :password (no IDENTIFIED BY... follows)
			driver.conninfo->id = yystack_[4].value.as < hostref_or_literal_t * > ();
			driver.conninfo->data_source = yystack_[5].value.as < hostref_or_literal_t * > ();
			driver.conninfo->username = yystack_[2].value.as < hostref_or_literal_t * > ();
			driver.conninfo->password = yystack_[1].value.as < connect_to_info_t * > ()->t1;
			driver.conninfo->dbname = new hostref_or_literal_t();			
			break;

		case 2:	// USING :db_data_source IDENTIFIED BY :password
			driver.conninfo->id = yystack_[4].value.as < hostref_or_literal_t * > ();
			driver.conninfo->data_source = yystack_[1].value.as < connect_to_info_t * > ()->t1;
			driver.conninfo->username = yystack_[2].value.as < hostref_or_literal_t * > ();
			driver.conninfo->password = yystack_[1].value.as < connect_to_info_t * > ()->t2;
			driver.conninfo->dbname = yystack_[5].value.as < hostref_or_literal_t * > ();			
			break;

	}

	driver.put_exec_list();
}
#line 1499 "gix_esql_parser.cc"
    break;

  case 50: // connectsql: EXECSQL CONNECT strliteral_or_hostref IDENTIFIED_BY strliteral_or_hostref opt_at opt_using END_EXEC
#line 379 "gix_esql_parser.yy"
                                                                                                      {
	if (!yystack_[1].value.as < hostref_or_literal_t * > ()->is_set) {
		driver.warning(yylhs.location, "Unsupported connection mode, data source information not provided. Connection will fail."); 
	}
	
	driver.conninfo = new esql_connection_info_t();
	driver.conninfo->id = yystack_[2].value.as < hostref_or_literal_t * > ();
	driver.conninfo->data_source = yystack_[1].value.as < hostref_or_literal_t * > ();
	driver.conninfo->username = yystack_[5].value.as < hostref_or_literal_t * > ();
	driver.conninfo->password = yystack_[3].value.as < hostref_or_literal_t * > ();
	driver.put_exec_list();

}
#line 1517 "gix_esql_parser.cc"
    break;

  case 51: // connectsql: EXECSQL CONNECT USING strliteral_or_hostref END_EXEC
#line 393 "gix_esql_parser.yy"
                                                       {
	driver.conninfo = new esql_connection_info_t();
	driver.conninfo->data_source = yystack_[1].value.as < hostref_or_literal_t * > ();
	driver.put_exec_list();
}
#line 1527 "gix_esql_parser.cc"
    break;

  case 52: // opt_auth_info: %empty
#line 401 "gix_esql_parser.yy"
       { yylhs.value.as < connect_to_info_t * > () = new connect_to_info_t(); yylhs.value.as < connect_to_info_t * > ()->type = 0; }
#line 1533 "gix_esql_parser.cc"
    break;

  case 53: // opt_auth_info: USING strliteral_or_hostref opt_identified_by
#line 402 "gix_esql_parser.yy"
                                                {
	if (yystack_[0].value.as < connect_to_info_t * > () == nullptr) {
		yylhs.value.as < connect_to_info_t * > () = new connect_to_info_t(); 
		yylhs.value.as < connect_to_info_t * > ()->type = 1;
		yylhs.value.as < connect_to_info_t * > ()->t1 = yystack_[1].value.as < hostref_or_literal_t * > ();
	}
	else {
		yylhs.value.as < connect_to_info_t * > () = yystack_[0].value.as < connect_to_info_t * > ();
		yylhs.value.as < connect_to_info_t * > ()->t1 = yystack_[1].value.as < hostref_or_literal_t * > ();
	}
}
#line 1549 "gix_esql_parser.cc"
    break;

  case 54: // opt_identified_by: %empty
#line 416 "gix_esql_parser.yy"
       { yylhs.value.as < connect_to_info_t * > () = nullptr; }
#line 1555 "gix_esql_parser.cc"
    break;

  case 55: // opt_identified_by: IDENTIFIED_BY strliteral_or_hostref
#line 417 "gix_esql_parser.yy"
                                      {
	yylhs.value.as < connect_to_info_t * > () = new connect_to_info_t(); 
	yylhs.value.as < connect_to_info_t * > ()->type = 2;
	yylhs.value.as < connect_to_info_t * > ()->t2 = yystack_[0].value.as < hostref_or_literal_t * > ();
}
#line 1565 "gix_esql_parser.cc"
    break;

  case 56: // declaresqlvar: DECLARE_VAR TOKEN IS sql_type END_EXEC
#line 425 "gix_esql_parser.yy"
                                       {
	
	cb_field_ptr x;

	std::string var_name = yystack_[3].value.as < std::string > ();
	uint64_t type_info = yystack_[1].value.as < uint64_t > ();

	uint32_t length = type_info & 0xffffffff;
	uint16_t precision = (length >> 16);
	uint16_t scale = (length & 0xffff);
	int sql_type = (type_info >> 32);

	if (driver.field_map.find(var_name) == driver.field_map.end()) {
		std::string src_file = driver.lexer.src_location_stack.top().filename;
		std::tuple<uint64_t, int, int, std::string> d = std::make_tuple(type_info, driver.startlineno, driver.endlineno, src_file);
		driver.field_sql_type_info[var_name] = d;
	}
	else {
		x = driver.field_map[var_name];
		x->sql_type = type_info;

		x->is_varlen = IS_VARLEN(sql_type);
		x->usage = IS_BINARY(sql_type) ? Usage::Binary : Usage::None;

		x->pictype = -1;	// Preprocessor will build the correct PIC
		
		// We do not want to overwrite precision and scale as defined in the COBOL source
		// x->picnsize = precision;
		// x->scale = scale;

		driver.cb_set_commandname("DECLARE_VAR");
		driver.cb_host_list_add (driver.host_reference_list, x->sname);
		driver.put_exec_list(); 
		
		// This will be rewritten by the preprocessor, so
		// we comment out the original variable definition
		cb_exec_sql_stmt_ptr stmt = new cb_exec_sql_stmt_t();
		stmt->commandName = "COMMENT";
		stmt->src_file = x->defined_at_source_file;
		stmt->startLine = x->defined_at_source_line;
		stmt->endLine = x->defined_at_source_line;
		driver.exec_list->push_back(stmt);
	}
}
#line 1614 "gix_esql_parser.cc"
    break;

  case 57: // dbid: strliteral_or_hostref
#line 471 "gix_esql_parser.yy"
                            { yylhs.value.as < hostref_or_literal_t * > () = yystack_[0].value.as < hostref_or_literal_t * > (); }
#line 1620 "gix_esql_parser.cc"
    break;

  case 58: // opt_using: USING strliteral_or_hostref
#line 474 "gix_esql_parser.yy"
                             { yylhs.value.as < hostref_or_literal_t * > () = yystack_[0].value.as < hostref_or_literal_t * > (); }
#line 1626 "gix_esql_parser.cc"
    break;

  case 59: // opt_using: %empty
#line 475 "gix_esql_parser.yy"
         { yylhs.value.as < hostref_or_literal_t * > () = new hostref_or_literal_t(); }
#line 1632 "gix_esql_parser.cc"
    break;

  case 60: // opt_connect_as: AS dbid
#line 479 "gix_esql_parser.yy"
         { yylhs.value.as < hostref_or_literal_t * > () = yystack_[0].value.as < hostref_or_literal_t * > (); }
#line 1638 "gix_esql_parser.cc"
    break;

  case 61: // opt_connect_as: %empty
#line 480 "gix_esql_parser.yy"
         { yylhs.value.as < hostref_or_literal_t * > () = new hostref_or_literal_t(); }
#line 1644 "gix_esql_parser.cc"
    break;

  case 62: // opt_at: AT dbid
#line 484 "gix_esql_parser.yy"
        { yylhs.value.as < hostref_or_literal_t * > () = yystack_[0].value.as < hostref_or_literal_t * > (); }
#line 1650 "gix_esql_parser.cc"
    break;

  case 63: // opt_at: %empty
#line 485 "gix_esql_parser.yy"
         { yylhs.value.as < hostref_or_literal_t * > () = new hostref_or_literal_t(); }
#line 1656 "gix_esql_parser.cc"
    break;

  case 64: // unexpected_at: AT dbid
#line 489 "gix_esql_parser.yy"
        {  
	driver.warning(yylhs.location, "AT DB-NAME is not allowed for CURSOR access, always used from CURSOR DECLARE"); 
}
#line 1664 "gix_esql_parser.cc"
    break;

  case 65: // unexpected_at: %empty
#line 492 "gix_esql_parser.yy"
         { /* everything fine */ }
#line 1670 "gix_esql_parser.cc"
    break;

  case 66: // opt_dbid: %empty
#line 496 "gix_esql_parser.yy"
       { yylhs.value.as < hostref_or_literal_t * > () = new hostref_or_literal_t(); }
#line 1676 "gix_esql_parser.cc"
    break;

  case 67: // opt_dbid: dbid
#line 497 "gix_esql_parser.yy"
       { yylhs.value.as < hostref_or_literal_t * > () = yystack_[0].value.as < hostref_or_literal_t * > (); }
#line 1682 "gix_esql_parser.cc"
    break;

  case 68: // strliteral_or_hostref: TOKEN
#line 501 "gix_esql_parser.yy"
      { yylhs.value.as < hostref_or_literal_t * > () = new hostref_or_literal_t(yystack_[0].value.as < std::string > (), true); }
#line 1688 "gix_esql_parser.cc"
    break;

  case 69: // strliteral_or_hostref: host_reference
#line 502 "gix_esql_parser.yy"
                 { yylhs.value.as < hostref_or_literal_t * > () = new hostref_or_literal_t(yystack_[0].value.as < std::string > (), false); }
#line 1694 "gix_esql_parser.cc"
    break;

  case 70: // resetsql: EXECSQL CONNECT_RESET opt_dbid END_EXEC
#line 506 "gix_esql_parser.yy"
                                        { 
	driver.connectionid = yystack_[1].value.as < hostref_or_literal_t * > ();
	driver.put_exec_list();
}
#line 1703 "gix_esql_parser.cc"
    break;

  case 71: // othersql: execsql_with_opt_at OTHERFUNC opt_othersql_tokens END_EXEC
#line 513 "gix_esql_parser.yy"
                                                           {
	driver.commandname = "PASSTHRU";
	yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (NULL, yystack_[2].value.as < std::string > ());
	if (yystack_[1].value.as < std::vector<std::string> * > ()) {
		yylhs.value.as < std::vector<std::string> * > () = driver.cb_concat_text_list (yylhs.value.as < std::vector<std::string> * > (), yystack_[1].value.as < std::vector<std::string> * > ());
	}
	driver.put_exec_list();
}
#line 1716 "gix_esql_parser.cc"
    break;

  case 72: // opt_othersql_tokens: %empty
#line 524 "gix_esql_parser.yy"
       { yylhs.value.as < std::vector<std::string> * > () = nullptr; }
#line 1722 "gix_esql_parser.cc"
    break;

  case 73: // opt_othersql_tokens: othersql_token opt_othersql_tokens
#line 525 "gix_esql_parser.yy"
                                     {
	yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (NULL, yystack_[1].value.as < std::string > ());
	if (yystack_[0].value.as < std::vector<std::string> * > ())
		yylhs.value.as < std::vector<std::string> * > () = driver.cb_concat_text_list (yylhs.value.as < std::vector<std::string> * > (), yystack_[0].value.as < std::vector<std::string> * > ());
}
#line 1732 "gix_esql_parser.cc"
    break;

  case 74: // othersql_token: host_reference
#line 533 "gix_esql_parser.yy"
                { yylhs.value.as < std::string > () = driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < std::string > ()); }
#line 1738 "gix_esql_parser.cc"
    break;

  case 75: // othersql_token: TOKEN
#line 534 "gix_esql_parser.yy"
                        { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1744 "gix_esql_parser.cc"
    break;

  case 76: // incfile: EXECSQL INCLUDE INCLUDE_FILE END_EXEC
#line 538 "gix_esql_parser.yy"
                                     {
	driver.put_exec_list();
	driver.lexer.pushNewFile(driver.incfilename, &driver, true, true);
}
#line 1753 "gix_esql_parser.cc"
    break;

  case 77: // incfile: COPY
#line 542 "gix_esql_parser.yy"
       { 
	driver.put_exec_list(); 

	driver.lexer.pushNewFile(driver.incfilename, &driver, true, false);
}
#line 1763 "gix_esql_parser.cc"
    break;

  case 78: // includesql: EXECSQL INCLUDE INCLUDE_SQLCA END_EXEC
#line 549 "gix_esql_parser.yy"
                                      {
	driver.put_exec_list();
	driver.lexer.pushNewFile("SQLCA", &driver, true, true);
}
#line 1772 "gix_esql_parser.cc"
    break;

  case 79: // selectintosql: execsql_with_opt_at SELECT token_list INTO res_host_references SELECTFROM token_list END_EXEC
#line 555 "gix_esql_parser.yy"
                                                                                               {
	yylhs.value.as < std::vector<std::string> * > () = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, yystack_[6].value.as < std::string > ()), yystack_[5].value.as < std::vector<std::string> * > ());
	driver.cb_concat_text_list(yylhs.value.as < std::vector<std::string> * > (), driver.cb_text_list_add(NULL, yystack_[2].value.as < std::string > ()));
	driver.cb_concat_text_list(yylhs.value.as < std::vector<std::string> * > (), yystack_[1].value.as < std::vector<std::string> * > ());
	driver.put_exec_list();
}
#line 1783 "gix_esql_parser.cc"
    break;

  case 80: // selectintosql: execsql_with_opt_at SELECT token_list INTO res_host_references END_EXEC
#line 561 "gix_esql_parser.yy"
                                                                           {
	yylhs.value.as < std::vector<std::string> * > () = driver.cb_concat_text_list(driver.cb_text_list_add(NULL, yystack_[4].value.as < std::string > ()), yystack_[3].value.as < std::vector<std::string> * > ());
	driver.put_exec_list();
}
#line 1792 "gix_esql_parser.cc"
    break;

  case 81: // badsql: execsql_with_opt_at error END_EXEC
#line 568 "gix_esql_parser.yy"
{
	yyerrok;
}
#line 1800 "gix_esql_parser.cc"
    break;

  case 82: // declaresql: execsql_with_opt_at DECLARE sql_declaration END_EXEC
#line 573 "gix_esql_parser.yy"
                                                     {
	//driver.put_exec_list();
}
#line 1808 "gix_esql_parser.cc"
    break;

  case 83: // sql_declaration: TOKEN statement_declaration
#line 579 "gix_esql_parser.yy"
                                { driver.declared_statements.push_back(yystack_[1].value.as < std::string > ()); }
#line 1814 "gix_esql_parser.cc"
    break;

  case 84: // sql_declaration: TOKEN cursor_declaration
#line 580 "gix_esql_parser.yy"
                                        { 
	driver.cb_set_cursorname(yystack_[1].value.as < std::string > ()); 
	if (!driver.procedure_division_started)
 		driver.put_startup_exec_list(); 
	else
		driver.put_exec_list();	
}
#line 1826 "gix_esql_parser.cc"
    break;

  case 85: // sql_declaration: TOKEN table_declaration
#line 587 "gix_esql_parser.yy"
                                        { }
#line 1832 "gix_esql_parser.cc"
    break;

  case 86: // statement_declaration: STATEMENT
#line 591 "gix_esql_parser.yy"
          {
	
}
#line 1840 "gix_esql_parser.cc"
    break;

  case 87: // cursor_declaration: CURSOR opt_with_hold FOR select
#line 597 "gix_esql_parser.yy"
                                { driver.cb_set_cursor_hold(yystack_[2].value.as < int > ()); }
#line 1846 "gix_esql_parser.cc"
    break;

  case 88: // table_declaration: TABLE token_list
#line 601 "gix_esql_parser.yy"
                 {
 	driver.cb_set_commandname("DECLARE_TABLE");
 	driver.put_exec_list(); 
}
#line 1855 "gix_esql_parser.cc"
    break;

  case 89: // opt_with_hold: %empty
#line 608 "gix_esql_parser.yy"
                { yylhs.value.as < int > () = 0; }
#line 1861 "gix_esql_parser.cc"
    break;

  case 90: // opt_with_hold: WITH_HOLD
#line 609 "gix_esql_parser.yy"
            { yylhs.value.as < int > () = 1; }
#line 1867 "gix_esql_parser.cc"
    break;

  case 91: // preparesql: execsql_with_opt_at PREPARE TOKEN FROM strliteral_or_hostref END_EXEC
#line 613 "gix_esql_parser.yy"
                                                                      {

	driver.cb_set_commandname("PREPARE_STATEMENT");
	driver.statement_name = yystack_[3].value.as < std::string > ();

	if (yystack_[1].value.as < hostref_or_literal_t * > ()->is_literal) {
		driver.sql_list->push_back(unquote(yystack_[1].value.as < hostref_or_literal_t * > ()->name));
		driver.sqlnum++;
		driver.sqlname = string_format("SQ%04d", driver.sqlnum);
		driver.statement_source = nullptr;
	}
	else {
		driver.statement_source = yystack_[1].value.as < hostref_or_literal_t * > ();
		driver.sql_list->clear();
	}

	driver.put_exec_list();
}
#line 1890 "gix_esql_parser.cc"
    break;

  case 92: // executesql: execsql_with_opt_at EXECUTE IMMEDIATE strliteral_or_hostref END_EXEC
#line 634 "gix_esql_parser.yy"
                                                                     {
	driver.commandname = "EXECUTE_IMMEDIATE";

	if (yystack_[1].value.as < hostref_or_literal_t * > ()->is_literal) {
		driver.sql_list->push_back(unquote(yystack_[1].value.as < hostref_or_literal_t * > ()->name));
		driver.sqlnum++;
		driver.sqlname = string_format("SQ%04d", driver.sqlnum);
		driver.statement_source = nullptr;
	}
	else {
		driver.statement_source = yystack_[1].value.as < hostref_or_literal_t * > ();
		driver.sql_list->clear();
	}
	driver.put_exec_list();
}
#line 1910 "gix_esql_parser.cc"
    break;

  case 93: // executesql: execsql_with_opt_at EXECUTE TOKEN opt_using_hostref_list END_EXEC
#line 649 "gix_esql_parser.yy"
                                                                    {

	driver.commandname = "EXECUTE_PREPARED";
	driver.statement_name = yystack_[2].value.as < std::string > ();
	driver.put_exec_list();
}
#line 1921 "gix_esql_parser.cc"
    break;

  case 94: // ignoresql: EXECSQL IGNORE token_list END_EXEC
#line 659 "gix_esql_parser.yy"
{
	driver.put_exec_list();
}
#line 1929 "gix_esql_parser.cc"
    break;

  case 96: // opt_using_hostref_list: USING host_references
#line 667 "gix_esql_parser.yy"
                        { }
#line 1935 "gix_esql_parser.cc"
    break;

  case 97: // select: SELECT token_list
#line 671 "gix_esql_parser.yy"
                 { yylhs.value.as < std::vector<std::string> * > () = driver.cb_concat_text_list (driver.cb_text_list_add (NULL, yystack_[1].value.as < std::string > ()), yystack_[0].value.as < std::vector<std::string> * > ());}
#line 1941 "gix_esql_parser.cc"
    break;

  case 98: // token_list: expr
#line 675 "gix_esql_parser.yy"
                                {      yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (NULL, yystack_[0].value.as < std::string > ());}
#line 1947 "gix_esql_parser.cc"
    break;

  case 99: // token_list: token_list expr
#line 676 "gix_esql_parser.yy"
                        {      yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (yystack_[1].value.as < std::vector<std::string> * > (), yystack_[0].value.as < std::string > ());}
#line 1953 "gix_esql_parser.cc"
    break;

  case 100: // token_list: token_list host_reference
#line 677 "gix_esql_parser.yy"
                              {
	yylhs.value.as < std::vector<std::string> * > () = driver.cb_text_list_add (yystack_[1].value.as < std::vector<std::string> * > (), driver.cb_host_list_add (driver.host_reference_list, yystack_[0].value.as < std::string > ()));
}
#line 1961 "gix_esql_parser.cc"
    break;

  case 101: // host_reference: HOSTTOKEN
#line 682 "gix_esql_parser.yy"
          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1967 "gix_esql_parser.cc"
    break;

  case 102: // expr: TOKEN
#line 684 "gix_esql_parser.yy"
            { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1973 "gix_esql_parser.cc"
    break;

  case 103: // expr: SELECT
#line 685 "gix_esql_parser.yy"
       {}
#line 1979 "gix_esql_parser.cc"
    break;

  case 104: // expr: FOR
#line 686 "gix_esql_parser.yy"
     {}
#line 1985 "gix_esql_parser.cc"
    break;

  case 105: // expr: UPDATE
#line 687 "gix_esql_parser.yy"
        {}
#line 1991 "gix_esql_parser.cc"
    break;

  case 106: // $@1: %empty
#line 690 "gix_esql_parser.yy"
                               {
	driver.current_field = NULL;
	driver.description_field = NULL;
	driver.put_exec_list();
}
#line 2001 "gix_esql_parser.cc"
    break;

  case 107: // sqlvariantstates: WORKINGBEGIN $@1 sqlvariantstate_list WORKINGEND
#line 696 "gix_esql_parser.yy"
           {
	// check host_variable
	driver.put_exec_list();
}
#line 2010 "gix_esql_parser.cc"
    break;

  case 108: // $@2: %empty
#line 700 "gix_esql_parser.yy"
              {
	driver.current_field = NULL;
	driver.description_field = NULL;
	driver.put_exec_list();
}
#line 2020 "gix_esql_parser.cc"
    break;

  case 109: // sqlvariantstates: LINKAGEBEGIN $@2 sqlvariantstate_list LINKAGEEND
#line 706 "gix_esql_parser.yy"
           {
	// check host_variable
	driver.put_exec_list();
}
#line 2029 "gix_esql_parser.cc"
    break;

  case 110: // $@3: %empty
#line 710 "gix_esql_parser.yy"
            {
	driver.current_field = NULL;
	driver.description_field = NULL;
	driver.put_exec_list();
}
#line 2039 "gix_esql_parser.cc"
    break;

  case 111: // sqlvariantstates: FILEBEGIN $@3 sqlvariantstate_list FILEEND
#line 716 "gix_esql_parser.yy"
        {
	// check host_variable
	driver.put_exec_list();
}
#line 2048 "gix_esql_parser.cc"
    break;

  case 112: // sqlvariantstates: PROCEDURE_DIVISION
#line 720 "gix_esql_parser.yy"
                    {
	driver.put_exec_list();
}
#line 2056 "gix_esql_parser.cc"
    break;

  case 113: // fd_def: FD token_list PERIOD
#line 726 "gix_esql_parser.yy"
                      {}
#line 2062 "gix_esql_parser.cc"
    break;

  case 120: // sqlvariantstate_list: sqlvariantstate_list HOSTVARIANTBEGIN
#line 737 "gix_esql_parser.yy"
                                       { driver.put_exec_list(); }
#line 2068 "gix_esql_parser.cc"
    break;

  case 121: // sqlvariantstate_list: sqlvariantstate_list HOSTVARIANTEND
#line 738 "gix_esql_parser.yy"
                                     { driver.put_exec_list(); }
#line 2074 "gix_esql_parser.cc"
    break;

  case 124: // $@4: %empty
#line 744 "gix_esql_parser.yy"
                              {
	cb_field_ptr x;

	x =  driver.cb_build_field_tree( yystack_[2].value.as < long > (), yystack_[1].value.as < std::string > () , driver.current_field);
	if(x != NULL)
	{
		if( x->level != 78)
			driver.current_field = x;

		if (yystack_[0].value.as < uint64_t > () != 0) {
			uint64_t type_info = yystack_[0].value.as < uint64_t > ();
			uint32_t length = type_info & 0xffffffff;
			uint16_t precision = (length >> 16);
			uint16_t scale = (length & 0xffff);
			int sql_type = type_info >> 32;

			x->sql_type = type_info;
			x->is_varlen = IS_VARLEN(sql_type);
			x->usage = IS_BINARY(sql_type) ? Usage::Binary : Usage::None;

			x->pictype = -1;	// Preprocessor will build the correct PIC for "SQL TYPE IS" defs

			// We do not want to overwrite precision and scale as defined in the COBOL source
			// x->picnsize = precision;
			// x->scale = scale;
			
			driver.cb_set_commandname("DECLARE_VAR");
			driver.cb_host_list_add (driver.host_reference_list, x->sname);
			driver.put_exec_list(); 
		}
	}
}
#line 2111 "gix_esql_parser.cc"
    break;

  case 125: // sqlvariantstate: NUMERIC WORD opt_sql_type_def $@4 data_description_clause_sequence
#line 777 "gix_esql_parser.yy"
{
	if (driver.description_field == NULL)
		driver.description_field = driver.current_field;
}
#line 2120 "gix_esql_parser.cc"
    break;

  case 126: // $@5: %empty
#line 781 "gix_esql_parser.yy"
         {
	cb_field_ptr x;

	x =  driver.cb_build_field_tree( yystack_[0].value.as < long > (), "" , driver.current_field); // regist dummy name
	if( x != NULL){
	}
}
#line 2132 "gix_esql_parser.cc"
    break;

  case 127: // sqlvariantstate: NUMERIC $@5 data_description_clause_sequence
#line 789 "gix_esql_parser.yy"
{
	if (driver.description_field == NULL)
		driver.description_field = driver.current_field;
}
#line 2141 "gix_esql_parser.cc"
    break;

  case 128: // opt_sql_type_def: %empty
#line 796 "gix_esql_parser.yy"
       { yylhs.value.as < uint64_t > () = 0; }
#line 2147 "gix_esql_parser.cc"
    break;

  case 129: // opt_sql_type_def: SQL_TYPE_IS sql_type
#line 797 "gix_esql_parser.yy"
                       {
		yylhs.value.as < uint64_t > () = yystack_[0].value.as < uint64_t > ();
}
#line 2155 "gix_esql_parser.cc"
    break;

  case 130: // sql_type: BINARY
#line 803 "gix_esql_parser.yy"
                { yylhs.value.as < uint64_t > () = (TYPE_SQL_BINARY << 32) + yystack_[0].value.as < uint64_t > (); }
#line 2161 "gix_esql_parser.cc"
    break;

  case 131: // sql_type: VARBINARY
#line 804 "gix_esql_parser.yy"
            { yylhs.value.as < uint64_t > () = (TYPE_SQL_VARBINARY << 32) + yystack_[0].value.as < uint64_t > (); }
#line 2167 "gix_esql_parser.cc"
    break;

  case 132: // sql_type: CHAR
#line 805 "gix_esql_parser.yy"
                { yylhs.value.as < uint64_t > () = (TYPE_SQL_CHAR << 32) + yystack_[0].value.as < uint64_t > (); }
#line 2173 "gix_esql_parser.cc"
    break;

  case 133: // sql_type: VARCHAR
#line 806 "gix_esql_parser.yy"
                { yylhs.value.as < uint64_t > () = (TYPE_SQL_VARCHAR << 32) + yystack_[0].value.as < uint64_t > (); }
#line 2179 "gix_esql_parser.cc"
    break;

  case 134: // sql_type: INTEGER
#line 807 "gix_esql_parser.yy"
                { yylhs.value.as < uint64_t > () = (TYPE_SQL_INT << 32) + yystack_[0].value.as < uint64_t > (); }
#line 2185 "gix_esql_parser.cc"
    break;

  case 135: // sql_type: FLOAT
#line 808 "gix_esql_parser.yy"
                { yylhs.value.as < uint64_t > () = (TYPE_SQL_FLOAT << 32) + yystack_[0].value.as < uint64_t > (); }
#line 2191 "gix_esql_parser.cc"
    break;

  case 136: // sql_type: DECIMAL
#line 809 "gix_esql_parser.yy"
                { yylhs.value.as < uint64_t > () = (TYPE_SQL_DECIMAL << 32) + yystack_[0].value.as < uint64_t > (); }
#line 2197 "gix_esql_parser.cc"
    break;

  case 137: // data_description_clause_sequence: %empty
#line 813 "gix_esql_parser.yy"
       {}
#line 2203 "gix_esql_parser.cc"
    break;

  case 138: // data_description_clause_sequence: data_description_clause_sequence data_description_clause
#line 815 "gix_esql_parser.yy"
{}
#line 2209 "gix_esql_parser.cc"
    break;

  case 145: // picture_clause: PICTURE
#line 828 "gix_esql_parser.yy"
                {  driver.build_picture( yystack_[0].value.as < std::string > (),driver.current_field);  }
#line 2215 "gix_esql_parser.cc"
    break;

  case 148: // usage: COMP
#line 837 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Binary;  }
#line 2221 "gix_esql_parser.cc"
    break;

  case 149: // usage: BINARY
#line 838 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Binary;  }
#line 2227 "gix_esql_parser.cc"
    break;

  case 150: // usage: COMP_1
#line 839 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Float;   }
#line 2233 "gix_esql_parser.cc"
    break;

  case 151: // usage: COMP_2
#line 840 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Double;  }
#line 2239 "gix_esql_parser.cc"
    break;

  case 152: // usage: COMP_3
#line 841 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::Packed;  }
#line 2245 "gix_esql_parser.cc"
    break;

  case 153: // usage: COMP_5
#line 842 "gix_esql_parser.yy"
                                { driver.current_field->usage = Usage::NativeBinary;  }
#line 2251 "gix_esql_parser.cc"
    break;

  case 154: // usage: WORD
#line 843 "gix_esql_parser.yy"
                    { driver.current_field->usage = Usage::None;    }
#line 2257 "gix_esql_parser.cc"
    break;

  case 155: // value_clause: VALUE _is_are _all const_clause
#line 846 "gix_esql_parser.yy"
                                              {}
#line 2263 "gix_esql_parser.cc"
    break;

  case 156: // const_clause: NUMERIC
#line 849 "gix_esql_parser.yy"
        {}
#line 2269 "gix_esql_parser.cc"
    break;

  case 157: // const_clause: WORD
#line 850 "gix_esql_parser.yy"
      { }
#line 2275 "gix_esql_parser.cc"
    break;

  case 158: // const_clause: CONST
#line 851 "gix_esql_parser.yy"
       {}
#line 2281 "gix_esql_parser.cc"
    break;

  case 159: // sign_clause: _sign_is LEADING flag_separate
#line 855 "gix_esql_parser.yy"
{
	driver.current_field->sign_leading = SIGN_LEADING;
}
#line 2289 "gix_esql_parser.cc"
    break;

  case 160: // sign_clause: _sign_is TRAILING flag_separate
#line 859 "gix_esql_parser.yy"
{

}
#line 2297 "gix_esql_parser.cc"
    break;

  case 161: // _sign_is: SIGN
#line 864 "gix_esql_parser.yy"
                       {}
#line 2303 "gix_esql_parser.cc"
    break;

  case 162: // _sign_is: SIGN IS
#line 865 "gix_esql_parser.yy"
          {}
#line 2309 "gix_esql_parser.cc"
    break;

  case 164: // flag_separate: SEPARATE
#line 869 "gix_esql_parser.yy"
           { driver.current_field->separate = SIGN_SEPARATE; }
#line 2315 "gix_esql_parser.cc"
    break;

  case 183: // external_clause: _is EXTERNAL
#line 916 "gix_esql_parser.yy"
             {}
#line 2321 "gix_esql_parser.cc"
    break;


#line 2325 "gix_esql_parser.cc"

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


  const signed char gix_esql_parser::yypact_ninf_ = -48;

  const short gix_esql_parser::yytable_ninf_ = -185;

  const short
  gix_esql_parser::yypact_[] =
  {
     -48,     4,   -48,   189,   -48,   -48,   -48,   -48,   -48,   -48,
      52,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,
     -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,
     -48,   -48,    71,   -21,   142,    71,   224,    71,   -48,   164,
     -48,   -48,   -48,    29,   224,   -48,   -48,   -48,    20,    75,
      17,    35,    25,    65,   224,   224,   151,   -48,   -48,   -48,
      38,   -48,   -48,    56,    71,    71,    44,   203,   -48,   -48,
     -48,   -48,   208,   -48,    87,   224,   224,   224,    83,    81,
     134,   141,   -48,   237,    57,    71,   -48,   108,    75,   -48,
     -20,   109,   -48,   -48,    97,   111,     3,   245,   -48,   251,
     -48,   -48,   103,   145,    71,   -48,   -48,   -48,   -48,     0,
     148,   -48,   153,   162,    51,   -48,   224,   -48,   -48,   166,
     155,   -48,   -48,   -48,   -48,   -48,   -48,   172,   -48,   -48,
     153,   153,   163,   173,   -48,   -48,   101,   -48,   224,   -48,
     -48,   -48,   -48,   190,    71,   -48,   224,   -48,   -48,    71,
     120,   -48,   165,   -48,   153,   -48,    86,   -48,   137,   -48,
      95,    71,   200,   171,   -48,    84,   116,   -48,   -48,   -48,
     -48,   191,   272,   206,   210,   213,   -48,    71,   178,   107,
     -48,   -48,   -48,   223,   -48,    78,   217,   -48,   -48,   223,
     224,   -48,   -48,   -48,   235,   -48,   -48,   -48,   198,    71,
     247,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,
     -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,
     221,   222,   -31,    -1,   -48,   -48,   -48,   -48,   -48,   -48,
     -27,   -48,   -48,   184,   -48,   256,   261,   224,   -48,    71,
     262,   -48,   -48,    78,   123,   -48,   -48,   -48,   205,   -22,
      -3,   207,   207,   -48,   -48,   -48,   272,   241,   -48,   -48,
     -48,    -2,    10,   278,   214,   212,   211,   291,   212,   -48,
     -48,   -48,    71,   -48,   -48,   -48,   -48,   -48,   -48,   -48,
     215,   -48,   292,   -48,   294,   -48,   219,   225,   -48,   -48,
     -48,   226,   -48,   -48,   296,   -48,   299,   298,   -48,   -48,
     -48
  };

  const unsigned char
  gix_esql_parser::yydefact_[] =
  {
       2,     0,     1,    63,   106,   108,   110,   112,    77,     3,
       0,    15,    16,    14,    13,    11,    10,     9,     8,     7,
       6,    17,    18,     5,    12,    23,    19,    20,    21,    22,
       4,    29,    66,     0,     0,     0,     0,    66,    24,     0,
     114,   114,   114,     0,     0,    33,    27,    31,     0,    72,
       0,     0,     0,     0,     0,     0,     0,    68,   101,    67,
       0,    57,    69,     0,     0,     0,     0,    62,   103,   102,
     105,   104,     0,    98,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    95,     0,    75,     0,    72,    74,
       0,     0,    37,    35,     0,     0,     0,     0,    34,     0,
      70,    76,    61,     0,     0,    94,   100,    99,    28,     0,
       0,    39,     0,   126,    63,   107,     0,   120,   121,     0,
       0,   123,   116,   117,   118,   122,   115,     0,   109,   111,
       0,     0,     0,     0,    71,    73,    89,    86,     0,    83,
      84,    85,    82,     0,     0,    25,     0,    30,    32,     0,
       0,    51,    63,    47,     0,    46,     0,    43,   128,   137,
       0,     0,     0,     0,   119,     0,    96,    40,    93,    92,
      90,     0,    88,     0,     0,     0,    60,     0,    59,     0,
      44,    38,    45,     0,   124,   127,     0,    62,   113,     0,
       0,    80,    41,    42,     0,    36,    91,    26,    52,     0,
       0,    48,   132,   133,   130,   131,   135,   134,   136,   129,
     137,   154,   145,   185,   150,   151,   152,   153,   148,   149,
     184,   161,   186,     0,   138,   139,   140,   146,   143,   141,
       0,   142,   144,     0,    78,     0,     0,     0,    87,     0,
       0,    58,    50,   125,     0,   162,   187,   188,   189,   170,
     173,   163,   163,   183,    56,    79,    97,    54,    49,   147,
     190,     0,     0,     0,   171,   177,     0,     0,   177,   164,
     160,   159,     0,    53,   157,   156,   158,   155,   182,   181,
       0,   168,     0,   169,     0,   165,   179,     0,   175,   166,
      55,     0,   172,   178,     0,   176,     0,     0,   180,   174,
     167
  };

  const short
  gix_esql_parser::yypgoto_[] =
  {
     -48,   -48,   -48,   308,   -48,   -48,   -48,   -48,   -48,   -48,
     -48,   -48,   -48,   -48,   -48,   -48,   156,   181,   -48,   -48,
     -48,   -48,   -48,   -48,   -32,   -48,   -48,   160,   -48,   276,
     -34,   -48,   -48,   227,   -48,   313,   -48,   -48,   -48,   315,
     -48,   -48,   -48,   -48,   -48,   -48,   -48,   316,   -48,   -48,
     -43,   -47,   -38,   -48,   -48,   -48,   -48,   -48,   158,   -48,
     -48,   -48,   -48,   129,   110,   -48,   -48,   -48,    77,   -48,
     -48,   -48,   -48,    67,   -48,   -48,   -48,   -48,    54,   -48,
     -48,   -48,   -48,   104,   -48,   -48
  };

  const short
  gix_esql_parser::yydefgoto_[] =
  {
      -1,     1,     9,   120,    11,    54,    12,    37,    13,    55,
      14,    56,    15,    16,    17,    78,   166,   156,    18,    19,
      20,   240,   273,   121,    59,   200,   150,    38,    39,    60,
      61,    21,    22,    87,    88,   122,   123,    24,    25,   124,
      91,   139,   140,   141,   171,    27,    28,   125,   132,   238,
      72,    62,    73,    30,    40,    41,    42,   126,    79,   127,
     210,   159,   184,   209,   185,   224,   225,   226,   227,   228,
     277,   229,   230,   270,   231,   265,   283,   268,   285,   286,
     295,   280,   232,   233,   248,   261
  };

  const short
  gix_esql_parser::yytable_[] =
  {
      66,    83,    89,    67,     2,   136,   274,    68,   137,    69,
      58,    96,    97,    99,    70,   266,   275,   249,   278,   153,
      63,   246,   145,    90,     3,   106,    84,   262,   279,    71,
     102,   103,   251,     4,   107,     5,   106,   109,   110,   111,
       6,    89,     7,    85,    93,   107,   247,   252,    82,   106,
     106,   133,   106,    43,    92,   154,    44,   100,   107,   107,
     263,   107,    45,    46,   264,   157,    47,    48,    49,   138,
     152,    95,   160,   162,    94,   101,    50,    57,    58,   267,
     250,    86,    58,   157,   167,   276,   211,   212,   104,   190,
     180,    58,   180,    58,     8,   172,   146,    51,    52,   113,
     112,   114,   161,   191,    36,   181,   108,   167,   175,   182,
     174,   115,   131,   192,    58,   106,   116,   176,   182,   193,
     117,   118,   192,    58,   107,   106,   201,   134,   142,   187,
     213,   211,   193,   144,   107,   119,    63,   186,   214,   215,
     216,   217,   218,   198,   143,   219,    53,   236,    57,    58,
     220,   221,   113,   149,   114,    68,   222,    69,   223,   113,
      58,   114,    70,  -184,   151,   241,   128,   155,    98,   116,
     158,     8,   163,   117,   118,   164,   116,    71,   129,    50,
     117,   118,   168,   214,   215,   216,   217,   218,   119,   106,
     219,    64,   169,   170,   256,   119,   173,    65,   107,    80,
      81,    31,    32,   188,    68,   257,    69,    58,   177,   106,
      33,    70,    68,   183,    69,    58,   161,   194,   107,    70,
      75,    76,    77,   189,     8,   195,    71,   105,    68,   196,
      69,     8,   197,   199,    71,    70,   234,    34,   290,   237,
      35,    68,    36,    69,    58,   -65,   -65,   -65,    70,    68,
      71,    69,    58,   239,   130,    68,    70,    69,    58,   -64,
     -64,   -64,    70,    71,   147,    68,   242,    69,    58,   253,
     148,    71,    70,   213,   245,   254,    68,    71,    69,    58,
     255,   258,   269,    70,   260,   272,   281,    71,   202,   203,
     204,   205,   206,   207,   208,   284,   282,   287,    71,   288,
     292,   291,   293,   294,   298,   299,   300,   296,   297,    10,
     179,   165,   178,    74,    23,   135,    26,    29,   235,   271,
     243,   259,   289,     0,   244
  };

  const short
  gix_esql_parser::yycheck_[] =
  {
      34,    44,    49,    35,     0,    25,     8,     4,    28,     6,
       7,    54,    55,    56,    11,    18,    18,    18,     8,    19,
      41,    52,    19,     6,    20,    72,     6,    49,    18,    26,
      64,    65,    59,    29,    72,    31,    83,    75,    76,    77,
      36,    88,    38,    23,    19,    83,    77,    74,    19,    96,
      97,    85,    99,     1,    19,    55,     4,    19,    96,    97,
      82,    99,    10,    11,    86,   112,    14,    15,    16,    89,
     104,     6,    21,   116,    49,    19,    24,     6,     7,    82,
      81,     6,     7,   130,   131,    87,     8,     9,    44,     5,
       6,     7,     6,     7,    90,   138,    93,    45,    46,    18,
      17,    20,    51,    19,    53,    19,    19,   154,   146,   156,
     144,    30,    55,     6,     7,   162,    35,   149,   165,   166,
      39,    40,     6,     7,   162,   172,    19,    19,    19,   161,
      52,     8,   179,    22,   172,    54,    41,    42,    60,    61,
      62,    63,    64,   177,    47,    67,    94,   190,     6,     7,
      72,    73,    18,    50,    20,     4,    78,     6,    80,    18,
       7,    20,    11,    85,    19,   199,    32,    19,    17,    35,
       8,    90,     6,    39,    40,     3,    35,    26,    37,    24,
      39,    40,    19,    60,    61,    62,    63,    64,    54,   236,
      67,    49,    19,    92,   237,    54,     6,    55,   236,    41,
      42,    12,    13,     3,     4,   239,     6,     7,    88,   256,
      21,    11,     4,    76,     6,     7,    51,    26,   256,    11,
      56,    57,    58,    52,    90,    19,    26,    19,     4,    19,
       6,    90,    19,    55,    26,    11,    19,    48,   272,     4,
      51,     4,    53,     6,     7,    56,    57,    58,    11,     4,
      26,     6,     7,    55,    17,     4,    11,     6,     7,    56,
      57,    58,    11,    26,    19,     4,    19,     6,     7,    85,
      19,    26,    11,    52,    52,    19,     4,    26,     6,     7,
      19,    19,    75,    11,    79,    44,     8,    26,    65,    66,
      67,    68,    69,    70,    71,    83,    82,    86,    26,     8,
       8,    86,     8,    84,     8,     6,     8,    82,    82,     1,
     154,   130,   152,    37,     1,    88,     1,     1,   189,   252,
     210,   244,   268,    -1,   220
  };

  const unsigned char
  gix_esql_parser::yystos_[] =
  {
       0,    96,     0,    20,    29,    31,    36,    38,    90,    97,
      98,    99,   101,   103,   105,   107,   108,   109,   113,   114,
     115,   126,   127,   130,   132,   133,   134,   140,   141,   142,
     148,    12,    13,    21,    48,    51,    53,   102,   122,   123,
     149,   150,   151,     1,     4,    10,    11,    14,    15,    16,
      24,    45,    46,    94,   100,   104,   106,     6,     7,   119,
     124,   125,   146,    41,    49,    55,   125,   119,     4,     6,
      11,    26,   145,   147,   124,    56,    57,    58,   110,   153,
     153,   153,    19,   145,     6,    23,     6,   128,   129,   146,
       6,   135,    19,    19,    49,     6,   145,   145,    17,   145,
      19,    19,   125,   125,    44,    19,   146,   147,    19,   147,
     147,   147,    17,    18,    20,    30,    35,    39,    40,    54,
      98,   118,   130,   131,   134,   142,   152,   154,    32,    37,
      17,    55,   143,   125,    19,   128,    25,    28,    89,   136,
     137,   138,    19,    47,    22,    19,    93,    19,    19,    50,
     121,    19,   125,    19,    55,    19,   112,   146,     8,   156,
      21,    51,   145,     6,     3,   112,   111,   146,    19,    19,
      92,   139,   145,     6,   125,   147,   119,    88,   122,   111,
       6,    19,   146,    76,   157,   159,    42,   119,     3,    52,
       5,    19,     6,   146,    26,    19,    19,    19,   125,    55,
     120,    19,    65,    66,    67,    68,    69,    70,    71,   158,
     155,     8,     9,    52,    60,    61,    62,    63,    64,    67,
      72,    73,    78,    80,   160,   161,   162,   163,   164,   166,
     167,   169,   177,   178,    19,   158,   145,     4,   144,    55,
     116,   125,    19,   159,   178,    52,    52,    77,   179,    18,
      81,    59,    74,    85,    19,    19,   145,   125,    19,   163,
      79,   180,    49,    82,    86,   170,    18,    82,   172,    75,
     168,   168,    44,   117,     8,    18,    87,   165,     8,    18,
     176,     8,    82,   171,    83,   173,   174,    86,     8,   173,
     125,    86,     8,     8,    84,   175,    82,    82,     8,     6,
       8
  };

  const unsigned char
  gix_esql_parser::yyr1_[] =
  {
       0,    95,    96,    96,    97,    97,    97,    97,    97,    97,
      97,    97,    97,    97,    97,    97,    97,    97,    97,    97,
      97,    97,    97,    97,    98,    99,    99,   100,   101,   102,
     103,   104,   105,   106,   106,   107,   107,   108,   109,   110,
     111,   111,   111,   112,   112,   112,   113,   114,   114,   115,
     115,   115,   116,   116,   117,   117,   118,   119,   120,   120,
     121,   121,   122,   122,   123,   123,   124,   124,   125,   125,
     126,   127,   128,   128,   129,   129,   130,   130,   131,   132,
     132,   133,   134,   135,   135,   135,   136,   137,   138,   139,
     139,   140,   141,   141,   142,   143,   143,   144,   145,   145,
     145,   146,   147,   147,   147,   147,   149,   148,   150,   148,
     151,   148,   148,   152,   153,   153,   153,   153,   153,   153,
     153,   153,   153,   153,   155,   154,   156,   154,   157,   157,
     158,   158,   158,   158,   158,   158,   158,   159,   159,   160,
     160,   160,   160,   160,   160,   161,   162,   162,   163,   163,
     163,   163,   163,   163,   163,   164,   165,   165,   165,   166,
     166,   167,   167,   168,   168,   169,   169,   170,   170,   170,
     170,   171,   171,   172,   172,   172,   173,   174,   174,   175,
     175,   176,   176,   177,   178,   178,   179,   179,   179,   180,
     180
  };

  const signed char
  gix_esql_parser::yyr2_[] =
  {
       0,     2,     0,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     4,     6,     1,     4,     1,
       4,     1,     4,     1,     2,     3,     6,     3,     6,     2,
       1,     2,     2,     1,     2,     2,     5,     5,     7,     9,
       8,     5,     0,     3,     0,     2,     5,     1,     2,     0,
       2,     0,     2,     0,     2,     0,     0,     1,     1,     1,
       4,     4,     0,     2,     1,     1,     4,     1,     4,     8,
       6,     3,     4,     2,     2,     2,     1,     4,     2,     0,
       1,     6,     5,     5,     4,     0,     2,     2,     1,     2,
       2,     1,     1,     1,     1,     1,     0,     4,     0,     4,
       0,     4,     1,     3,     0,     2,     2,     2,     2,     3,
       2,     2,     2,     2,     0,     5,     0,     3,     0,     2,
       1,     1,     1,     1,     1,     1,     1,     0,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     4,     1,     1,     1,     3,
       3,     1,     2,     0,     1,     4,     4,     5,     2,     2,
       0,     0,     2,     0,     4,     2,     2,     0,     2,     0,
       2,     1,     1,     2,     0,     1,     0,     1,     1,     0,
       1
  };


#if YYDEBUG || 1
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char*
  const gix_esql_parser::yytname_[] =
  {
  "\"end of file\"", "error", "\"invalid token\"", "PERIOD", "SELECT",
  "SELECTFROM", "TOKEN", "HOSTTOKEN", "WORD", "PICTURE", "INSERT",
  "UPDATE", "DISCONNECT", "CONNECT_RESET", "DELETE", "EXECUTE",
  "OTHERFUNC", "INTO", "NUMERIC", "END_EXEC", "EXECSQL", "INCLUDE", "FROM",
  "IMMEDIATE", "DECLARE", "CURSOR", "FOR", "COMMA", "STATEMENT",
  "WORKINGBEGIN", "WORKINGEND", "LINKAGEBEGIN", "LINKAGEEND",
  "LOCALSTORAGEBEGIN", "LOCALSTORAGEEND", "FD", "FILEBEGIN", "FILEEND",
  "PROCEDURE_DIVISION", "HOSTVARIANTBEGIN", "HOSTVARIANTEND",
  "INCLUDE_FILE", "INCLUDE_SQLCA", "SQLCA", "IDENTIFIED_BY", "COMMIT_WORK",
  "ROLLBACK_WORK", "SAVEPOINT", "CONNECT", "TO", "AS", "AT", "IS",
  "IGNORE", "DECLARE_VAR", "USING", "OPEN", "CLOSE", "FETCH", "TRAILING",
  "COMP_1", "COMP_2", "COMP_3", "COMP_5", "COMP", "CHAR", "VARCHAR",
  "BINARY", "VARBINARY", "FLOAT", "INTEGER", "DECIMAL", "USAGE", "SIGN",
  "LEADING", "SEPARATE", "SQL_TYPE_IS", "ARE", "VALUE", "ALL", "OCCURS",
  "UNBOUNDED", "DEPENDING_ON", "ASCENDING_KEY_IS", "INDEXED_BY",
  "EXTERNAL", "TIMES", "CONST", "USER", "TABLE", "COPY", "COPY_FILE",
  "WITH_HOLD", "WHERE_CURRENT_OF", "PREPARE", "$accept", "sqlstate_list",
  "sqlstate", "execsql_with_opt_at", "updatesql", "update",
  "disconnectsql", "disconnect", "deletesql", "delete", "insertsql",
  "insert", "rollbacksql", "commitsql", "fetchsql", "fetch",
  "host_references", "res_host_references", "closesql", "opensql",
  "connectsql", "opt_auth_info", "opt_identified_by", "declaresqlvar",
  "dbid", "opt_using", "opt_connect_as", "opt_at", "unexpected_at",
  "opt_dbid", "strliteral_or_hostref", "resetsql", "othersql",
  "opt_othersql_tokens", "othersql_token", "incfile", "includesql",
  "selectintosql", "badsql", "declaresql", "sql_declaration",
  "statement_declaration", "cursor_declaration", "table_declaration",
  "opt_with_hold", "preparesql", "executesql", "ignoresql",
  "opt_using_hostref_list", "select", "token_list", "host_reference",
  "expr", "sqlvariantstates", "$@1", "$@2", "$@3", "fd_def",
  "sqlvariantstate_list", "sqlvariantstate", "$@4", "$@5",
  "opt_sql_type_def", "sql_type", "data_description_clause_sequence",
  "data_description_clause", "picture_clause", "usage_clause", "usage",
  "value_clause", "const_clause", "sign_clause", "_sign_is",
  "flag_separate", "occurs_clause", "occurs_numeric_data",
  "opt_depending_on", "occurs_unbounded_data", "occurs_sort_opts",
  "opt_ascending_key_is", "opt_indexed_by", "numeric_or_word",
  "external_clause", "_is", "_is_are", "_all", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  gix_esql_parser::yyrline_[] =
  {
       0,   196,   196,   197,   200,   201,   202,   203,   204,   205,
     206,   207,   208,   209,   210,   211,   212,   213,   214,   215,
     216,   217,   218,   219,   222,   227,   232,   242,   246,   254,
     257,   265,   268,   275,   276,   281,   284,   299,   305,   311,
     317,   318,   319,   322,   323,   324,   327,   333,   337,   346,
     379,   393,   401,   402,   416,   417,   425,   471,   474,   475,
     479,   480,   484,   485,   489,   492,   496,   497,   501,   502,
     506,   513,   524,   525,   533,   534,   538,   542,   549,   555,
     561,   567,   573,   579,   580,   587,   591,   597,   601,   608,
     609,   613,   634,   649,   658,   666,   667,   671,   675,   676,
     677,   682,   684,   685,   686,   687,   690,   690,   700,   700,
     710,   710,   720,   726,   731,   732,   733,   734,   735,   736,
     737,   738,   739,   740,   744,   744,   781,   781,   796,   797,
     803,   804,   805,   806,   807,   808,   809,   813,   814,   819,
     820,   821,   822,   823,   824,   828,   832,   833,   837,   838,
     839,   840,   841,   842,   843,   846,   849,   850,   851,   854,
     858,   864,   865,   868,   869,   873,   874,   878,   879,   880,
     881,   885,   886,   890,   891,   892,   896,   900,   901,   905,
     906,   911,   912,   916,   919,   919,   920,   920,   920,   921,
     921
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
#line 3044 "gix_esql_parser.cc"

#line 924 "gix_esql_parser.yy"


// Register errors to the driver:
void yy::gix_esql_parser::error (const location_type& l, const std::string& m)
{
    driver.error(l, m, ERR_SYNTAX_ERROR);
}
