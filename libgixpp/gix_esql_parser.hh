// A Bison parser, made by GNU Bison 3.7.4.

// Skeleton interface for Bison LALR(1) parsers in C++

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


/**
 ** \file gix_esql_parser.hh
 ** Define the yy::parser class.
 */

// C++ LALR(1) parser skeleton written by Akim Demaille.

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.

#ifndef YY_YY_GIX_ESQL_PARSER_HH_INCLUDED
# define YY_YY_GIX_ESQL_PARSER_HH_INCLUDED
// "%code requires" blocks.
#line 11 "gix_esql_parser.yy"

#include <QString>
#include <QList>

#include "ESQLDefinitions.h"

#define  SIGNLEADING 1
#define  FLAGVARYING 1

#define SIGN_SEPARATE 1

class gix_esql_driver;

#line 63 "gix_esql_parser.hh"

# include <cassert>
# include <cstdlib> // std::abort
# include <iostream>
# include <stdexcept>
# include <string>
# include <vector>

#if defined __cplusplus
# define YY_CPLUSPLUS __cplusplus
#else
# define YY_CPLUSPLUS 199711L
#endif

// Support move semantics when possible.
#if 201103L <= YY_CPLUSPLUS
# define YY_MOVE           std::move
# define YY_MOVE_OR_COPY   move
# define YY_MOVE_REF(Type) Type&&
# define YY_RVREF(Type)    Type&&
# define YY_COPY(Type)     Type
#else
# define YY_MOVE
# define YY_MOVE_OR_COPY   copy
# define YY_MOVE_REF(Type) Type&
# define YY_RVREF(Type)    const Type&
# define YY_COPY(Type)     const Type&
#endif

// Support noexcept when possible.
#if 201103L <= YY_CPLUSPLUS
# define YY_NOEXCEPT noexcept
# define YY_NOTHROW
#else
# define YY_NOEXCEPT
# define YY_NOTHROW throw ()
#endif

// Support constexpr when possible.
#if 201703 <= YY_CPLUSPLUS
# define YY_CONSTEXPR constexpr
#else
# define YY_CONSTEXPR
#endif
# include "location.hh"
#include <typeinfo>
#ifndef YY_ASSERT
# include <cassert>
# define YY_ASSERT assert
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

namespace yy {
#line 197 "gix_esql_parser.hh"




  /// A Bison parser.
  class gix_esql_parser
  {
  public:
#ifndef YYSTYPE
  /// A buffer to store and retrieve objects.
  ///
  /// Sort of a variant, but does not keep track of the nature
  /// of the stored data, since that knowledge is available
  /// via the current parser state.
  class semantic_type
  {
  public:
    /// Type of *this.
    typedef semantic_type self_type;

    /// Empty construction.
    semantic_type () YY_NOEXCEPT
      : yybuffer_ ()
      , yytypeid_ (YY_NULLPTR)
    {}

    /// Construct and fill.
    template <typename T>
    semantic_type (YY_RVREF (T) t)
      : yytypeid_ (&typeid (T))
    {
      YY_ASSERT (sizeof (T) <= size);
      new (yyas_<T> ()) T (YY_MOVE (t));
    }

#if 201103L <= YY_CPLUSPLUS
    /// Non copyable.
    semantic_type (const self_type&) = delete;
    /// Non copyable.
    self_type& operator= (const self_type&) = delete;
#endif

    /// Destruction, allowed only if empty.
    ~semantic_type () YY_NOEXCEPT
    {
      YY_ASSERT (!yytypeid_);
    }

# if 201103L <= YY_CPLUSPLUS
    /// Instantiate a \a T in here from \a t.
    template <typename T, typename... U>
    T&
    emplace (U&&... u)
    {
      YY_ASSERT (!yytypeid_);
      YY_ASSERT (sizeof (T) <= size);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T (std::forward <U>(u)...);
    }
# else
    /// Instantiate an empty \a T in here.
    template <typename T>
    T&
    emplace ()
    {
      YY_ASSERT (!yytypeid_);
      YY_ASSERT (sizeof (T) <= size);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T ();
    }

    /// Instantiate a \a T in here from \a t.
    template <typename T>
    T&
    emplace (const T& t)
    {
      YY_ASSERT (!yytypeid_);
      YY_ASSERT (sizeof (T) <= size);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T (t);
    }
# endif

    /// Instantiate an empty \a T in here.
    /// Obsolete, use emplace.
    template <typename T>
    T&
    build ()
    {
      return emplace<T> ();
    }

    /// Instantiate a \a T in here from \a t.
    /// Obsolete, use emplace.
    template <typename T>
    T&
    build (const T& t)
    {
      return emplace<T> (t);
    }

    /// Accessor to a built \a T.
    template <typename T>
    T&
    as () YY_NOEXCEPT
    {
      YY_ASSERT (yytypeid_);
      YY_ASSERT (*yytypeid_ == typeid (T));
      YY_ASSERT (sizeof (T) <= size);
      return *yyas_<T> ();
    }

    /// Const accessor to a built \a T (for %printer).
    template <typename T>
    const T&
    as () const YY_NOEXCEPT
    {
      YY_ASSERT (yytypeid_);
      YY_ASSERT (*yytypeid_ == typeid (T));
      YY_ASSERT (sizeof (T) <= size);
      return *yyas_<T> ();
    }

    /// Swap the content with \a that, of same type.
    ///
    /// Both variants must be built beforehand, because swapping the actual
    /// data requires reading it (with as()), and this is not possible on
    /// unconstructed variants: it would require some dynamic testing, which
    /// should not be the variant's responsibility.
    /// Swapping between built and (possibly) non-built is done with
    /// self_type::move ().
    template <typename T>
    void
    swap (self_type& that) YY_NOEXCEPT
    {
      YY_ASSERT (yytypeid_);
      YY_ASSERT (*yytypeid_ == *that.yytypeid_);
      std::swap (as<T> (), that.as<T> ());
    }

    /// Move the content of \a that to this.
    ///
    /// Destroys \a that.
    template <typename T>
    void
    move (self_type& that)
    {
# if 201103L <= YY_CPLUSPLUS
      emplace<T> (std::move (that.as<T> ()));
# else
      emplace<T> ();
      swap<T> (that);
# endif
      that.destroy<T> ();
    }

# if 201103L <= YY_CPLUSPLUS
    /// Move the content of \a that to this.
    template <typename T>
    void
    move (self_type&& that)
    {
      emplace<T> (std::move (that.as<T> ()));
      that.destroy<T> ();
    }
#endif

    /// Copy the content of \a that to this.
    template <typename T>
    void
    copy (const self_type& that)
    {
      emplace<T> (that.as<T> ());
    }

    /// Destroy the stored \a T.
    template <typename T>
    void
    destroy ()
    {
      as<T> ().~T ();
      yytypeid_ = YY_NULLPTR;
    }

  private:
#if YY_CPLUSPLUS < 201103L
    /// Non copyable.
    semantic_type (const self_type&);
    /// Non copyable.
    self_type& operator= (const self_type&);
#endif

    /// Accessor to raw memory as \a T.
    template <typename T>
    T*
    yyas_ () YY_NOEXCEPT
    {
      void *yyp = yybuffer_.yyraw;
      return static_cast<T*> (yyp);
     }

    /// Const accessor to raw memory as \a T.
    template <typename T>
    const T*
    yyas_ () const YY_NOEXCEPT
    {
      const void *yyp = yybuffer_.yyraw;
      return static_cast<const T*> (yyp);
     }

    /// An auxiliary type to compute the largest semantic type.
    union union_type
    {
      // updatesql
      // update
      // disconnectsql
      // disconnect
      // deletesql
      // delete
      // insertsql
      // insert
      // opensql
      // othersql
      // incfile
      // includesql
      // selectintosql
      // declaresql
      // select
      // token_list
      // declare_cursor
      char dummy1[sizeof (QList<QString> *)];

      // SELECT
      // SELECTFROM
      // TOKEN
      // HOSTTOKEN
      // WORD
      // PICTURE
      // INSERT
      // UPDATE
      // DISCONNECT
      // DELETE
      // EXECUTE
      // OTHERFUNC
      // INTO
      // host_reference
      // expr
      // declare_table
      // declare_special
      char dummy2[sizeof (QString)];

      // WITH_HOLD
      char dummy3[sizeof (int)];

      // NUMERIC
      char dummy4[sizeof (long)];
    };

    /// The size of the largest semantic type.
    enum { size = sizeof (union_type) };

    /// A buffer to store semantic values.
    union
    {
      /// Strongest alignment constraints.
      long double yyalign_me;
      /// A buffer large enough to store any of the semantic values.
      char yyraw[size];
    } yybuffer_;

    /// Whether the content is built: if defined, the name of the stored type.
    const std::type_info *yytypeid_;
  };

#else
    typedef YYSTYPE semantic_type;
#endif
    /// Symbol locations.
    typedef location location_type;

    /// Syntax errors thrown from user actions.
    struct syntax_error : std::runtime_error
    {
      syntax_error (const location_type& l, const std::string& m)
        : std::runtime_error (m)
        , location (l)
      {}

      syntax_error (const syntax_error& s)
        : std::runtime_error (s.what ())
        , location (s.location)
      {}

      ~syntax_error () YY_NOEXCEPT YY_NOTHROW;

      location_type location;
    };

    /// Token kinds.
    struct token
    {
      enum token_kind_type
      {
        TOK_YYEMPTY = -2,
    TOK_YYEOF = 0,                 // "end of file"
    TOK_YYerror = 256,             // error
    TOK_YYUNDEF = 257,             // "invalid token"
    TOK_PERIOD = 258,              // "."
    TOK_SELECT = 259,              // SELECT
    TOK_SELECTFROM = 260,          // SELECTFROM
    TOK_TOKEN = 261,               // TOKEN
    TOK_HOSTTOKEN = 262,           // HOSTTOKEN
    TOK_WORD = 263,                // WORD
    TOK_PICTURE = 264,             // PICTURE
    TOK_INSERT = 265,              // INSERT
    TOK_UPDATE = 266,              // UPDATE
    TOK_DISCONNECT = 267,          // DISCONNECT
    TOK_CONNECT_RESET = 268,       // CONNECT_RESET
    TOK_DELETE = 269,              // DELETE
    TOK_EXECUTE = 270,             // EXECUTE
    TOK_OTHERFUNC = 271,           // OTHERFUNC
    TOK_INTO = 272,                // INTO
    TOK_NUMERIC = 273,             // NUMERIC
    TOK_END_EXEC = 274,            // END_EXEC
    TOK_EXECSQL = 275,             // EXECSQL
    TOK_EXECSQL_INCLUDE = 276,     // EXECSQL_INCLUDE
    TOK_FROM = 277,                // FROM
    TOK_DECLARE = 278,             // DECLARE
    TOK_CURSOR = 279,              // CURSOR
    TOK_FOR = 280,                 // FOR
    TOK_WORKINGBEGIN = 281,        // WORKINGBEGIN
    TOK_WORKINGEND = 282,          // WORKINGEND
    TOK_LINKAGEBEGIN = 283,        // LINKAGEBEGIN
    TOK_LINKAGEEND = 284,          // LINKAGEEND
    TOK_FILEBEGIN = 285,           // FILEBEGIN
    TOK_FILEEND = 286,             // FILEEND
    TOK_PROCEDURE_DIVISION = 287,  // PROCEDURE_DIVISION
    TOK_HOSTVARIANTBEGIN = 288,    // HOSTVARIANTBEGIN
    TOK_HOSTVARIANTEND = 289,      // HOSTVARIANTEND
    TOK_INCLUDE_FILE = 290,        // INCLUDE_FILE
    TOK_INCLUDE_SQLCA = 291,       // INCLUDE_SQLCA
    TOK_SQLCA = 292,               // SQLCA
    TOK_IDENTIFIED_BY = 293,       // IDENTIFIED_BY
    TOK_COMMIT_WORK = 294,         // COMMIT_WORK
    TOK_ROLLBACK_WORK = 295,       // ROLLBACK_WORK
    TOK_CONNECT = 296,             // CONNECT
    TOK_USING = 297,               // USING
    TOK_OPEN = 298,                // OPEN
    TOK_CLOSE = 299,               // CLOSE
    TOK_FETCH = 300,               // FETCH
    TOK_TRAILING = 301,            // TRAILING
    TOK_COMP_1 = 302,              // COMP_1
    TOK_COMP_2 = 303,              // COMP_2
    TOK_COMP_3 = 304,              // COMP_3
    TOK_COMP = 305,                // COMP
    TOK_BINARY = 306,              // BINARY
    TOK_USAGE = 307,               // USAGE
    TOK_SIGN = 308,                // SIGN
    TOK_LEADING = 309,             // LEADING
    TOK_SEPARATE = 310,            // SEPARATE
    TOK_IS = 311,                  // IS
    TOK_ARE = 312,                 // ARE
    TOK_VALUE = 313,               // VALUE
    TOK_ALL = 314,                 // ALL
    TOK_OCCURS = 315,              // OCCURS
    TOK_EXTERNAL = 316,            // EXTERNAL
    TOK_TIMES = 317,               // TIMES
    TOK_CONST = 318,               // CONST
    TOK_CONNECT_TO = 319,          // CONNECT_TO
    TOK_USER = 320,                // USER
    TOK_TABLE = 321,               // TABLE
    TOK_TO = 322,                  // TO
    TOK_BEGIN_DECLARE_SPECIAL = 323, // BEGIN_DECLARE_SPECIAL
    TOK_COPY = 324,                // COPY
    TOK_COPY_FILE = 325,           // COPY_FILE
    TOK_WITH_HOLD = 326,           // WITH_HOLD
    TOK_WHERE_CURRENT_OF = 327     // WHERE_CURRENT_OF
      };
      /// Backward compatibility alias (Bison 3.6).
      typedef token_kind_type yytokentype;
    };

    /// Token kind, as returned by yylex.
    typedef token::yytokentype token_kind_type;

    /// Backward compatibility alias (Bison 3.6).
    typedef token_kind_type token_type;

    /// Symbol kinds.
    struct symbol_kind
    {
      enum symbol_kind_type
      {
        YYNTOKENS = 73, ///< Number of tokens.
        S_YYEMPTY = -2,
        S_YYEOF = 0,                             // "end of file"
        S_YYerror = 1,                           // error
        S_YYUNDEF = 2,                           // "invalid token"
        S_PERIOD = 3,                            // "."
        S_SELECT = 4,                            // SELECT
        S_SELECTFROM = 5,                        // SELECTFROM
        S_TOKEN = 6,                             // TOKEN
        S_HOSTTOKEN = 7,                         // HOSTTOKEN
        S_WORD = 8,                              // WORD
        S_PICTURE = 9,                           // PICTURE
        S_INSERT = 10,                           // INSERT
        S_UPDATE = 11,                           // UPDATE
        S_DISCONNECT = 12,                       // DISCONNECT
        S_CONNECT_RESET = 13,                    // CONNECT_RESET
        S_DELETE = 14,                           // DELETE
        S_EXECUTE = 15,                          // EXECUTE
        S_OTHERFUNC = 16,                        // OTHERFUNC
        S_INTO = 17,                             // INTO
        S_NUMERIC = 18,                          // NUMERIC
        S_END_EXEC = 19,                         // END_EXEC
        S_EXECSQL = 20,                          // EXECSQL
        S_EXECSQL_INCLUDE = 21,                  // EXECSQL_INCLUDE
        S_FROM = 22,                             // FROM
        S_DECLARE = 23,                          // DECLARE
        S_CURSOR = 24,                           // CURSOR
        S_FOR = 25,                              // FOR
        S_WORKINGBEGIN = 26,                     // WORKINGBEGIN
        S_WORKINGEND = 27,                       // WORKINGEND
        S_LINKAGEBEGIN = 28,                     // LINKAGEBEGIN
        S_LINKAGEEND = 29,                       // LINKAGEEND
        S_FILEBEGIN = 30,                        // FILEBEGIN
        S_FILEEND = 31,                          // FILEEND
        S_PROCEDURE_DIVISION = 32,               // PROCEDURE_DIVISION
        S_HOSTVARIANTBEGIN = 33,                 // HOSTVARIANTBEGIN
        S_HOSTVARIANTEND = 34,                   // HOSTVARIANTEND
        S_INCLUDE_FILE = 35,                     // INCLUDE_FILE
        S_INCLUDE_SQLCA = 36,                    // INCLUDE_SQLCA
        S_SQLCA = 37,                            // SQLCA
        S_IDENTIFIED_BY = 38,                    // IDENTIFIED_BY
        S_COMMIT_WORK = 39,                      // COMMIT_WORK
        S_ROLLBACK_WORK = 40,                    // ROLLBACK_WORK
        S_CONNECT = 41,                          // CONNECT
        S_USING = 42,                            // USING
        S_OPEN = 43,                             // OPEN
        S_CLOSE = 44,                            // CLOSE
        S_FETCH = 45,                            // FETCH
        S_TRAILING = 46,                         // TRAILING
        S_COMP_1 = 47,                           // COMP_1
        S_COMP_2 = 48,                           // COMP_2
        S_COMP_3 = 49,                           // COMP_3
        S_COMP = 50,                             // COMP
        S_BINARY = 51,                           // BINARY
        S_USAGE = 52,                            // USAGE
        S_SIGN = 53,                             // SIGN
        S_LEADING = 54,                          // LEADING
        S_SEPARATE = 55,                         // SEPARATE
        S_IS = 56,                               // IS
        S_ARE = 57,                              // ARE
        S_VALUE = 58,                            // VALUE
        S_ALL = 59,                              // ALL
        S_OCCURS = 60,                           // OCCURS
        S_EXTERNAL = 61,                         // EXTERNAL
        S_TIMES = 62,                            // TIMES
        S_CONST = 63,                            // CONST
        S_CONNECT_TO = 64,                       // CONNECT_TO
        S_USER = 65,                             // USER
        S_TABLE = 66,                            // TABLE
        S_TO = 67,                               // TO
        S_BEGIN_DECLARE_SPECIAL = 68,            // BEGIN_DECLARE_SPECIAL
        S_COPY = 69,                             // COPY
        S_COPY_FILE = 70,                        // COPY_FILE
        S_WITH_HOLD = 71,                        // WITH_HOLD
        S_WHERE_CURRENT_OF = 72,                 // WHERE_CURRENT_OF
        S_YYACCEPT = 73,                         // $accept
        S_sqlstate_list = 74,                    // sqlstate_list
        S_sqlstate = 75,                         // sqlstate
        S_updatesql = 76,                        // updatesql
        S_update = 77,                           // update
        S_disconnectsql = 78,                    // disconnectsql
        S_disconnect = 79,                       // disconnect
        S_deletesql = 80,                        // deletesql
        S_delete = 81,                           // delete
        S_insertsql = 82,                        // insertsql
        S_insert = 83,                           // insert
        S_rollbacksql = 84,                      // rollbacksql
        S_commitsql = 85,                        // commitsql
        S_fetchsql = 86,                         // fetchsql
        S_fetch = 87,                            // fetch
        S_host_references = 88,                  // host_references
        S_res_host_references = 89,              // res_host_references
        S_closesql = 90,                         // closesql
        S_opensql = 91,                          // opensql
        S_connectsql = 92,                       // connectsql
        S_connect_to = 93,                       // connect_to
        S_resetsql = 94,                         // resetsql
        S_othersql = 95,                         // othersql
        S_connect = 96,                          // connect
        S_identified = 97,                       // identified
        S_using = 98,                            // using
        S_incfile = 99,                          // incfile
        S_includesql = 100,                      // includesql
        S_selectintosql = 101,                   // selectintosql
        S_declaresql = 102,                      // declaresql
        S_select = 103,                          // select
        S_declare_for = 104,                     // declare_for
        S_token_list = 105,                      // token_list
        S_host_reference = 106,                  // host_reference
        S_expr = 107,                            // expr
        S_sqlvariantstates = 108,                // sqlvariantstates
        S_109_1 = 109,                           // $@1
        S_110_2 = 110,                           // $@2
        S_111_3 = 111,                           // $@3
        S_sqlvariantstate_list = 112,            // sqlvariantstate_list
        S_declare_cursor = 113,                  // declare_cursor
        S_declare_table = 114,                   // declare_table
        S_declare_special = 115,                 // declare_special
        S_sqlvariantstate = 116,                 // sqlvariantstate
        S_117_4 = 117,                           // $@4
        S_118_5 = 118,                           // $@5
        S_data_description_clause_sequence = 119, // data_description_clause_sequence
        S_data_description_clause = 120,         // data_description_clause
        S_picture_clause = 121,                  // picture_clause
        S_usage_clause = 122,                    // usage_clause
        S_usage = 123,                           // usage
        S_value_clause = 124,                    // value_clause
        S_const_clause = 125,                    // const_clause
        S_sign_clause = 126,                     // sign_clause
        S__sign_is = 127,                        // _sign_is
        S_flag_separate = 128,                   // flag_separate
        S_occurs_clause = 129,                   // occurs_clause
        S_external_clause = 130,                 // external_clause
        S__is = 131,                             // _is
        S__is_are = 132,                         // _is_are
        S__all = 133,                            // _all
        S__times = 134                           // _times
      };
    };

    /// (Internal) symbol kind.
    typedef symbol_kind::symbol_kind_type symbol_kind_type;

    /// The number of tokens.
    static const symbol_kind_type YYNTOKENS = symbol_kind::YYNTOKENS;

    /// A complete symbol.
    ///
    /// Expects its Base type to provide access to the symbol kind
    /// via kind ().
    ///
    /// Provide access to semantic value and location.
    template <typename Base>
    struct basic_symbol : Base
    {
      /// Alias to Base.
      typedef Base super_type;

      /// Default constructor.
      basic_symbol ()
        : value ()
        , location ()
      {}

#if 201103L <= YY_CPLUSPLUS
      /// Move constructor.
      basic_symbol (basic_symbol&& that)
        : Base (std::move (that))
        , value ()
        , location (std::move (that.location))
      {
        switch (this->kind ())
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
        value.move< QList<QString> * > (std::move (that.value));
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
        value.move< QString > (std::move (that.value));
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
        value.move< int > (std::move (that.value));
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.move< long > (std::move (that.value));
        break;

      default:
        break;
    }

      }
#endif

      /// Copy constructor.
      basic_symbol (const basic_symbol& that);

      /// Constructors for typed symbols.
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, location_type&& l)
        : Base (t)
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const location_type& l)
        : Base (t)
        , location (l)
      {}
#endif

#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, QList<QString> *&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const QList<QString> *& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif

#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, QString&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const QString& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif

#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, int&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const int& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif

#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, long&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const long& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif

      /// Destroy the symbol.
      ~basic_symbol ()
      {
        clear ();
      }

      /// Destroy contents, and record that is empty.
      void clear ()
      {
        // User destructor.
        symbol_kind_type yykind = this->kind ();
        basic_symbol<Base>& yysym = *this;
        (void) yysym;
        switch (yykind)
        {
       default:
          break;
        }

        // Value type destructor.
switch (yykind)
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
        value.template destroy< QList<QString> * > ();
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
        value.template destroy< QString > ();
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
        value.template destroy< int > ();
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.template destroy< long > ();
        break;

      default:
        break;
    }

        Base::clear ();
      }

      /// The user-facing name of this symbol.
      std::string name () const YY_NOEXCEPT
      {
        return gix_esql_parser::symbol_name (this->kind ());
      }

      /// Backward compatibility (Bison 3.6).
      symbol_kind_type type_get () const YY_NOEXCEPT;

      /// Whether empty.
      bool empty () const YY_NOEXCEPT;

      /// Destructive move, \a s is emptied into this.
      void move (basic_symbol& s);

      /// The semantic value.
      semantic_type value;

      /// The location.
      location_type location;

    private:
#if YY_CPLUSPLUS < 201103L
      /// Assignment operator.
      basic_symbol& operator= (const basic_symbol& that);
#endif
    };

    /// Type access provider for token (enum) based symbols.
    struct by_kind
    {
      /// Default constructor.
      by_kind ();

#if 201103L <= YY_CPLUSPLUS
      /// Move constructor.
      by_kind (by_kind&& that);
#endif

      /// Copy constructor.
      by_kind (const by_kind& that);

      /// The symbol kind as needed by the constructor.
      typedef token_kind_type kind_type;

      /// Constructor from (external) token numbers.
      by_kind (kind_type t);

      /// Record that this symbol is empty.
      void clear ();

      /// Steal the symbol kind from \a that.
      void move (by_kind& that);

      /// The (internal) type number (corresponding to \a type).
      /// \a empty when empty.
      symbol_kind_type kind () const YY_NOEXCEPT;

      /// Backward compatibility (Bison 3.6).
      symbol_kind_type type_get () const YY_NOEXCEPT;

      /// The symbol kind.
      /// \a S_YYEMPTY when empty.
      symbol_kind_type kind_;
    };

    /// Backward compatibility for a private implementation detail (Bison 3.6).
    typedef by_kind by_type;

    /// "External" symbols: returned by the scanner.
    struct symbol_type : basic_symbol<by_kind>
    {
      /// Superclass.
      typedef basic_symbol<by_kind> super_type;

      /// Empty symbol.
      symbol_type () {}

      /// Constructor for valueless symbols, and symbols from each type.
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, location_type l)
        : super_type(token_type (tok), std::move (l))
#else
      symbol_type (int tok, const location_type& l)
        : super_type(token_type (tok), l)
#endif
      {
        YY_ASSERT (tok == token::TOK_YYEOF
                   || (token::TOK_YYerror <= tok && tok <= token::TOK_PERIOD)
                   || tok == token::TOK_CONNECT_RESET
                   || (token::TOK_END_EXEC <= tok && tok <= token::TOK_COPY_FILE)
                   || tok == token::TOK_WHERE_CURRENT_OF);
      }
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, QString v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
#else
      symbol_type (int tok, const QString& v, const location_type& l)
        : super_type(token_type (tok), v, l)
#endif
      {
        YY_ASSERT ((token::TOK_SELECT <= tok && tok <= token::TOK_DISCONNECT)
                   || (token::TOK_DELETE <= tok && tok <= token::TOK_INTO));
      }
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, int v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
#else
      symbol_type (int tok, const int& v, const location_type& l)
        : super_type(token_type (tok), v, l)
#endif
      {
        YY_ASSERT (tok == token::TOK_WITH_HOLD);
      }
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, long v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
#else
      symbol_type (int tok, const long& v, const location_type& l)
        : super_type(token_type (tok), v, l)
#endif
      {
        YY_ASSERT (tok == token::TOK_NUMERIC);
      }
    };

    /// Build a parser object.
    gix_esql_parser (gix_esql_driver& driver_yyarg);
    virtual ~gix_esql_parser ();

#if 201103L <= YY_CPLUSPLUS
    /// Non copyable.
    gix_esql_parser (const gix_esql_parser&) = delete;
    /// Non copyable.
    gix_esql_parser& operator= (const gix_esql_parser&) = delete;
#endif

    /// Parse.  An alias for parse ().
    /// \returns  0 iff parsing succeeded.
    int operator() ();

    /// Parse.
    /// \returns  0 iff parsing succeeded.
    virtual int parse ();

#if YYDEBUG
    /// The current debugging stream.
    std::ostream& debug_stream () const YY_ATTRIBUTE_PURE;
    /// Set the current debugging stream.
    void set_debug_stream (std::ostream &);

    /// Type for debugging levels.
    typedef int debug_level_type;
    /// The current debugging level.
    debug_level_type debug_level () const YY_ATTRIBUTE_PURE;
    /// Set the current debugging level.
    void set_debug_level (debug_level_type l);
#endif

    /// Report a syntax error.
    /// \param loc    where the syntax error is found.
    /// \param msg    a description of the syntax error.
    virtual void error (const location_type& loc, const std::string& msg);

    /// Report a syntax error.
    void error (const syntax_error& err);

    /// The user-facing name of the symbol whose (internal) number is
    /// YYSYMBOL.  No bounds checking.
    static std::string symbol_name (symbol_kind_type yysymbol);

    // Implementation of make_symbol for each symbol type.
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_YYEOF (location_type l)
      {
        return symbol_type (token::TOK_YYEOF, std::move (l));
      }
#else
      static
      symbol_type
      make_YYEOF (const location_type& l)
      {
        return symbol_type (token::TOK_YYEOF, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_YYerror (location_type l)
      {
        return symbol_type (token::TOK_YYerror, std::move (l));
      }
#else
      static
      symbol_type
      make_YYerror (const location_type& l)
      {
        return symbol_type (token::TOK_YYerror, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_YYUNDEF (location_type l)
      {
        return symbol_type (token::TOK_YYUNDEF, std::move (l));
      }
#else
      static
      symbol_type
      make_YYUNDEF (const location_type& l)
      {
        return symbol_type (token::TOK_YYUNDEF, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PERIOD (location_type l)
      {
        return symbol_type (token::TOK_PERIOD, std::move (l));
      }
#else
      static
      symbol_type
      make_PERIOD (const location_type& l)
      {
        return symbol_type (token::TOK_PERIOD, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SELECT (QString v, location_type l)
      {
        return symbol_type (token::TOK_SELECT, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_SELECT (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_SELECT, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SELECTFROM (QString v, location_type l)
      {
        return symbol_type (token::TOK_SELECTFROM, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_SELECTFROM (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_SELECTFROM, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_TOKEN (QString v, location_type l)
      {
        return symbol_type (token::TOK_TOKEN, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_TOKEN (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_TOKEN, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_HOSTTOKEN (QString v, location_type l)
      {
        return symbol_type (token::TOK_HOSTTOKEN, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_HOSTTOKEN (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_HOSTTOKEN, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_WORD (QString v, location_type l)
      {
        return symbol_type (token::TOK_WORD, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_WORD (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_WORD, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PICTURE (QString v, location_type l)
      {
        return symbol_type (token::TOK_PICTURE, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_PICTURE (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_PICTURE, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INSERT (QString v, location_type l)
      {
        return symbol_type (token::TOK_INSERT, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_INSERT (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_INSERT, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_UPDATE (QString v, location_type l)
      {
        return symbol_type (token::TOK_UPDATE, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_UPDATE (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_UPDATE, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DISCONNECT (QString v, location_type l)
      {
        return symbol_type (token::TOK_DISCONNECT, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_DISCONNECT (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_DISCONNECT, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CONNECT_RESET (location_type l)
      {
        return symbol_type (token::TOK_CONNECT_RESET, std::move (l));
      }
#else
      static
      symbol_type
      make_CONNECT_RESET (const location_type& l)
      {
        return symbol_type (token::TOK_CONNECT_RESET, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DELETE (QString v, location_type l)
      {
        return symbol_type (token::TOK_DELETE, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_DELETE (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_DELETE, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_EXECUTE (QString v, location_type l)
      {
        return symbol_type (token::TOK_EXECUTE, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_EXECUTE (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_EXECUTE, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OTHERFUNC (QString v, location_type l)
      {
        return symbol_type (token::TOK_OTHERFUNC, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_OTHERFUNC (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_OTHERFUNC, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INTO (QString v, location_type l)
      {
        return symbol_type (token::TOK_INTO, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_INTO (const QString& v, const location_type& l)
      {
        return symbol_type (token::TOK_INTO, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_NUMERIC (long v, location_type l)
      {
        return symbol_type (token::TOK_NUMERIC, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_NUMERIC (const long& v, const location_type& l)
      {
        return symbol_type (token::TOK_NUMERIC, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_END_EXEC (location_type l)
      {
        return symbol_type (token::TOK_END_EXEC, std::move (l));
      }
#else
      static
      symbol_type
      make_END_EXEC (const location_type& l)
      {
        return symbol_type (token::TOK_END_EXEC, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_EXECSQL (location_type l)
      {
        return symbol_type (token::TOK_EXECSQL, std::move (l));
      }
#else
      static
      symbol_type
      make_EXECSQL (const location_type& l)
      {
        return symbol_type (token::TOK_EXECSQL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_EXECSQL_INCLUDE (location_type l)
      {
        return symbol_type (token::TOK_EXECSQL_INCLUDE, std::move (l));
      }
#else
      static
      symbol_type
      make_EXECSQL_INCLUDE (const location_type& l)
      {
        return symbol_type (token::TOK_EXECSQL_INCLUDE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_FROM (location_type l)
      {
        return symbol_type (token::TOK_FROM, std::move (l));
      }
#else
      static
      symbol_type
      make_FROM (const location_type& l)
      {
        return symbol_type (token::TOK_FROM, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DECLARE (location_type l)
      {
        return symbol_type (token::TOK_DECLARE, std::move (l));
      }
#else
      static
      symbol_type
      make_DECLARE (const location_type& l)
      {
        return symbol_type (token::TOK_DECLARE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CURSOR (location_type l)
      {
        return symbol_type (token::TOK_CURSOR, std::move (l));
      }
#else
      static
      symbol_type
      make_CURSOR (const location_type& l)
      {
        return symbol_type (token::TOK_CURSOR, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_FOR (location_type l)
      {
        return symbol_type (token::TOK_FOR, std::move (l));
      }
#else
      static
      symbol_type
      make_FOR (const location_type& l)
      {
        return symbol_type (token::TOK_FOR, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_WORKINGBEGIN (location_type l)
      {
        return symbol_type (token::TOK_WORKINGBEGIN, std::move (l));
      }
#else
      static
      symbol_type
      make_WORKINGBEGIN (const location_type& l)
      {
        return symbol_type (token::TOK_WORKINGBEGIN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_WORKINGEND (location_type l)
      {
        return symbol_type (token::TOK_WORKINGEND, std::move (l));
      }
#else
      static
      symbol_type
      make_WORKINGEND (const location_type& l)
      {
        return symbol_type (token::TOK_WORKINGEND, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LINKAGEBEGIN (location_type l)
      {
        return symbol_type (token::TOK_LINKAGEBEGIN, std::move (l));
      }
#else
      static
      symbol_type
      make_LINKAGEBEGIN (const location_type& l)
      {
        return symbol_type (token::TOK_LINKAGEBEGIN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LINKAGEEND (location_type l)
      {
        return symbol_type (token::TOK_LINKAGEEND, std::move (l));
      }
#else
      static
      symbol_type
      make_LINKAGEEND (const location_type& l)
      {
        return symbol_type (token::TOK_LINKAGEEND, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_FILEBEGIN (location_type l)
      {
        return symbol_type (token::TOK_FILEBEGIN, std::move (l));
      }
#else
      static
      symbol_type
      make_FILEBEGIN (const location_type& l)
      {
        return symbol_type (token::TOK_FILEBEGIN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_FILEEND (location_type l)
      {
        return symbol_type (token::TOK_FILEEND, std::move (l));
      }
#else
      static
      symbol_type
      make_FILEEND (const location_type& l)
      {
        return symbol_type (token::TOK_FILEEND, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PROCEDURE_DIVISION (location_type l)
      {
        return symbol_type (token::TOK_PROCEDURE_DIVISION, std::move (l));
      }
#else
      static
      symbol_type
      make_PROCEDURE_DIVISION (const location_type& l)
      {
        return symbol_type (token::TOK_PROCEDURE_DIVISION, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_HOSTVARIANTBEGIN (location_type l)
      {
        return symbol_type (token::TOK_HOSTVARIANTBEGIN, std::move (l));
      }
#else
      static
      symbol_type
      make_HOSTVARIANTBEGIN (const location_type& l)
      {
        return symbol_type (token::TOK_HOSTVARIANTBEGIN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_HOSTVARIANTEND (location_type l)
      {
        return symbol_type (token::TOK_HOSTVARIANTEND, std::move (l));
      }
#else
      static
      symbol_type
      make_HOSTVARIANTEND (const location_type& l)
      {
        return symbol_type (token::TOK_HOSTVARIANTEND, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INCLUDE_FILE (location_type l)
      {
        return symbol_type (token::TOK_INCLUDE_FILE, std::move (l));
      }
#else
      static
      symbol_type
      make_INCLUDE_FILE (const location_type& l)
      {
        return symbol_type (token::TOK_INCLUDE_FILE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INCLUDE_SQLCA (location_type l)
      {
        return symbol_type (token::TOK_INCLUDE_SQLCA, std::move (l));
      }
#else
      static
      symbol_type
      make_INCLUDE_SQLCA (const location_type& l)
      {
        return symbol_type (token::TOK_INCLUDE_SQLCA, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SQLCA (location_type l)
      {
        return symbol_type (token::TOK_SQLCA, std::move (l));
      }
#else
      static
      symbol_type
      make_SQLCA (const location_type& l)
      {
        return symbol_type (token::TOK_SQLCA, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_IDENTIFIED_BY (location_type l)
      {
        return symbol_type (token::TOK_IDENTIFIED_BY, std::move (l));
      }
#else
      static
      symbol_type
      make_IDENTIFIED_BY (const location_type& l)
      {
        return symbol_type (token::TOK_IDENTIFIED_BY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COMMIT_WORK (location_type l)
      {
        return symbol_type (token::TOK_COMMIT_WORK, std::move (l));
      }
#else
      static
      symbol_type
      make_COMMIT_WORK (const location_type& l)
      {
        return symbol_type (token::TOK_COMMIT_WORK, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_ROLLBACK_WORK (location_type l)
      {
        return symbol_type (token::TOK_ROLLBACK_WORK, std::move (l));
      }
#else
      static
      symbol_type
      make_ROLLBACK_WORK (const location_type& l)
      {
        return symbol_type (token::TOK_ROLLBACK_WORK, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CONNECT (location_type l)
      {
        return symbol_type (token::TOK_CONNECT, std::move (l));
      }
#else
      static
      symbol_type
      make_CONNECT (const location_type& l)
      {
        return symbol_type (token::TOK_CONNECT, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_USING (location_type l)
      {
        return symbol_type (token::TOK_USING, std::move (l));
      }
#else
      static
      symbol_type
      make_USING (const location_type& l)
      {
        return symbol_type (token::TOK_USING, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OPEN (location_type l)
      {
        return symbol_type (token::TOK_OPEN, std::move (l));
      }
#else
      static
      symbol_type
      make_OPEN (const location_type& l)
      {
        return symbol_type (token::TOK_OPEN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CLOSE (location_type l)
      {
        return symbol_type (token::TOK_CLOSE, std::move (l));
      }
#else
      static
      symbol_type
      make_CLOSE (const location_type& l)
      {
        return symbol_type (token::TOK_CLOSE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_FETCH (location_type l)
      {
        return symbol_type (token::TOK_FETCH, std::move (l));
      }
#else
      static
      symbol_type
      make_FETCH (const location_type& l)
      {
        return symbol_type (token::TOK_FETCH, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_TRAILING (location_type l)
      {
        return symbol_type (token::TOK_TRAILING, std::move (l));
      }
#else
      static
      symbol_type
      make_TRAILING (const location_type& l)
      {
        return symbol_type (token::TOK_TRAILING, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COMP_1 (location_type l)
      {
        return symbol_type (token::TOK_COMP_1, std::move (l));
      }
#else
      static
      symbol_type
      make_COMP_1 (const location_type& l)
      {
        return symbol_type (token::TOK_COMP_1, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COMP_2 (location_type l)
      {
        return symbol_type (token::TOK_COMP_2, std::move (l));
      }
#else
      static
      symbol_type
      make_COMP_2 (const location_type& l)
      {
        return symbol_type (token::TOK_COMP_2, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COMP_3 (location_type l)
      {
        return symbol_type (token::TOK_COMP_3, std::move (l));
      }
#else
      static
      symbol_type
      make_COMP_3 (const location_type& l)
      {
        return symbol_type (token::TOK_COMP_3, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COMP (location_type l)
      {
        return symbol_type (token::TOK_COMP, std::move (l));
      }
#else
      static
      symbol_type
      make_COMP (const location_type& l)
      {
        return symbol_type (token::TOK_COMP, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_BINARY (location_type l)
      {
        return symbol_type (token::TOK_BINARY, std::move (l));
      }
#else
      static
      symbol_type
      make_BINARY (const location_type& l)
      {
        return symbol_type (token::TOK_BINARY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_USAGE (location_type l)
      {
        return symbol_type (token::TOK_USAGE, std::move (l));
      }
#else
      static
      symbol_type
      make_USAGE (const location_type& l)
      {
        return symbol_type (token::TOK_USAGE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SIGN (location_type l)
      {
        return symbol_type (token::TOK_SIGN, std::move (l));
      }
#else
      static
      symbol_type
      make_SIGN (const location_type& l)
      {
        return symbol_type (token::TOK_SIGN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LEADING (location_type l)
      {
        return symbol_type (token::TOK_LEADING, std::move (l));
      }
#else
      static
      symbol_type
      make_LEADING (const location_type& l)
      {
        return symbol_type (token::TOK_LEADING, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SEPARATE (location_type l)
      {
        return symbol_type (token::TOK_SEPARATE, std::move (l));
      }
#else
      static
      symbol_type
      make_SEPARATE (const location_type& l)
      {
        return symbol_type (token::TOK_SEPARATE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_IS (location_type l)
      {
        return symbol_type (token::TOK_IS, std::move (l));
      }
#else
      static
      symbol_type
      make_IS (const location_type& l)
      {
        return symbol_type (token::TOK_IS, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_ARE (location_type l)
      {
        return symbol_type (token::TOK_ARE, std::move (l));
      }
#else
      static
      symbol_type
      make_ARE (const location_type& l)
      {
        return symbol_type (token::TOK_ARE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_VALUE (location_type l)
      {
        return symbol_type (token::TOK_VALUE, std::move (l));
      }
#else
      static
      symbol_type
      make_VALUE (const location_type& l)
      {
        return symbol_type (token::TOK_VALUE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_ALL (location_type l)
      {
        return symbol_type (token::TOK_ALL, std::move (l));
      }
#else
      static
      symbol_type
      make_ALL (const location_type& l)
      {
        return symbol_type (token::TOK_ALL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OCCURS (location_type l)
      {
        return symbol_type (token::TOK_OCCURS, std::move (l));
      }
#else
      static
      symbol_type
      make_OCCURS (const location_type& l)
      {
        return symbol_type (token::TOK_OCCURS, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_EXTERNAL (location_type l)
      {
        return symbol_type (token::TOK_EXTERNAL, std::move (l));
      }
#else
      static
      symbol_type
      make_EXTERNAL (const location_type& l)
      {
        return symbol_type (token::TOK_EXTERNAL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_TIMES (location_type l)
      {
        return symbol_type (token::TOK_TIMES, std::move (l));
      }
#else
      static
      symbol_type
      make_TIMES (const location_type& l)
      {
        return symbol_type (token::TOK_TIMES, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CONST (location_type l)
      {
        return symbol_type (token::TOK_CONST, std::move (l));
      }
#else
      static
      symbol_type
      make_CONST (const location_type& l)
      {
        return symbol_type (token::TOK_CONST, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CONNECT_TO (location_type l)
      {
        return symbol_type (token::TOK_CONNECT_TO, std::move (l));
      }
#else
      static
      symbol_type
      make_CONNECT_TO (const location_type& l)
      {
        return symbol_type (token::TOK_CONNECT_TO, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_USER (location_type l)
      {
        return symbol_type (token::TOK_USER, std::move (l));
      }
#else
      static
      symbol_type
      make_USER (const location_type& l)
      {
        return symbol_type (token::TOK_USER, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_TABLE (location_type l)
      {
        return symbol_type (token::TOK_TABLE, std::move (l));
      }
#else
      static
      symbol_type
      make_TABLE (const location_type& l)
      {
        return symbol_type (token::TOK_TABLE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_TO (location_type l)
      {
        return symbol_type (token::TOK_TO, std::move (l));
      }
#else
      static
      symbol_type
      make_TO (const location_type& l)
      {
        return symbol_type (token::TOK_TO, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_BEGIN_DECLARE_SPECIAL (location_type l)
      {
        return symbol_type (token::TOK_BEGIN_DECLARE_SPECIAL, std::move (l));
      }
#else
      static
      symbol_type
      make_BEGIN_DECLARE_SPECIAL (const location_type& l)
      {
        return symbol_type (token::TOK_BEGIN_DECLARE_SPECIAL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COPY (location_type l)
      {
        return symbol_type (token::TOK_COPY, std::move (l));
      }
#else
      static
      symbol_type
      make_COPY (const location_type& l)
      {
        return symbol_type (token::TOK_COPY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COPY_FILE (location_type l)
      {
        return symbol_type (token::TOK_COPY_FILE, std::move (l));
      }
#else
      static
      symbol_type
      make_COPY_FILE (const location_type& l)
      {
        return symbol_type (token::TOK_COPY_FILE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_WITH_HOLD (int v, location_type l)
      {
        return symbol_type (token::TOK_WITH_HOLD, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_WITH_HOLD (const int& v, const location_type& l)
      {
        return symbol_type (token::TOK_WITH_HOLD, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_WHERE_CURRENT_OF (location_type l)
      {
        return symbol_type (token::TOK_WHERE_CURRENT_OF, std::move (l));
      }
#else
      static
      symbol_type
      make_WHERE_CURRENT_OF (const location_type& l)
      {
        return symbol_type (token::TOK_WHERE_CURRENT_OF, l);
      }
#endif


    class context
    {
    public:
      context (const gix_esql_parser& yyparser, const symbol_type& yyla);
      const symbol_type& lookahead () const { return yyla_; }
      symbol_kind_type token () const { return yyla_.kind (); }
      const location_type& location () const { return yyla_.location; }

      /// Put in YYARG at most YYARGN of the expected tokens, and return the
      /// number of tokens stored in YYARG.  If YYARG is null, return the
      /// number of expected tokens (guaranteed to be less than YYNTOKENS).
      int expected_tokens (symbol_kind_type yyarg[], int yyargn) const;

    private:
      const gix_esql_parser& yyparser_;
      const symbol_type& yyla_;
    };

  private:
#if YY_CPLUSPLUS < 201103L
    /// Non copyable.
    gix_esql_parser (const gix_esql_parser&);
    /// Non copyable.
    gix_esql_parser& operator= (const gix_esql_parser&);
#endif


    /// Stored state numbers (used for stacks).
    typedef unsigned char state_type;

    /// The arguments of the error message.
    int yy_syntax_error_arguments_ (const context& yyctx,
                                    symbol_kind_type yyarg[], int yyargn) const;

    /// Generate an error message.
    /// \param yyctx     the context in which the error occurred.
    virtual std::string yysyntax_error_ (const context& yyctx) const;
    /// Compute post-reduction state.
    /// \param yystate   the current state
    /// \param yysym     the nonterminal to push on the stack
    static state_type yy_lr_goto_state_ (state_type yystate, int yysym);

    /// Whether the given \c yypact_ value indicates a defaulted state.
    /// \param yyvalue   the value to check
    static bool yy_pact_value_is_default_ (int yyvalue);

    /// Whether the given \c yytable_ value indicates a syntax error.
    /// \param yyvalue   the value to check
    static bool yy_table_value_is_error_ (int yyvalue);

    static const short yypact_ninf_;
    static const signed char yytable_ninf_;

    /// Convert a scanner token kind \a t to a symbol kind.
    /// In theory \a t should be a token_kind_type, but character literals
    /// are valid, yet not members of the token_type enum.
    static symbol_kind_type yytranslate_ (int t);

    /// Convert the symbol name \a n to a form suitable for a diagnostic.
    static std::string yytnamerr_ (const char *yystr);

    /// For a symbol, its name in clear.
    static const char* const yytname_[];


    // Tables.
    // YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
    // STATE-NUM.
    static const short yypact_[];

    // YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
    // Performed when YYTABLE does not specify something else to do.  Zero
    // means the default is an error.
    static const unsigned char yydefact_[];

    // YYPGOTO[NTERM-NUM].
    static const short yypgoto_[];

    // YYDEFGOTO[NTERM-NUM].
    static const short yydefgoto_[];

    // YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
    // positive, shift that token.  If negative, reduce the rule whose
    // number is the opposite.  If YYTABLE_NINF, syntax error.
    static const short yytable_[];

    static const short yycheck_[];

    // YYSTOS[STATE-NUM] -- The (internal number of the) accessing
    // symbol of state STATE-NUM.
    static const unsigned char yystos_[];

    // YYR1[YYN] -- Symbol number of symbol that rule YYN derives.
    static const unsigned char yyr1_[];

    // YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.
    static const signed char yyr2_[];


#if YYDEBUG
    // YYRLINE[YYN] -- Source line where rule number YYN was defined.
    static const short yyrline_[];
    /// Report on the debug stream that the rule \a r is going to be reduced.
    virtual void yy_reduce_print_ (int r) const;
    /// Print the state stack on the debug stream.
    virtual void yy_stack_print_ () const;

    /// Debugging level.
    int yydebug_;
    /// Debug stream.
    std::ostream* yycdebug_;

    /// \brief Display a symbol kind, value and location.
    /// \param yyo    The output stream.
    /// \param yysym  The symbol.
    template <typename Base>
    void yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const;
#endif

    /// \brief Reclaim the memory associated to a symbol.
    /// \param yymsg     Why this token is reclaimed.
    ///                  If null, print nothing.
    /// \param yysym     The symbol.
    template <typename Base>
    void yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const;

  private:
    /// Type access provider for state based symbols.
    struct by_state
    {
      /// Default constructor.
      by_state () YY_NOEXCEPT;

      /// The symbol kind as needed by the constructor.
      typedef state_type kind_type;

      /// Constructor.
      by_state (kind_type s) YY_NOEXCEPT;

      /// Copy constructor.
      by_state (const by_state& that) YY_NOEXCEPT;

      /// Record that this symbol is empty.
      void clear () YY_NOEXCEPT;

      /// Steal the symbol kind from \a that.
      void move (by_state& that);

      /// The symbol kind (corresponding to \a state).
      /// \a symbol_kind::S_YYEMPTY when empty.
      symbol_kind_type kind () const YY_NOEXCEPT;

      /// The state number used to denote an empty symbol.
      /// We use the initial state, as it does not have a value.
      enum { empty_state = 0 };

      /// The state.
      /// \a empty when empty.
      state_type state;
    };

    /// "Internal" symbol: element of the stack.
    struct stack_symbol_type : basic_symbol<by_state>
    {
      /// Superclass.
      typedef basic_symbol<by_state> super_type;
      /// Construct an empty symbol.
      stack_symbol_type ();
      /// Move or copy construction.
      stack_symbol_type (YY_RVREF (stack_symbol_type) that);
      /// Steal the contents from \a sym to build this.
      stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) sym);
#if YY_CPLUSPLUS < 201103L
      /// Assignment, needed by push_back by some old implementations.
      /// Moves the contents of that.
      stack_symbol_type& operator= (stack_symbol_type& that);

      /// Assignment, needed by push_back by other implementations.
      /// Needed by some other old implementations.
      stack_symbol_type& operator= (const stack_symbol_type& that);
#endif
    };

    /// A stack with random access from its top.
    template <typename T, typename S = std::vector<T> >
    class stack
    {
    public:
      // Hide our reversed order.
      typedef typename S::iterator iterator;
      typedef typename S::const_iterator const_iterator;
      typedef typename S::size_type size_type;
      typedef typename std::ptrdiff_t index_type;

      stack (size_type n = 200)
        : seq_ (n)
      {}

#if 201103L <= YY_CPLUSPLUS
      /// Non copyable.
      stack (const stack&) = delete;
      /// Non copyable.
      stack& operator= (const stack&) = delete;
#endif

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      const T&
      operator[] (index_type i) const
      {
        return seq_[size_type (size () - 1 - i)];
      }

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      T&
      operator[] (index_type i)
      {
        return seq_[size_type (size () - 1 - i)];
      }

      /// Steal the contents of \a t.
      ///
      /// Close to move-semantics.
      void
      push (YY_MOVE_REF (T) t)
      {
        seq_.push_back (T ());
        operator[] (0).move (t);
      }

      /// Pop elements from the stack.
      void
      pop (std::ptrdiff_t n = 1) YY_NOEXCEPT
      {
        for (; 0 < n; --n)
          seq_.pop_back ();
      }

      /// Pop all elements from the stack.
      void
      clear () YY_NOEXCEPT
      {
        seq_.clear ();
      }

      /// Number of elements on the stack.
      index_type
      size () const YY_NOEXCEPT
      {
        return index_type (seq_.size ());
      }

      /// Iterator on top of the stack (going downwards).
      const_iterator
      begin () const YY_NOEXCEPT
      {
        return seq_.begin ();
      }

      /// Bottom of the stack.
      const_iterator
      end () const YY_NOEXCEPT
      {
        return seq_.end ();
      }

      /// Present a slice of the top of a stack.
      class slice
      {
      public:
        slice (const stack& stack, index_type range)
          : stack_ (stack)
          , range_ (range)
        {}

        const T&
        operator[] (index_type i) const
        {
          return stack_[range_ - i];
        }

      private:
        const stack& stack_;
        index_type range_;
      };

    private:
#if YY_CPLUSPLUS < 201103L
      /// Non copyable.
      stack (const stack&);
      /// Non copyable.
      stack& operator= (const stack&);
#endif
      /// The wrapped container.
      S seq_;
    };


    /// Stack type.
    typedef stack<stack_symbol_type> stack_type;

    /// The stack.
    stack_type yystack_;

    /// Push a new state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param sym  the symbol
    /// \warning the contents of \a s.value is stolen.
    void yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym);

    /// Push a new look ahead token on the state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param s    the state
    /// \param sym  the symbol (for its value and location).
    /// \warning the contents of \a sym.value is stolen.
    void yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym);

    /// Pop \a n symbols from the stack.
    void yypop_ (int n = 1);

    /// Constants.
    enum
    {
      yylast_ = 254,     ///< Last index in yytable_.
      yynnts_ = 62,  ///< Number of nonterminal symbols.
      yyfinal_ = 2 ///< Termination state number.
    };


    // User arguments.
    gix_esql_driver& driver;

  };

  inline
  gix_esql_parser::symbol_kind_type
  gix_esql_parser::yytranslate_ (int t)
  {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static
    const signed char
    translate_table[] =
    {
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72
    };
    // Last valid token kind.
    const int code_max = 327;

    if (t <= 0)
      return symbol_kind::S_YYEOF;
    else if (t <= code_max)
      return YY_CAST (symbol_kind_type, translate_table[t]);
    else
      return symbol_kind::S_YYUNDEF;
  }

  // basic_symbol.
  template <typename Base>
  gix_esql_parser::basic_symbol<Base>::basic_symbol (const basic_symbol& that)
    : Base (that)
    , value ()
    , location (that.location)
  {
    switch (this->kind ())
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
        value.copy< QList<QString> * > (YY_MOVE (that.value));
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
        value.copy< QString > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
        value.copy< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.copy< long > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

  }



  template <typename Base>
  gix_esql_parser::symbol_kind_type
  gix_esql_parser::basic_symbol<Base>::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }

  template <typename Base>
  bool
  gix_esql_parser::basic_symbol<Base>::empty () const YY_NOEXCEPT
  {
    return this->kind () == symbol_kind::S_YYEMPTY;
  }

  template <typename Base>
  void
  gix_esql_parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move (s);
    switch (this->kind ())
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
        value.move< QList<QString> * > (YY_MOVE (s.value));
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
        value.move< QString > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_WITH_HOLD: // WITH_HOLD
        value.move< int > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_NUMERIC: // NUMERIC
        value.move< long > (YY_MOVE (s.value));
        break;

      default:
        break;
    }

    location = YY_MOVE (s.location);
  }

  // by_kind.
  inline
  gix_esql_parser::by_kind::by_kind ()
    : kind_ (symbol_kind::S_YYEMPTY)
  {}

#if 201103L <= YY_CPLUSPLUS
  inline
  gix_esql_parser::by_kind::by_kind (by_kind&& that)
    : kind_ (that.kind_)
  {
    that.clear ();
  }
#endif

  inline
  gix_esql_parser::by_kind::by_kind (const by_kind& that)
    : kind_ (that.kind_)
  {}

  inline
  gix_esql_parser::by_kind::by_kind (token_kind_type t)
    : kind_ (yytranslate_ (t))
  {}

  inline
  void
  gix_esql_parser::by_kind::clear ()
  {
    kind_ = symbol_kind::S_YYEMPTY;
  }

  inline
  void
  gix_esql_parser::by_kind::move (by_kind& that)
  {
    kind_ = that.kind_;
    that.clear ();
  }

  inline
  gix_esql_parser::symbol_kind_type
  gix_esql_parser::by_kind::kind () const YY_NOEXCEPT
  {
    return kind_;
  }

  inline
  gix_esql_parser::symbol_kind_type
  gix_esql_parser::by_kind::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }

} // yy
#line 2827 "gix_esql_parser.hh"




#endif // !YY_YY_GIX_ESQL_PARSER_HH_INCLUDED
