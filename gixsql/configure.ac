#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([gixsql], [1.0.15], [m.ridoni@gmail.com])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([1.9.6 -Wall -Werror dist-bzip2])

# Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AM_PROG_AR
AC_PROG_YACC
AC_PROG_LEX

LT_INIT
AC_SUBST([LIBTOOL_DEPS])

# Checks for header files.
AC_CHECK_HEADERS([locale.h malloc.h stdint.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for libraries.
PKG_PROG_PKG_CONFIG

AC_ARG_ENABLE([mysql],
  [AS_HELP_STRING([--disable-mysql], [Enable MySQL support @<:@yes@:>@])])

AC_ARG_ENABLE([odbc],
  [AS_HELP_STRING([--disable-odbc], [Enable ODBC support @<:@yes@:>@])])

AC_ARG_ENABLE([pgsql],
  [AS_HELP_STRING([--disable-pgsql], [Enable PostgreSQL support @<:@yes@:>@])])

AS_IF([test "$enable_mysql" != "no"],
  [PKG_CHECK_MODULES([MYSQL],
    [mysqlclient],
    [enable_mysql=yes],
    [AS_IF([test "$enable_mysql" = "yes"],
      [AC_MSG_ERROR([libmysqlclient required, but not found.])],
      [enable_mysql=no])])])

AS_IF([test "$enable_odbc" != "no"],
  [PKG_CHECK_MODULES([ODBC],
    [odbc],
    [enable_odbc=yes],
    [AS_IF([test "$enable_odbc" = "yes"],
      [AC_MSG_ERROR([libodbc required, but not found.])],
      [enable_odbc=no])])])

AS_IF([test "$enable_pgsql" != "no"],
  [PKG_CHECK_MODULES([LIBPQ],
    [libpq],
    [enable_pgsql=yes],
    [AS_IF([test "$enable_pgsql" = "yes"],
      [AC_MSG_ERROR([libpq required, but not found.])],
      [enable_pgsql=no])])])

AM_CONDITIONAL([ENABLE_MYSQL], [test "$enable_mysql" = "yes"])
AM_CONDITIONAL([ENABLE_ODBC], [test "$enable_odbc" = "yes"])
AM_CONDITIONAL([ENABLE_PGSQL], [test "$enable_pgsql" = "yes"])


# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_CHECK_FUNCS([localeconv memmove memset select strcasecmp strdup strndup strrchr strstr strtoull])

AC_CONFIG_FILES([Makefile
                 libcpputils/Makefile
                 libgixpp/Makefile
                 gixpp/Makefile
                 runtime/libgixsql/Makefile
                 runtime/libgixsql-mysql/Makefile
                 runtime/libgixsql-odbc/Makefile
                 runtime/libgixsql-pgsql/Makefile
])
AC_OUTPUT



