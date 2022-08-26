#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([gixsql], [1.0.18], [m.ridoni@gmail.com])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([1.9.6 -Wall -Werror dist-bzip2 subdir-objects])

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

# AC_CANONICAL_HOST is needed to access the 'host_os' variable    
AC_CANONICAL_HOST

build_linux=no
build_windows=no
build_mac=no

# Detect the target system
case "${host_os}" in
    linux*)
        build_linux=yes
        ;;
    cygwin*|mingw*)
        build_windows=yes
        ;;
    darwin*)
        build_mac=yes
        ;;
    *)
        AC_MSG_ERROR(["OS $host_os is not supported"])
        ;;
esac

# Pass the conditionals to automake
AM_CONDITIONAL([BUILD_LINUX], [test "$build_linux" = "yes"])
AM_CONDITIONAL([BUILD_WINDOWS], [test "$build_windows" = "yes"])
AM_CONDITIONAL([BUILD_OSX], [test "$build_mac" = "yes"])

# Checks for libraries.
PKG_PROG_PKG_CONFIG

AC_ARG_ENABLE([mysql],
  [AS_HELP_STRING([--enable-mysql], [Enable MySQL support @<:@yes@:>@])])

AC_ARG_ENABLE([odbc],
  [AS_HELP_STRING([--enable-odbc], [Enable ODBC support @<:@yes@:>@])])

AC_ARG_ENABLE([pgsql],
  [AS_HELP_STRING([--enable-pgsql], [Enable PostgreSQL support @<:@yes@:>@])])
  
AC_ARG_ENABLE([oracle],
  [AS_HELP_STRING([--enable-oracle], [Enable Oracle support @<:@yes@:>@])])

AC_ARG_ENABLE([pgsql],
  [AS_HELP_STRING([--enable-sqlite], [Enable SQLite support @<:@yes@:>@])])  

AC_ARG_WITH([default-driver],
	[AS_HELP_STRING([--with-default-driver[=none|odbc|mysql|pgsql|oracle|sqlite]],
		[set DBMS default-driver])],
		[],
		[with_default_driver=none])

AS_IF([test "$enable_mysql" != "no"],
  [PKG_CHECK_MODULES([MYSQL],
    [mysqlclient],
    [enable_mysql=yes],
    [AS_IF([test "$enable_mysql" = "yes"],
      [AC_MSG_ERROR([libmysqlclient required, but not found.])],
      [enable_mysql=maybenot])])])

AS_IF([test "$enable_mysql" != "no" && test "$enable_mysql" != "yes"],
  [PKG_CHECK_MODULES([MARIADB],
    [libmariadb],
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

# No client packages are needed for Oracle and SQLite,
# since they include their own client libraries

AS_IF([test "$enable_oracle" != "no"], [enable_oracle=yes], [enable_oracle=no])

AS_IF([test "$enable_sqlite" != "no"], [enable_sqlite=yes], [enable_sqlite=no])


# Check if the driver ID is valid
AS_IF([test "$with_default_driver" == "none" || test "$with_default_driver" == "odbc" || test "$with_default_driver" == "mysql" || test "$with_default_driver" == "pgsql" ] || test "$with_default_driver" == "oracle" ] || test "$with_default_driver" == "sqlite" ],
		[],
		[AC_MSG_ERROR([invalid DBMS driver id \"${with_default_driver}\".])]
)


num_drivers=0
cur_driver=
AS_IF([test "$enable_odbc" = "yes"],   [ ((num_drivers++)) ; cur_driver=odbc  ])
AS_IF([test "$enable_mysql" = "yes"],  [ ((num_drivers++)) ; cur_driver=mysql ])
AS_IF([test "$enable_pgsql" = "yes"],  [ ((num_drivers++)) ; cur_driver=pgsql ])
AS_IF([test "$enable_oracle" = "yes"], [ ((num_drivers++)) ; cur_driver=oracle ])
AS_IF([test "$enable_sqlite" = "yes"], [ ((num_drivers++)) ; cur_driver=sqlite ])

AS_IF([test "$num_drivers" = "0" ], [ AC_MSG_ERROR([At least one DBMS driver must be enabled]) ])

AS_IF([test x$with_default_driver != xnone ],
  [
	echo "setting ${with_default_driver} as default driver"

	TMP_HDR=/tmp/default_driver.h
	HDR=${srcdir}/runtime/libgixsql/default_driver.h
	echo "#pragma once" > $TMP_HDR
	echo "" >> $TMP_HDR
	echo "#define GIXSQL_DEFAULT_DRIVER \"${with_default_driver}\"" >> $TMP_HDR
	echo "" >> $TMP_HDR
	diff $TMP_HDR $HDR > /dev/null
	AS_IF([test "$?" != "0"], [ cp $TMP_HDR $HDR ])
  ],
  [
   	AS_IF([test "$num_drivers" = "1"], [
		echo "single-driver configuration detected, setting ${cur_driver} as default driver"

		TMP_HDR=/tmp/default_driver.h
		HDR=${srcdir}/runtime/libgixsql/default_driver.h
		echo "#pragma once" > $TMP_HDR
		echo "" >> $TMP_HDR
		echo "#define GIXSQL_DEFAULT_DRIVER \"${cur_driver}\"" >> $TMP_HDR
		echo "" >> $TMP_HDR
		diff $TMP_HDR $HDR > /dev/null
		AS_IF([test "$?" != "0"], [ cp $TMP_HDR $HDR ])
	])
  ]

)


AM_CONDITIONAL([ENABLE_MYSQL],  [test "$enable_mysql" = "yes"])
AM_CONDITIONAL([ENABLE_ODBC],   [test "$enable_odbc" = "yes"])
AM_CONDITIONAL([ENABLE_PGSQL],  [test "$enable_pgsql" = "yes"])
AM_CONDITIONAL([ENABLE_ORACLE], [test "$enable_oracle" = "yes"])
AM_CONDITIONAL([ENABLE_SQLITE], [test "$enable_sqlite" = "yes"])


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
                 runtime/libgixsql-oracle/Makefile
                 runtime/libgixsql-sqlite/Makefile
])
AC_OUTPUT
