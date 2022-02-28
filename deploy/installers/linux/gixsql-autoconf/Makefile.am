## Process this file with automake to generate Makefile.in
ACLOCAL_AMFLAGS = -I m4
SUBDIRS = libcpputils libgixpp gixpp libgixsql
EXTRA_DIST =
CLEANFILES = *~

if ENABLE_MYSQL
SUBDIRS += libgixsql-mysql
endif

if ENABLE_ODBC
SUBDIRS += libgixsql-odbc
endif

if ENABLE_PGSQL
SUBDIRS += libgixsql-pgsql
endif

pkgconfigdir = $(prefix)/lib/copy
pkgconfig_DATA = copy/SQLCA.cpy
