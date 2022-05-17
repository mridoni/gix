## Process this file with automake to generate Makefile.in
ACLOCAL_AMFLAGS = -I m4
SUBDIRS = libcpputils libgixpp gixpp runtime/libgixsql
EXTRA_DIST = copy/SQLCA.cpy misc/gixsql-wrapper
CLEANFILES = *~

if ENABLE_MYSQL
SUBDIRS += runtime/libgixsql-mysql
endif

if ENABLE_ODBC
SUBDIRS += runtime/libgixsql-odbc
endif

if ENABLE_PGSQL
SUBDIRS += runtime/libgixsql-pgsql
endif

pkgconfigdir = $(prefix)/share/gix/copy
pkgconfig_DATA = copy/SQLCA.cpy

