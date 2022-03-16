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

pkgconfigdir = $(prefix)/share/gix/copy
pkgconfig_DATA = copy/SQLCA.cpy

install-exec-hook:
	cp misc/gixsql-wrapper $(prefix)/bin/gixsql && \
		chmod 755 $(prefix)/bin/gixsql
