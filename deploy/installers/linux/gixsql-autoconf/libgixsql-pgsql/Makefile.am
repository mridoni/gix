## Process this file with automake to generate a Makefile.in

lib_LTLIBRARIES = libgixsql-pgsql.la 
libgixsql_pgsql_la_SOURCES = DbInterfaceManagerPGSQL.cpp  DbInterfacePGSQL.cpp  dblib.cpp  utils.cpp

libgixsql_pgsql_la_CXXFLAGS = $(LIBPQ_CFLAGS) -I ../libgixsql
libgixsql_pgsql_la_LIBADD = $(LIBPQ_LIBS)


