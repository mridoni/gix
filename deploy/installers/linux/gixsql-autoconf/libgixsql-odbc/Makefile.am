## Process this file with automake to generate a Makefile.in

lib_LTLIBRARIES = libgixsql-odbc.la 
libgixsql_odbc_la_SOURCES = DbInterfaceManagerODBC.cpp  DbInterfaceODBC.cpp  dblib.cpp  utils.cpp

libgixsql_odbc_la_CXXFLAGS = $(ODBC_CFLAGS) -I ../libgixsql
libgixsql_odbc_la_LIBADD = $(ODBC_LIBS)


