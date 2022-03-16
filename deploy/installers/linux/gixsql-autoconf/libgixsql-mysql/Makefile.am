## Process this file with automake to generate a Makefile.in

lib_LTLIBRARIES = libgixsql-mysql.la 
libgixsql_mysql_la_SOURCES = DbInterfaceManagerMySQL.cpp  DbInterfaceMySQL.cpp  dblib.cpp  utils.cpp

libgixsql_mysql_la_CXXFLAGS = $(MYSQL_CFLAGS) -I ../libgixsql
libgixsql_mysql_la_LIBADD = $(MYSQL_LIBS)


