## Process this file with automake to generate a Makefile.in

lib_LTLIBRARIES = libgixsql-mysql.la 
libgixsql_mysql_la_SOURCES = DbInterfaceManagerMySQL.cpp  DbInterfaceMySQL.cpp  dblib.cpp  utils.cpp DbInterfaceMySQL.h utils.h

libgixsql_mysql_la_CXXFLAGS = $(MYSQL_CFLAGS) $(MARIADB_CFLAGS) -I$(top_srcdir)/runtime/libgixsql -std=c++17 -DSPDLOG_FMT_EXTERNAL 
libgixsql_mysql_la_LIBADD = $(MYSQL_LIBS) $(MARIADB_LIBS)
libgixsql_mysql_la_LDFLAGS = -lfmt -lspdlog -no-undefined -avoid-version


