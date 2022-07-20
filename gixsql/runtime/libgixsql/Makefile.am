## Process this file with automake to generate a Makefile.in

lib_LTLIBRARIES = libgixsql.la 
libgixsql_la_SOURCES = Connection.cpp  ConnectionManager.cpp  Cursor.cpp  CursorManager.cpp  DataSourceInfo.cpp  DbInterfaceFactory.cpp \
			dllmain.cpp  gixsql.cpp  Logger.cpp  platform.cpp  SqlVar.cpp  SqlVarList.cpp  utils.cpp \
			Connection.h Cursor.h DataSourceInfo.h gixsql.h ICursor.h IDbInterface.h Logger.h sqlca.h \
			SqlVarList.h ConnectionManager.h CursorManager.h DbInterfaceFactory.h IConnection.h IDataSourceInfo.h \
			IDbManagerInterface.h ISchemaManager.h platform.h SqlVar.h utils.h default_driver.h

libgixsql_la_CXXFLAGS = -std=c++17 -DSPDLOG_FMT_EXTERNAL -I$(top_srcdir)/libgixpp
libgixsql_la_LDFLAGS =  -lfmt -lspdlog -no-undefined -avoid-version
