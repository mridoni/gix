## Process this file with automake to generate a Makefile.in

lib_LTLIBRARIES = libgixsql-oracle.la 
libgixsql_oracle_la_SOURCES = DbInterfaceManagerOracle.cpp  DbInterfaceOracle.cpp  dblib.cpp  utils.cpp DbInterfaceOracle.h utils.h \
							 odpi/dpi.h odpi/dpiConn.c odpi/dpiContext.c odpi/dpiData.c odpi/dpiDebug.c odpi/dpiDeqOptions.c odpi/dpiEnqOptions.c \
							 odpi/dpiEnv.c odpi/dpiError.c odpi/dpiErrorMessages.h odpi/dpiGen.c odpi/dpiGlobal.c odpi/dpiHandleList.c \
							 odpi/dpiHandlePool.c odpi/dpiImpl.h odpi/dpiJson.c odpi/dpiLob.c odpi/dpiMsgProps.c odpi/dpiObject.c \
							 odpi/dpiObjectAttr.c odpi/dpiObjectType.c odpi/dpiOci.c odpi/dpiOracleType.c odpi/dpiPool.c odpi/dpiQueue.c \
							 odpi/dpiRowid.c odpi/dpiSodaColl.c odpi/dpiSodaCollCursor.c odpi/dpiSodaDb.c odpi/dpiSodaDoc.c \
							 odpi/dpiSodaDocCursor.c odpi/dpiStmt.c odpi/dpiSubscr.c odpi/dpiUtils.c odpi/dpiVar.c 

libgixsql_oracle_la_CXXFLAGS = -I $(srcdir)/odpi -I$(top_srcdir)/runtime/libgixsql -std=c++17 -DSPDLOG_FMT_EXTERNAL

libgixsql_oracle_la_LIBADD =
libgixsql_oracle_la_LDFLAGS = -lfmt -lspdlog -no-undefined -avoid-version




