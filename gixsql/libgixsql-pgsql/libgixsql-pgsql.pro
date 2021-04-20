TEMPLATE = lib
TARGET = gixsql-pgsql
win32:TARGET = libgixsql-pgsql

INCLUDEPATH += . ../libgixsql /usr/include/postgresql

CONFIG(debug,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Debug
CONFIG(release,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17

linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17

win32:QMAKE_CXXFLAGS_RELEASE+= -I\"$(PGSQL_HOME)\\include\" -DHAVE_STRUCT_TIMESPEC
win32:QMAKE_LFLAGS_RELEASE+= -L\"$(PGSQL_HOME)\\lib\" 
win32:QMAKE_LIBS+= -lpq
win32:DEFINES -= UNICODE _UNICODE

HEADERS += DbInterfacePGSQL.h  utils.h

SOURCES += DbInterfaceManagerPGSQL.cpp   DbInterfacePGSQL.cpp   dblib.cpp   utils.cpp
