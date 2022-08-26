TEMPLATE = lib
TARGET = gixsql-sqlite
win32:TARGET = libgixsql-sqlite

INCLUDEPATH += . ../libgixsql /usr/include/postgresql

CONFIG(debug,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Debug
CONFIG(release,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -D_DEBUG

linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17

linux:QMAKE_LIBS+= 

win32:QMAKE_CXXFLAGS_RELEASE+= 
win32:QMAKE_LFLAGS_RELEASE+= 

win32:QMAKE_LIBS+= -lpq

win32:DEFINES -= UNICODE _UNICODE

HEADERS += DbInterfaceSQLite.h  utils.h

SOURCES += DbInterfaceManagerSQLite.cpp   DbInterfaceSQLite.cpp   dblib.cpp   utils.cpp
