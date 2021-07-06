TEMPLATE = lib
TARGET = gixsql-odbc
win32:TARGET = libgixsql-odbc

INCLUDEPATH += . ../libgixsql

CONFIG(debug,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Debug
CONFIG(release,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17

linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17

linux:QMAKE_LIBS+= -lodbc

win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -D_WIN32

win32:QMAKE_LIBS+= -lodbc32 -lodbccp32

win32:DEFINES -= UNICODE _UNICODE

HEADERS += DbInterfaceODBC.h  utils.h

SOURCES += DbInterfaceManagerODBC.cpp   DbInterfaceODBC.cpp   dblib.cpp   utils.cpp
