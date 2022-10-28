TEMPLATE = lib
TARGET = gixsql-oracle
win32:TARGET = libgixsql-oracle

INCLUDEPATH += . ../libgixsql ./odpi

CONFIG(debug,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Debug
CONFIG(release,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -D_DEBUG

linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17

linux:QMAKE_LIBS+= 

win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17
win32:QMAKE_LFLAGS_RELEASE+= -O0 -std=c++17 -D_DEBUG

win32:QMAKE_LIBS+= -lpq

win32:DEFINES -= UNICODE _UNICODE

HEADERS += DbInterfaceOracle.h  utils.h

SOURCES += DbInterfaceManagerOracle.cpp   DbInterfaceOracle.cpp   dblib.cpp   utils.cpp
