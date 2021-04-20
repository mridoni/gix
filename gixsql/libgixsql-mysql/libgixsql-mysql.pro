TEMPLATE = lib
TARGET = gixsql-mysql
win32:TARGET = libgixsql-mysql

INCLUDEPATH += . ../libgixsql /usr/include/mysql

CONFIG(debug,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Debug
CONFIG(release,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17

linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17

win32:QMAKE_CXXFLAGS_RELEASE+= -I\"$(MYSQL_HOME)\\include\"
win32:QMAKE_LFLAGS_RELEASE+= -L\"$(MYSQL_HOME)\\lib\" 
win32:QMAKE_LIBS+= -lmysql
win32:DEFINES -= UNICODE _UNICODE

HEADERS += DbInterfaceMySQL.h  utils.h

SOURCES += DbInterfaceManagerMySQL.cpp   DbInterfaceMySQL.cpp   dblib.cpp   utils.cpp
