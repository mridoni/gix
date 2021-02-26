TEMPLATE = lib
TARGET = libgixsql
INCLUDEPATH += .

CONFIG(debug,debug|release) DESTDIR = ../../x64/Debug
CONFIG(release,debug|release) DESTDIR = ../../x64/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0

linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O2 -O3
linux:QMAKE_CC=gcc-8
linux:QMAKE_CXX=g++-8

HEADERS += Connection.h \
           ConnectionManager.h \
           ConnectionString.h \
           Cursor.h \
           CursorManager.h \
           DbInterfaceFactory.h \
           gixsql.h \
           IConnection.h \
           IConnectionString.h \
           ICursor.h \
           IDbInterface.h \
           ILogger.h \
           ISchemaManager.h \
           Logger.h \
           platform.h \
           sqlca.h \
           SqlVar.h \
           SqlVarList.h \
           utils.h

SOURCES += Connection.cpp \
           ConnectionManager.cpp \
           ConnectionString.cpp \
           Cursor.cpp \
           CursorManager.cpp \
           DbInterface.cpp \
           DbInterfaceFactory.cpp \
           dllmain.cpp \
           gixsql.cpp \
           Logger.cpp \
           platform.cpp \
           SqlVar.cpp \
           SqlVarList.cpp \
           utils.cpp
