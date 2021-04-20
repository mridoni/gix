TEMPLATE = lib
TARGET = gixsql
win32:TARGET = libgixsql
INCLUDEPATH += .

CONFIG(debug,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Debug
CONFIG(release,debug|release) DESTDIR = ../../$$(HOST_PLATFORM)/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17

linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17

win32:QMAKE_LFLAGS_RELEASE+= 
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -D_HAS_STD_BYTE=0
win32:DEFINES -= UNICODE _UNICODE

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
           DbInterfaceFactory.cpp \
           dllmain.cpp \
           gixsql.cpp \
           Logger.cpp \
           platform.cpp \
           SqlVar.cpp \
           SqlVarList.cpp \
           utils.cpp
