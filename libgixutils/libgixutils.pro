TEMPLATE = lib
CONFIG += staticlib
TARGET = gixutils
INCLUDEPATH += . 

CONFIG(debug,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Debug
CONFIG(release,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic -Wl,--whole-archive
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17
linux:DEFINES += _DEBUG

linux:QMAKE_LFLAGS_RELEASE+= -Wl,--whole-archive
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17

win32:QMAKE_LFLAGS_RELEASE+= -Wl,--whole-archive
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17
win32:DEFINES -= UNICODE _UNICODE

HEADERS += CobolUtils.h  CopyResolver.h  libgixutils_global.h  MapFileReader.h \
				MapFileReaderResult.h  MapFileWriter.h  PathUtils.h  SysUtils.h


SOURCES += CobolUtils.cpp  CopyResolver.cpp  MapFileReader.cpp  MapFileWriter.cpp  PathUtils.cpp
