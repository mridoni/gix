TEMPLATE = lib
CONFIG += staticlib
TARGET = gix-debugger-common
INCLUDEPATH += . .. ../gixsql/libcpputils ../gix-debugger-common

CONFIG(release,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Release
CONFIG(debug,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Debug

linux:QMAKE_LFLAGS_DEBUG += -rdynamic -Wl,--whole-archive
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas

linux:QMAKE_LFLAGS_RELEASE+= -Wl,--whole-archive
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas

win32:QMAKE_LFLAGS_RELEASE+=
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas

win32:DEFINES -= UNICODE _UNICODE

HEADERS += any_type.h DebuggerHostInputMessage.h DebuggerHostMessage.h DebuggerHostOutputMessage.h debugger-msg-defs.h \
           gix-debugger-types.h GixDebuggerSessionConfig.h json11.hpp NetworkManager.h 


SOURCES += any_type.cpp DebuggerHostInputMessage.cpp DebuggerHostMessage.cpp DebuggerHostOutputMessage.cpp gix-debugger-types.cpp \
           GixDebuggerSessionConfig.cpp json11.cpp NetworkManager.cpp 

