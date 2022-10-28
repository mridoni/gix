TEMPLATE = app
TARGET = gix-debugger

CONFIG(release,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Release
CONFIG(debug,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Debug

QT +=
CONFIG += c++17
INCLUDEPATH += . ../gix-debugger-client ../gix-debugger-engine-common ../gix-debugger-common ../gixsql/libcpputils

linux:QMAKE_LFLAGS += "-Wl,-rpath,\'\$$ORIGIN\'"					 
linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas
linux:QMAKE_LFLAGS_RELEASE+= -rdynamic
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas
linux:DEFINES += GIX_DEBUGGER
linux:INCLUDEPATH += /usr/include/libdwarf /usr/include/libelfin ../gix-debugger-engine-common
linux:LIBPATH +=$(DESTDIR) 
linux:LIBS += -ldwarf -ldwarf++ -lelf++ -lcpputils -lgix-debugger-common -lnng -lfmt

win32:DEFINES +=
win32:INCLUDEPATH += /usr/include/libdwarf /usr/include/libelfin ../gix-debugger-engine-common
win32:RC_FILE =
win32:LIBPATH += $(DESTDIR)
win32:LIBS += -ldwarf -ldwarf++ -lelf++ -lcpputils -lgix-debugger-common -lnng 
win32:QMAKE_LFLAGS_DEBUG += -rdynamic
win32:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas
win32:QMAKE_LFLAGS_RELEASE+=
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas
win32:DEFINES -= UNICODE _UNICODE

#macx:LIBS += 
#macx:QMAKE_POST_LINK = mkdir -p ../$$(HOST_PLATFORM)/Debug/gix-ide.app/Contents/Frameworks && cp -frvp ../../scintilla/bin/ScintillaEdit.framework ../$$(HOST_PLATFORM)/Debug/gix-ide.app/Contents/Frameworks
#macx:INCLUDEPATH += ../gixsql/libgixsql 
#macx:LIBPATH += ../libs/scintilla/src/scintilla/bin ../gix-common $(DESTDIR) 

DEPENDPATH += .
MOC_DIR += GeneratedFiles
OBJECTS_DIR += build
UI_DIR += GeneratedFiles
RCC_DIR += GeneratedFiles

HEADERS = ../gix-debugger-engine-common/comp_helper.h ../gix-debugger-engine-common/DwarfSymbolProvider.h \
          ../gix-debugger-engine-common/GixDebugger.h ../gix-debugger-engine-common/SymbolBufferReader.h \
          DbgrCommandHandler.h DebuggerHostDriver.h debugger_host_sink.h gix-debugger_global.h ISymbolProvider.h popl.hpp 

SOURCES = ../gix-debugger-engine-common/comp_helper.cpp ../gix-debugger-engine-common/DwarfSymbolProvider.cpp \
          ../gix-debugger-engine-common/GixDebugger.cpp ../gix-debugger-engine-common/SymbolBufferReader.cpp \
          DbgrCommandHandler.cpp DebuggerHostDriver.cpp main.cpp
          
gix-debugger.path = ${DESTDIR}
gix-debugger.files = $(DESTIDIR)/gix-debugger

win32:HEADERS += ../gix-debugger-engine-common/BufferedStackWalker.h ../gix-debugger-engine-common/CodeviewSymbolProvider.h \
                 ../gix-debugger-engine-common/GixDebuggerWin.h ../gix-debugger-engine-common/StackWalker.hh

win32:SOURCES += ../gix-debugger-engine-common/BufferedStackWalker.cpp ../gix-debugger-engine-common/CodeviewSymbolProvider.cpp \
                 ../gix-debugger-engine-common/GixDebuggerWin.cpp ../gix-debugger-engine-common/StackWalker.cpp
                 

linux:HEADERS += ../gix-debugger-engine-common/GixDebuggerLinux.h ../gix-debugger-engine-common/LinuxProcessRunner.h

linux:SOURCES += ../gix-debugger-engine-common/GixDebuggerLinux.cpp ../gix-debugger-engine-common/LinuxProcessRunner.cpp

INSTALLS += gix-debugger

