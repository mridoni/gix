TEMPLATE = app
TARGET = gix-ide

CONFIG(release,debug|release) DESTDIR = ../x64/Release
CONFIG(debug,debug|release) DESTDIR = ../x64/Debug

QT += core xml network gui xmlpatterns uitools widgets
CONFIG +=
INCLUDEPATH += . ./GeneratedFiles ./GeneratedFiles/build \
				 ../gix-common ../gix-common/projectsystem ../gix-common/buildsystem ../gix-common/metadata \
				 ../libgixutils ../libgixpp ../gixsql/libgixsql ../libs/scintilla/include 

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas
linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas
linux:DEFINES += QT_NETWORK_LIB QT_UITOOLS_LIB QT_WIDGETS_LIB QT_XML_LIB QT_XMLPATTERNS_LIB _GCDEBUGGER_NET_DEBUG _DEBUG
linux:INCLUDEPATH += ../libs/scintilla/include ../libs/scintilla/qt/ScintillaEditBase ../libs/scintilla/qt/ScintillaEdit ../gixsql/libgixsql /usr/include/libdwarf
linux:LIBPATH += ../libs/scintilla/src/scintilla/bin $(DESTDIR) 
linux:LIBS += -lScintillaEdit -L$(DESTDIR) -L ../libs/scintilla/src/scintilla/bin/ -lgix-common -lgixsql -ldwarf

win32:DEFINES += _UNICODE WIN64 QT_DLL QT_NETWORK_LIB QT_UITOOLS_LIB QT_WIDGETS_LIB QT_XML_LIB QT_XMLPATTERNS_LIB _HAS_STD_BYTE=0
win32:INCLUDEPATH += ../libs/scintilla/include ../libs/scintilla/qt/ScintillaEditBase ../libs/scintilla/qt/ScintillaEdit ../gixsql/libgixsql  ../build-tools/grammar-tools  ../libs/libdwarf
win32:RC_FILE = gix-ide.rc
win32:LIBPATH += ../libs/scintilla/src/scintilla/bin $(DESTDIR) 
win32:LIBS += -lScintillaEdit4 -lwsock32 -lgix-common -lgixsql -ldwarf -ldbghelp -limagehlp -lgixutils -lz
win32:QMAKE_LFLAGS_DEBUG += -rdynamic -L ../libs/libdwarf
win32:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas
win32:QMAKE_LFLAGS_RELEASE+= -L ../libs/libdwarf
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas

macx:LIBS += -F $(HOME)/Projects/scintilla/bin -framework ScintillaEdit -lgixutils -lgixixpp -lgix-common -lgixsql
macx:QMAKE_POST_LINK = mkdir -p ../x64/Debug/gix-ide.app/Contents/Frameworks && cp -frvp ../../scintilla/bin/ScintillaEdit.framework ../x64/Debug/gix-ide.app/Contents/Frameworks
macx:INCLUDEPATH += ../gixsql/libgixsql 
macx:LIBPATH += ../libs/scintilla/src/scintilla/bin ../gix-common $(DESTDIR) 

DEPENDPATH += .
MOC_DIR += GeneratedFiles
OBJECTS_DIR += build
UI_DIR += GeneratedFiles
RCC_DIR += GeneratedFiles

include(gix-ide.pri)

gix-ide.path = /opt/gix-ide
gix-ide.files = gix-ide

INSTALLS += gix-ide

win32:HEADERS += ./CodeviewSymbolProvider.h ./GixDebuggerWin64.h
win32:SOURCES += ./CodeviewSymbolProvider.cpp ./GixDebuggerWin64.cpp ./imagehlp_funcs.cpp

linux:HEADERS += ./GixDebuggerLinux.h
linux:SOURCES += ./GixDebuggerLinux.cpp