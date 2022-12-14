TEMPLATE = app
TARGET = gix-http

CONFIG(release,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Release
CONFIG(debug,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Debug

QT += core xml network gui xmlpatterns concurrent
CONFIG += c++17
INCLUDEPATH += . ./GeneratedFiles ./GeneratedFiles/build ./http ./templateengine ./logging ../build-tools/grammar-tools \
				 ../gix-common ../gix-common/projectsystem ../gix-common/buildsystem ../gix-common/metadata ../gix-debugger-common \
				 ../libgixutils ../gixsql/libgixpp ../gixsql/libcpputils

linux:QMAKE_LFLAGS += "-Wl,-rpath,\'\$$ORIGIN\'"					 
linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas
linux:QMAKE_LFLAGS_RELEASE+= -rdynamic
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas
linux:DEFINES += QT_NETWORK_LIB QT_UITOOLS_LIB QT_WIDGETS_LIB QT_XML_LIB QT_XMLPATTERNS_LIB _GCDEBUGGER_NET_DEBUG _DEBUG
linux:INCLUDEPATH +=
linux:LIBPATH +=$(DESTDIR) 
linux:LIBS += -L$(DESTDIR) -lgix-common -ldl -lfmt -lstdc++fs

win32:DEFINES += QT_DLL QT_NETWORK_LIB QT_UITOOLS_LIB QT_WIDGETS_LIB QT_XML_LIB QT_XMLPATTERNS_LIB _HAS_STD_BYTE=0
win32:INCLUDEPATH +=
win32:RC_FILE =
win32:LIBPATH += $(DESTDIR)
win32:LIBS += -lwsock32 -lgix-common -lgixutils -lz
win32:QMAKE_LFLAGS_DEBUG += -rdynamic
win32:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas
win32:QMAKE_LFLAGS_RELEASE+=
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas
win32:DEFINES -= UNICODE _UNICODE

macx:LIBS += -F $(HOME)/Projects/scintilla/bin -framework ScintillaEdit -lgixutils -lgixixpp -lgix-common -lgixsql
macx:QMAKE_POST_LINK = mkdir -p ../$$(HOST_PLATFORM)/Debug/gix-ide.app/Contents/Frameworks && cp -frvp ../../scintilla/bin/ScintillaEdit.framework ../$$(HOST_PLATFORM)/Debug/gix-ide.app/Contents/Frameworks
macx:INCLUDEPATH += ../gixsql/libgixsql 
macx:LIBPATH += ../libs/scintilla/src/scintilla/bin ../gix-common $(DESTDIR) 

DEPENDPATH += .
MOC_DIR += GeneratedFiles
OBJECTS_DIR += build
UI_DIR += GeneratedFiles
RCC_DIR += GeneratedFiles

include(gix-http.pri)

gix-http.path = ${DESTDIR}
gix-http.files = $(DESTIDIR)/gix-http

INSTALLS += gix-http

