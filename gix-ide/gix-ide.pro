TEMPLATE = app
TARGET = gix-ide

CONFIG(release,debug|release) DESTDIR = ../x64/Release
CONFIG(debug,debug|release) DESTDIR = ../x64/Debug

QT += core xml network gui xmlpatterns uitools widgets
CONFIG += c++17
INCLUDEPATH += . ./GeneratedFiles ./GeneratedFiles/build ../gix-common ../libs/scintilla/include ../gixsql/libgixsql

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0

linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O2 -O3

win32:DEFINES += _UNICODE WIN64 QT_DLL QT_NETWORK_LIB QT_UITOOLS_LIB QT_WIDGETS_LIB QT_XML_LIB QT_XMLPATTERNS_LIB _HAS_STD_BYTE=0
win32:INCLUDEPATH += ../libs/scintilla/include ../libs/scintilla/qt/ScintillaEditBase ../libs/scintilla/qt/ScintillaEdit ../gixsql/libgixsql
win32:RC_FILE = gix-ide.rc
win32:LIBPATH += ../libs/scintilla/src/scintilla/bin $(DESTDIR) 
win32:LIBS += -lScintillaEdit4 -lwsock32 -lgix-common -lgixsql

linux:DEFINES += QT_NETWORK_LIB QT_UITOOLS_LIB QT_WIDGETS_LIB QT_XML_LIB QT_XMLPATTERNS_LIB _GCDEBUGGER_NET_DEBUG _DEBUG
linux:INCLUDEPATH += ../libs/scintilla/include ../libs/scintilla/qt/ScintillaEditBase ../libs/scintilla/qt/ScintillaEdit ../gixsql/libgixsql
linux:LIBS += -lScintillaEdit -L$(DESTDIR) -L ../libs/scintilla/src/scintilla/bin/ -lgix-common -lgixsql
linux:QMAKE_CC=gcc-8
linux:QMAKE_CXX=g++-8

macx:LIBS += -F $(HOME)/Projects/scintilla/bin -framework ScintillaEdit -lgix-common -lgixsql
macx:QMAKE_POST_LINK = mkdir -p ../x64/Debug/gix-ide.app/Contents/Frameworks && cp -frvp ../../scintilla/bin/ScintillaEdit.framework ../x64/Debug/gix-ide.app/Contents/Frameworks
macx:INCLUDEPATH += ../gixsql/libgixsql 
macx:LIBPATH += ../libs/scintilla/src/scintilla/bin ../gix-common $(DESTDIR) 

DEPENDPATH += .
MOC_DIR += $$DESTDIR/GeneratedFiles
OBJECTS_DIR += $$DESTDIR/build
UI_DIR += $$DESTDIR/GeneratedFiles
RCC_DIR += $$DESTDIR/GeneratedFiles

include(gix-ide.pri)

gix-ide.path = /opt/gix-ide
gix-ide.files = gix-ide

INSTALLS += gix-ide
