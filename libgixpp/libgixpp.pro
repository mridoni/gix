TEMPLATE = lib
CONFIG += staticlib
TARGET = gixpp
INCLUDEPATH += . ../libgixutils

CONFIG(debug,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Debug
CONFIG(release,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic -Wl,--whole-archive
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas

linux:QMAKE_LFLAGS_RELEASE+= -Wl,--whole-archive
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas

win32:QMAKE_LFLAGS_RELEASE+=
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas -I ../build-tools/grammar-tools
win32:QMAKE_LEX = ../../build-tools/grammar-tools/win_flex
win32:DEFINES -= UNICODE _UNICODE

#win32:QMAKE_YACC = ../../build-tools/grammar-tools/win_bison
#win32:QMAKE_YACCFLAGS =  -Wall -o "gix_esql_parser.cc" --defines="gix_esql_parser.hh"
#win32:QMAKE_EXT_YACC = hh

#win32:YACCSOURCES = gix_esql_parser.yy
#win32:LEXSOURCES  = gix_esql_scanner.ll

HEADERS += ESQLCall.h  ESQLDefinitions.h  FileData.h  GixPreProcessor.h  ITransformationStep.h  \
			libgixpp_global.h  libgixpp.h  TPESQLProcessing.h  TPSourceConsolidation.h \
			gix_esql_driver.hh  GixEsqlLexer.hh  gix_esql_parser.hh  location.hh

SOURCES += ESQLCall.cpp  FileData.cpp  GixEsqlLexer.cpp  GixPreProcessor.cpp \
			ITransformationStep.cpp  libgixpp.cpp  TPESQLProcessing.cpp  TPSourceConsolidation.cpp \
			gix_esql_driver.cc gix_esql_parser.cc  gix_esql_scanner.cc	
