TEMPLATE = lib
CONFIG += staticlib
TARGET = gixpp
INCLUDEPATH += . ../libcpputils

CONFIG(debug,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/../Debug
CONFIG(release,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/../Release

linux:include(flex.pri)
linux:include(bison.pri)
linux:FLEXSOURCES = gix_esql_scanner.ll
linux:BISONSOURCES = gix_esql_parser.yy

win32:include(flex.pri)
win32:include(bison.pri)
win32:FLEXSOURCES = gix_esql_scanner.ll
win32:BISONSOURCES = gix_esql_parser.yy

linux:PRE_TARGETDEPS += compiler_flex_make_all compiler_bison_make_all
win32:PRE_TARGETDEPS += compiler_flex_make_all compiler_bison_make_all

linux:QMAKE_LFLAGS_DEBUG += -rdynamic -Wl,--whole-archive
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas -I ../build-tools/grammar-tools
linux:QMAKE_LFLAGS_RELEASE+= -Wl,--whole-archive
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas -I ../build-tools/grammar-tools

win32:QMAKE_LFLAGS_RELEASE+=
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas -I ../build-tools/grammar-tools
win32:QMAKE_LFLAGS_DEBUG+=
win32:QMAKE_CXXFLAGS_DEBUG+= -O0 -std=c++17 -Wno-unknown-pragmas -I ../build-tools/grammar-tools

win32:QMAKE_LEX = ../build-tools/grammar-tools/win_flex
win32:DEFINES -= UNICODE _UNICODE

#win32:QMAKE_YACC = ../../build-tools/grammar-tools/win_bison
#win32:QMAKE_YACCFLAGS =  -Wall -o "gix_esql_parser.cc" --defines="gix_esql_parser.hh"
#win32:QMAKE_EXT_YACC = hh

#win32:YACCSOURCES = gix_esql_parser.yy
#win32:LEXSOURCES  = gix_esql_scanner.ll

HEADERS += ESQLCall.h ESQLDefinitions.h FileData.h gix_esql_driver.hh GixEsqlLexer.hh GixPreProcessor.h \
			ITransformationStep.h libgixpp_global.h MapFileReader.h MapFileWriter.h \
			TPESQLProcessing.h TPSourceConsolidation.h libgixpp.h



SOURCES += ESQLCall.cpp FileData.cpp gix_esql_driver.cc GixEsqlLexer.cpp GixPreProcessor.cpp \
			ITransformationStep.cpp MapFileReader.cpp MapFileWriter.cpp \
			TPESQLProcessing.cpp TPSourceConsolidation.cpp 






