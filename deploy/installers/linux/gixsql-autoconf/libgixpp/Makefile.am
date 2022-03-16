## Process this file with automake to generate a Makefile.in

noinst_LIBRARIES = libgixpp.a
libgixpp_a_SOURCES = ESQLCall.cpp  FileData.cpp  GixEsqlLexer.cpp  GixPreProcessor.cpp  ITransformationStep.cpp  MapFileReader.cpp  MapFileWriter.cpp  TPESQLProcessing.cpp  TPSourceConsolidation.cpp gix_esql_driver.cc gix_esql_parser.yy gix_esql_scanner.ll
libgixpp_a_CXXFLAGS = -std=c++17 -I../libcpputils

# sources to be build first (we actually want the bison generated headers)
BUILT_SOURCES = gix_esql_parser.cc

# all generated source files, preventing the need for the user to have bison/flex installed
EXTRA_DIST = gix_esql_parser.cc gix_esql_parser.hh location.hh gix_esql_scanner.cc

AM_YFLAGS = -Wno-yacc -Wall -o gix_esql_parser.cc --defines=gix_esql_parser.hh
AM_LFLAGS = -c++ -v --debug -o "gix_esql_scanner.cc"

MAINTAINERCLEANFILES = gix_esql_parser.cc gix_esql_parser.hh location.hh gix_esql_scanner.cc
