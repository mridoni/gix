## Process this file with automake to generate a Makefile.in

noinst_LIBRARIES = libgixpp.a
libgixpp_a_SOURCES = ESQLCall.cpp  FileData.cpp  GixEsqlLexer.cpp  GixPreProcessor.cpp  ITransformationStep.cpp  MapFileReader.cpp  MapFileWriter.cpp  TPESQLProcessing.cpp  TPSourceConsolidation.cpp gix_esql_driver.cc gix_esql_parser.yy gix_esql_scanner.ll
libgixpp_a_CXXFLAGS = -std=c++17 -I../libcpputils

BUILT_SOURCES = gix_esql_parser.cc gix_esql_parser.hh gix_esql_scanner.cc
AM_YFLAGS = -Wno-yacc -Wall -o gix_esql_parser.cc --defines=gix_esql_parser.hh
AM_LFLAGS = -c++ -v --debug -o "gix_esql_scanner.cc"

CLEANFILES = gix_esql_parser.cc gix_esql_parser.hh gix_esql_scanner.cc location.hh
