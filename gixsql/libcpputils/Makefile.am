## Process this file with automake to generate a Makefile.in
##AM_CPPFLAGS = @GTK_CFLAGS@
##AM_LDADD = @GTK_LIBS@

noinst_LIBRARIES = libcpputils.a
libcpputils_a_SOURCES = CopyResolver.cpp libcpputils.cpp CopyResolver.h ErrorData.h libcpputils.h \
	linq/linq_cursor.hpp linq/linq_groupby.hpp linq/linq.hpp linq/linq_iterators.hpp linq/linq_last.hpp linq/linq_select.hpp linq/linq_selectmany.hpp linq/linq_skip.hpp linq/linq_take.hpp linq/linq_where.hpp linq/util.hpp

libcpputils_a_CXXFLAGS = -std=c++17

include_HEADERS = CopyResolver.h ErrorData.h libcpputils.h
