## Process this file with automake to generate a Makefile.in

bin_PROGRAMS = gixpp
gixpp_SOURCES = main.cpp popl.hpp
gixpp_CXXFLAGS = -std=c++17 -I.. -I$(top_srcdir)/libcpputils -I$(top_srcdir)/libgixpp -I$(top_srcdir)/build-tools/grammar-tools
gixpp_LDFLAGS =
gixpp_LDADD = ../libcpputils/libcpputils.a ../libgixpp/libgixpp.a

#install-exec-hook:
#	cp $(top_srcdir)/misc/gixsql-wrapper $(prefix)/bin/gixsql && \
#		chmod 755 $(prefix)/bin/gixsql

install-exec-hook:
	install -c -m 755 $(top_srcdir)/misc/gixsql-wrapper $(DESTDIR)$(bindir)/ && \
		mv $(DESTDIR)$(bindir)/gixsql-wrapper $(DESTDIR)$(bindir)/gixsql

uninstall-hook:
	rm -f $(DESTDIR)$(bindir)/gixsql
