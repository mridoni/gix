CONFIG(release,debug|release) DESTDIR = $$(HOST_PLATFORM)/Release
CONFIG(debug,debug|release) DESTDIR = $$(HOST_PLATFORM)/Debug

CONFIG += ordered c++17

TEMPLATE = subdirs
SUBDIRS += \
			libgixsql \
			libgixsql-odbc \
			libgixsql-mysql \
			libgixsql-pgsql \
			libgixsql-sqlite \
			libgixsql-oracle \
			libcpputils \
			libgixpp \
			libgixutils \
			gix-common \
			gix-http \
			gixpp \
			scintilla \
			gix-debugger-client \
			gix-debugger-common
			
win32:SUBDIRS += libs/libdwarf/libdwarf	
			
SUBDIRS += gix-ide gix-debugger 

libgixpp.subdir = gixsql/libgixpp			
libcpputils.subdir = gixsql/libcpputils	
libgixutils.subdir = libgixutils	
libgixsql.subdir  = gixsql/runtime/libgixsql
libgixsql-odbc.subdir  = gixsql/runtime/libgixsql-odbc			
libgixsql-mysql.subdir  = gixsql/runtime/libgixsql-mysql			
libgixsql-pgsql.subdir  = gixsql/runtime/libgixsql-pgsql			
libgixsql-sqlite.subdir  = gixsql/runtime/libgixsql-sqlite
libgixsql-oracle.subdir  = gixsql/runtime/libgixsql-oracle	

gix-debugger-client.subdir = gix-debugger-client

gix-debugger-common.subdir = gix-debugger-common

gix-common.subdir  = gix-common			
gix-common.depends = libcpputils libgixutils libgixpp

gix-debugger.subdir = gix-debugger
gix-debugger.depends = gix-debugger-common

libgixpp.depends = libcpputils

scintilla.subdir = libs/scintilla/qt/ScintillaEdit

gix-ide.subdir = gix-ide
gix-ide.depends = libgixpp libgixutils gix-common gix-debugger-client gix-debugger-common libgixsql scintilla

win32:gix-ide.depends += libs/libdwarf/libdwarf
win32:libdwarf.subdir = libs/libdwarf/libdwarf

libgixsql-odbc.depends = libgixsql
libgixsql-mysql.depends = libgixsql
libgixsql-pgsql.depends = libgixsql
libgixsql-sqlite.depends = libgixsql
libgixsql-oracle.depends = libgixsql

gixpp.subdir = gixsql/gixpp
gixpp.depends = libgixpp

unix:GIX_INSTALL_DIR = /opt/gix-ide
win32:GIX_INSTALL_DIR = c:/gix-ide

gix-ide.path = ${DESTDIR}
gix-ide.files = $(DESTIDIR)/gix-ide

gix-debugger.path = ${DESTDIR}
gix-debugger.files = $(DESTIDIR)/gix-debugger

INSTALLS += gix-ide gix-debugger

DEFINES += GIX_INSTALL_DIR=$${GIX_INSTALL_DIR}

QMAKE_EXTRA_VARIABLES = GIX_INSTALL_DIR
