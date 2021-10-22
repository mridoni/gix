CONFIG(release,debug|release) DESTDIR = $$(HOST_PLATFORM)/Release
CONFIG(debug,debug|release) DESTDIR = $$(HOST_PLATFORM)/Debug

CONFIG += ordered c++17

TEMPLATE = subdirs
SUBDIRS += \
			libgixsql \
			libgixsql-odbc \
			libgixsql-mysql \
			libgixsql-pgsql \
			libcpputils \
			libgixpp \
			libgixutils \
			gix-common \
			gix-http \
			gixpp \
			scintilla
			
win32:SUBDIRS += libs/libdwarf/libdwarf	
			
SUBDIRS += gix-ide				

libgixpp.subdir = libgixpp			
libgixutils.subdir = libgixutils	
libcpputils.subdir = libcpputils	
libgixsql.subdir  = gixsql/libgixsql
libgixsql-odbc.subdir  = gixsql/libgixsql-odbc			
libgixsql-mysql.subdir  = gixsql/libgixsql-mysql			
libgixsql-pgsql.subdir  = gixsql/libgixsql-pgsql			

gix-common.subdir  = gix-common			
gix-common.depends = libcpputils libgixutils

libgixpp.depends = libcpputils

scintilla.subdir = libs/scintilla/qt/ScintillaEdit

gix-ide.subdir = gix-ide
gix-ide.depends = libgixpp libgixutils gix-common gix-debugger libgixsql scintilla

win32:gix-ide.depends += libs/libdwarf/libdwarf
win32:libdwarf.subdir = libs/libdwarf/libdwarf

libgixsql-odbc.depends = libgixsql
libgixsql-mysql.depends = libgixsql
libgixsql-pgsql.depends = libgixsql

gixpp.subdir = gixpp
gixpp.depends = libgixpp

unix:GIX_INSTALL_DIR = /opt/gix-ide
win32:GIX_INSTALL_DIR = c:/gix-ide

gix-ide.path = ${DESTDIR}
gix-ide.files = $(DESTIDIR)/gix-ide

INSTALLS += gix-ide

DEFINES += GIX_INSTALL_DIR=$${GIX_INSTALL_DIR}

QMAKE_EXTRA_VARIABLES = GIX_INSTALL_DIR

#!system(echo "GIX_INSTALL_DIR=$${GIX_INSTALL_DIR}" ;  > gix.cf) {
#	warning(no configuration)
#}
