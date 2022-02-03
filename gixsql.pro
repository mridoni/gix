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
			gixpp
			
libgixpp.subdir = libgixpp			
libcpputils.subdir = libcpputils	
libgixsql.subdir  = gixsql/libgixsql
libgixsql-odbc.subdir  = gixsql/libgixsql-odbc			
libgixsql-mysql.subdir  = gixsql/libgixsql-mysql			
libgixsql-pgsql.subdir  = gixsql/libgixsql-pgsql			


libgixpp.depends = libcpputils

libgixsql-odbc.depends = libgixsql
libgixsql-mysql.depends = libgixsql
libgixsql-pgsql.depends = libgixsql

gixpp.subdir = gixpp
gixpp.depends = libgixpp

unix:GIX_INSTALL_DIR = /opt/gix-ide
win32:GIX_INSTALL_DIR = c:/gix-ide

DEFINES += GIX_INSTALL_DIR=$${GIX_INSTALL_DIR}

QMAKE_EXTRA_VARIABLES = GIX_INSTALL_DIR

#!system(echo "GIX_INSTALL_DIR=$${GIX_INSTALL_DIR}" ;  > gix.cf) {
#	warning(no configuration)
#}
