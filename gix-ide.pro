CONFIG(release,debug|release) DESTDIR = x64/Release
CONFIG(debug,debug|release) DESTDIR = x64/Debug

CONFIG += ordered

TEMPLATE = subdirs
SUBDIRS += \
			gixsql \
			libgixsql \
			libgixsql-odbc \
			libgixsql-mysql \
			libgixsql-pgsql \
			gix-debugger \
			gix-common \
			gix-ide
			
gixsql.subdir = gixsql/gixsql
libgixsql.subdir  = gixsql/libgixsql
libgixsql-odbc.subdir  = gixsql/libgixsql-odbc			
libgixsql-mysql.subdir  = gixsql/libgixsql-mysql			
libgixsql-pgsql.subdir  = gixsql/libgixsql-pgsql			
gix-debugger.subdir  = gix-debugger			
gix-common.subdir  = gix-common			
gix-ide.subdir = gix-ide

gix-ide.depends = gix-common gix-debugger libgixsql
gixsql.depends = libgixsql
libgixsql-odbc.depends = libgixsql
libgixsql-mysql.depends = libgixsql
libgixsql-pgsql.depends = libgixsql

unix:GIX_INSTALL_DIR = /opt/gix-ide
win32:GIX_INSTALL_DIR = c:/gix-ide

gix-ide.path = $${GIX_INSTALL_DIR}/bin
gix-ide.files = $(DESTIDIR)/gix-ide

INSTALLS += gix-ide

DEFINES += GIX_INSTALL_DIR=$${GIX_INSTALL_DIR}

QMAKE_EXTRA_VARIABLES = GIX_INSTALL_DIR

#!system(echo "GIX_INSTALL_DIR=$${GIX_INSTALL_DIR}" ;  > gix.cf) {
#	warning(no configuration)
#}
