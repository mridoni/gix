CONFIG(release,debug|release) DESTDIR = x64/Release
CONFIG(debug,debug|release) DESTDIR = x64/Debug

CONFIG += ordered c++17

TEMPLATE = subdirs
SUBDIRS += \
			libgixsql \
			libgixsql-odbc \
			libgixsql-mysql \
			libgixsql-pgsql \
			libgixpp \
			libgixutils \
			gix-common \
			scintilla \
			gix-ide
			
libgixpp.subdir = libgixpp			
libgixutils.subdir = libgixutils			
libgixsql.subdir  = gixsql/libgixsql
libgixsql-odbc.subdir  = gixsql/libgixsql-odbc			
libgixsql-mysql.subdir  = gixsql/libgixsql-mysql			
libgixsql-pgsql.subdir  = gixsql/libgixsql-pgsql			
gix-common.subdir  = gix-common			
scintilla.subdir = libs/scintilla/src/scintilla/qt/ScintillaEdit
gix-ide.subdir = gix-ide

gix-ide.depends = libgixpp libgixutils gix-common gix-debugger libgixsql scintilla
libgixsql-odbc.depends = libgixsql
libgixsql-mysql.depends = libgixsql
libgixsql-pgsql.depends = libgixsql

unix:GIX_INSTALL_DIR = /opt/gix-ide
win32:GIX_INSTALL_DIR = c:/gix-ide

gix-ide.path = ${GIX_INSTALL_DIR}/bin
gix-ide.files = $(DESTIDIR)/gix-ide

INSTALLS += gix-ide

DEFINES += GIX_INSTALL_DIR=$${GIX_INSTALL_DIR}

QMAKE_EXTRA_VARIABLES = GIX_INSTALL_DIR

#!system(echo "GIX_INSTALL_DIR=$${GIX_INSTALL_DIR}" ;  > gix.cf) {
#	warning(no configuration)
#}
