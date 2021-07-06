Package: gix-ide
Section: devel
Version: #GIXIDEMAJ#.#GIXIDEMIN#.#GIXIDEREL#~#GIX_REVISION#-1
Priority: optional
Architecture: amd64
Depends: qt5-default, libqt5xmlpatterns5, libqtermwidget5-0, libdwarf1, libelf++0, libdwarf++0, libmysqlclient21, libpq5, unixodbc
Installed-Size: 11284
Maintainer: Marco Ridoni <m.ridoni@gmail.com>
Description: Gix-IDE (or Gix) is an IDE and platform for GnuCOBOL providing a native debugger, ESQL and HTTP REST services.
 .
 The centerpiece is a Qt-based IDE that provides syntax highlighting 
 (thanks to Scintilla), support for different versions of the compiler, 
 a DB manager, variable navigation and integrated native debugging.
 .
 Gix-IDE incorporates a custom module that provides native debugging 
 (only supported with GnuCOBOL 3.1+) including breakpoints, stepping, 
 variable inspection of both executable and callable modules.
 .
 Gix-IDE provides an ESQL preprocessor and drivers for ODBC, MySQL 
 and PostgreSQL.
 .
 An HTTP server is provided that can natively call GnuCOBOL modules, 
 using HTTP methods and JSON for input/output. Such modules can be 
 run and debugged from inside the IDE.
 .
 Gix-IDE also incorporates a DB manager that can generate COPY files 
 from a DB connection (DCLGEN-style) and look at basic table/field 
 properties.
