Package: gixsql
Section: devel
Version: #GIXIDEMAJ#.#GIXIDEMIN#.#GIXIDEREL#~#GIX_REVISION#-1
Priority: optional
Architecture: amd64
Depends: libmysqlclient21, libpq5, unixodbc
Installed-Size: 11284
Maintainer: Marco Ridoni <m.ridoni@gmail.com>
Description: GixSQL is an ESQL preprocessor and a series of runtime libraries to enable GnuCOBOL to access ODBC, MySQL, PostgreSQL databases
 .
 GixSQL comprises a preprocessor (a standalone executable or a library) 
 and a set of runtime libraries. libgixsql.dll/.so is the main library 
 and it is the one that will be linked to your COBOL modules. 
 The other libraries (e.g. libgisql-odbc.dll/.so) will be dynamically 
 loaded at runtime depending on the DB you chose in your configuration. 
 It is possible, if so desired, to develop additional libraries 
 for specific DBMSs not covered in the standard install.

