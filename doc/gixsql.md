## GixSQL

GixSQL is an ESQL preprocessor and a series of runtime libraries to enable GnuCOBOL to access ODBC, MySQL, PostgreSQL databases.

It originated as a (private) fork of OceSQL but has been almost completely rewritten: while the semantics related to the GnuCOBOL interface are similar and several support functions have been kept, the parser, scanner and library frameworks have been developed in C++ and have a different organization, with dynamically loadable modules as "database drivers".

The core of GixSQL has been incorporated in a more generic "preprocessing library" (libgixpp) that is used in Gix-IDE to parse COBOL files and derive some metadata used for navigation and debugging.

GixSQL is available both as an integrated module in Gix-IDE and as an external set of tools and libraries. Though the preprocessor still depends on Qt (for the time being) the runtime libraries are standard C++.

### Architecture

GixSQL comprises a preprocessor (a standalone executable or a library) and a set of runtime libraries. libgixmysql.dll/.so is the main library and it is the one that will be linked to your COBOL modules. The other libraries (e.g. libgisql-odbc.dll/.so) will be dynamically loaded at runtime depending on the DB you chose in your configuration (see below). It is possible, if so desired, to develop additional libraries for specific DBMSs not covered in the standard install.

### Using GixSQL from Gix-IDE
When you create a project in Gix-IDE, you are asked whether yo want to enable it for ESQL preprocessing. This is not an absolute requirement. At any point you can set the project property "Preprocess for ESQL" (under "General") to "Yes". There are several properties you can configure here:

- **Default driver**: the database driver to be loaded and used. You can choose between:
	- MySQL
	- PostgreSQL
	- ODBC
	- Dynamically selected (based on a "connection string" supplied to the driver)
	- Dynamically selected (based on the GIXSQL_DB_MODE environment variable)
- **Preprocess COPY files**: if set to "Yes" all copy files (not only those in EXEC SQL INCLUDE sections) will be parsed by the ESQL preprocessor. This is useful when you have copy files containing code that include EXEC SQL statements.
- **Use anonymous parameters**: if set to "Yes", parameters in SQL statments will be represented as "?", otherwire a numeric indicator (i.e "$1") will be used.
- **Emit static calls**: if set to "Yes", the calls to the gixsql library functions will be emitted as static. This should be normally set to "Yes".

### Connecting to a database from COBOL

There is no "bind" procedure in GixSQL, you will have to manually open a connection to a database. This is done with a syntax that is quite similar to the one used in Micro Focus COBOL:

    ACCEPT DBNAME FROM ENVIRONMENT-VALUE.                        
    ACCEPT DBAUTH FROM ENVIRONMENT-VALUE.                      
    EXEC SQL
      CONNECT TO :DBNAME USER :DBAUTH
    END-EXEC. 

In this case the two values are retrieved from the environment and passed to the CONNECT function.

DBAUTH is a "connection string"-style alphanumeric field, whose format is basically

    localhost:5432/mydb

or

    PGSQL;localhost:5432/mydb

if you want to specify a database type (that can be ODBC, MYSQL or PGSQL) instead of providing it in the environment variable named GIXSQL_DB_MODE.

User name and password are provided in the second parameter (DBAUTH in this case) and follow the format:

    username.password

(yes, that's a dot)
*Note: I acknowledge this is rather simplistic and should be improved with more options, parameters, etc.*

When you run your code from the IDE, the path for the runtime libraries needed for the DBMS you chose are automatically added to your PATH. Obviously, when you are running outside outside the IDE, you will have to do this manually: the runtime libraries and their dependencies reside in `{gix-install-dir}\lib\{platform}\{architecture}` (**platform** can be either x64 or x86, **architecture** can either be mdvc or gcc, depending on the compiler type you are using).

At the moment  Gix-IDE always uses its embedded version of GixSQL. In the future this will be extended to allow for other preprocessors.

### Using GixSQL outside Gix-IDE

If you want to manually precompile COBOL programs for ESQL, you can use the preprocessor binary (**gixpp** or **gixpp.exe**) you will find in the **bin** folder in Gix-IDE's install directory. When you run it from the console, ensure you have the same **bin** directory in your PATH since it contains some libraries that are needed by **gixpp**. These are the command line options available, that correspond to those described earlier:

    Usage: gixpp [options]
    
    Options:
      -h, --help                                              Displays help on
                                                              commandline options.
      -I <copypath>                                           COPY file path list
      -i <infile>                                             input file
      -o <outfile>                                            output file
      -s <symfile>                                            output symbol file
      -e, --esql                                              preprocess for ESQL
                                                              (single file mode
                                                              takes precedence)
      -p, --esql-preprocess-copy                              ESQL: preprocess copy
                                                              files outside EXEC SQL
                                                              INCLUDE statements
      -E <ESQL: copy files extension list (comma-separated)>  esql-copy-exts
      -a, --esql-anon-params                                  ESQL: use anonymous
                                                              (not numbered)
                                                              parameters
      -S, --esql-static-calls                                 ESQL: emit static
                                                              calls
      -g, --debug-info                                        generate debug info
      -c                                                      consolidate source to
                                                              single-file (CP)
      -k                                                      keep temporary files
      -v                                                      Verbose
      -d                                                      Verbose (debug)

When you want to build and link from the console, remember also to add the `<gix-install-dir>/copy` directory to the COPY path list (it contains SQLCA) and to include **libgixmysql** (and the appropriate path, depending on your architecture) to the compiler's command line.


