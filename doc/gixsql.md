## GixSQL

GixSQL is an ESQL preprocessor and a series of runtime libraries to enable GnuCOBOL to access ODBC, MySQL, PostgreSQL databases.

It originated as a (private) fork of OceSQL but has been almost completely rewritten: while the semantics related to the GnuCOBOL interface are similar and several support functions have been kept, the parser, scanner and library frameworks have been developed in C++ and have a different organization, with dynamically loadable modules as "database drivers".

The core of GixSQL has been incorporated in a more generic "preprocessing library" (libgixpp) that is used in Gix-IDE to parse COBOL files and derive some metadata used for navigation and debugging.

GixSQL is available both as an integrated module in Gix-IDE and as an external set of tools and libraries. As of v1.0.4 GixSQL is standard C++ and does not depend any more on Qt.

As of v1.0.7 GixSQL comes also in a standalone install package, so it can also be used without installing the full-blown IDE.

### Architecture
 
GixSQL comprises a preprocessor (a standalone executable or a library) and a set of runtime libraries. libgixsql.dll/.so is the main library and it is the one that will be linked to your COBOL modules. The other libraries (e.g. libgisql-odbc.dll/.so) will be dynamically loaded at runtime depending on the DB you chose in your configuration (see below). It is possible, if so desired, to develop additional libraries for specific DBMSs not covered in the standard install.

### Using GixSQL from Gix-IDE
When you create a project in Gix-IDE, you are asked whether you want to enable it for ESQL preprocessing. This is not an absolute requirement. At any point you can set the project property "Preprocess for ESQL" (under "General") to "Yes". There are several properties you can configure here:

- **Default driver**: the database driver to be loaded and used. You can choose between:
	- MySQL
	- PostgreSQL
	- ODBC
	- Dynamically selected (based on a "connection string" supplied to the driver)
	- Dynamically selected (based on the GIXSQL_DB_MODE environment variable)
- **Preprocess COPY files**: if set to "Yes" all copy files (not only those in EXEC SQL INCLUDE sections) will be parsed by the ESQL preprocessor. This is useful when you have copy files containing code that includes EXEC SQL statements.
- **Use anonymous parameters**: if set to "Yes", parameters in SQL statments will be represented as "?", otherwise a numeric indicator (i.e "$1") will be used.
- **Emit static calls**: if set to "Yes", the calls to the gixsql library functions will be emitted as static. This should be normally set to "Yes".

### Connecting to a database from COBOL

There is no "bind" procedure in GixSQL, you will have to manually open a connection to a database. This is done with a syntax that is quite similar to the one used in Micro Focus COBOL:

    ACCEPT DBNAME FROM ENVIRONMENT-VALUE.                        
    ACCEPT DBAUTH FROM ENVIRONMENT-VALUE.                      
    EXEC SQL
      CONNECT TO :DBNAME USER :DBAUTH
    END-EXEC. 

In this case the two values are retrieved from the environment variables DBNAME and DBAUTH and passed to the CONNECT function.

DBAUTH is a "connection string"-style alphanumeric field, whose format is basically

    localhost:5432/mydb

or

    PGSQL;localhost:5432/mydb

if you want to specify a database type (that can be ODBC, MYSQL or PGSQL) instead of providing it in the environment variable named GIXSQL_DB_MODE.

User name and password are provided in the second parameter (DBAUTH in this case) and follow the format:

    username.password

(yes, that's a dot)
*Note: I acknowledge this is rather simplistic and should be improved with more options, parameters, etc.*

When you run your code from the IDE, the path for the runtime libraries needed for the DBMS you chose are automatically added to your PATH. Obviously, when you are running outside the IDE, you will have to do this manually: the runtime libraries and their dependencies reside in `{gix-install-dir}\lib\{platform}\{architecture}` (**platform** can be either x64 or x86, **architecture** can either be msvc or gcc, depending on the compiler type you are using).

At the moment  Gix-IDE always uses its embedded version of GixSQL. In the future this will be extended to allow for other preprocessors.

### Examples

You can find a sample project collection for GixSQL (TEST001.gix) in the folder %USERPROFILE%\Documents\Gix\Examples ($HOME/Documents/gix/examples on Linux) that should have been created when you installed Gix-IDE. Under the project directory (%USERPROFILE%\Documents\Gix\Examples\TEST001 or $HOME/Documents/gix/examples/TEST001 on Linux) there is a SQL file with a DDL query and some data you can use to run the sample project.

### Using GixSQL outside Gix-IDE

If you want to manually precompile COBOL programs for ESQL, you can use the preprocessor binary (**gixpp** or **gixpp.exe**) you will find in the **bin** folder in Gix-IDE's install directory. When you run it from the console, ensure you have the same **bin** directory in your PATH/LD_LIBRARY_PATH since it contains some libraries that are needed by **gixpp**. These are the command line options available, that correspond to those described earlier:

	gixpp - the ESQL preprocessor for Gix-IDE/GixSQL
	Version: 1.0.7
	libgixpp version: 1.0.7
	
	Options:
	  -h, --help                  displays help on commandline options
	  -I, --copypath arg          COPY file path list
	  -i, --infile arg            input file
	  -o, --outfile arg           output file
	  -s, --symfile arg           output symbol file
	  -e, --esql                  preprocess for ESQL
	  -p, --esql-preprocess-copy  ESQL: preprocess all included COPY files
	  -E, --esql-copy-exts arg    ESQL: copy files extension list (comma-separated)
	  -a, --esql-anon-params      ESQL: use anonymous (not numbered) parameters
	  -S, --esql-static-calls     ESQL: emit static calls
	  -g, --debug-info            generate debug info
	  -c, --consolidate           consolidate source to single-file
	  -k, --keep                  keep temporary files
	  -v, --verbose               verbose
	  -d, --verbose-debug         verbose (debug)
	  

When you want to build and link from the console, remember also to add the `<gix-install-dir>/copy` directory to the COPY path list (it contains SQLCA) and to include **libgixsql** (and the appropriate path, depending on your architecture) to the compiler's command line.


### Basic command line example

First of all, you will (rather obviously) need a database server: this can be PostgreSQL, MySQL, or any other database that has a ODBC driver (DB2 works too). The DDL for this example targets PostgreSQL and might need to be modified to work with your DBMS of choice. 

You will find the files needed to run this example in the "examples/test001" subfolder, inside the "Gix" folder that the installer created in your "Documents" folder. On Windows this should be: 

`C:\Users\%USERNAME%\Documents\gix\test001`

On Linux (this may vary according to your distribution) it should be 

`$HOME/Documents/gix/test001`

Create an empty database or a schema, ensure you can access it with a given username and password, then use the DDL file `test001.sql` to create the test table we are going to use (named `emptable`). 

Make sure that the preprocessor (gixpp) is in your path, then preprocess the COBOL source file:

	gixpp -e -S -I. -ext ".,*.cpy,*.CPY" -i TEST001.cbl -o TEST001.cbsql

where:

- -e : preprocess for ESQL (mandatory for preprocessing)
- -S : use static calls when emitting GixSQL library calls (this is the mode to be normally used)
- -I. : use the curent directory for included COPY files (SQLCA is included from GixSQL's own directory)
- -ext ".,.cpy,.CPY" : look for COPY files having one of these extensions (comma-separated list)
- -i and -o : input and output file paths

 
If all goes well, you can compile the preprocessed file `TEST001.cbsql`:


    cobc -x TEST001.cbsql -L <GIXSQL_LIB_DIR> -llibgixsql	


The location of the actual path for `GIXSQL_LIB_DIR` depends on several elements:

- The install path for GixIDE (if you have installed the full-blown IDE) or GixSQL (if you only have installed one of the GixSQL packages)
- The architecture your version of Gix-IDE/GixSQL was compiled for or is running on (x86 or x64)
- On Windows only: the flavour of GnuCOBOL compiler you are running (MSVC-based or MinGW-based)

For instance, if we are on Windows x64, using GixSQL as shipped with Gix-IDE and an x64 version of GnuCOBOL, MSVC-based, the actual command will be:

    cobc -x TEST001.cbsql -L "C:\Program Files\Gix-IDE\lib\x64\msvc" -llibgixsql	

On Linux the packages are installed in /opt, so it would be (the "lib" prefix in the library name is not needed):

	cobc -x TEST001.cbsql -L /opt/gix-ide/lib/x64/gcc -lgixsql	

Now we can run the test program, we just need to set some environment variables (on Linux just use the appropriate paths and syntax):

- Set libgixsql to use the PostgreSQL driver
		
		set GIXSQL_DB_MODE=PGSQL
	
- Add the path for libgixsql (and the driver library, in this case libgixsql-pgsql.dll) to the path
	
		set PATH=%PATH%;C:\Program Files\Gix-IDE\lib\x64\msvc
	
	or (on Linux)
	
		export PATH=$PATH:/opt/gix-ide/lib/x64/gcc
	
	
- Set the two environment variables needed by the COBOL code itself (obviously they can be named as you wish):

		set DBNAME=192.168.1.1:5432/testdb
		set DBAUTH=test.test

The first variable uses the standard format for a GixSQL connection string used in the `EXEC SQL CONNECT TO` command: `host:port/dbname`. As an exception, if you are using the ODBC driver, it must only contain the DSN name, with no formatting, e.g.:

	set DBNAME=MYODBCCONN

The second variable contains username and password, separated by a dot.

Run the program (`TEST001.exe` on Windows) and you should get this output:


	C:\Users\Test\Documents\Gix\Examples\test001>TEST001.exe
	DB  : 192.168.1.1:5432/testdb
	USER: test.test
	***************************************
	open 00000000
	open
	fetch 00000000
	department: [DEP1]
	last name : [Doe       ]
	first name: [John      ]
	street    : [123, Nowhere Lane   ]
	city      : [Noplace        ]
	state     : [NA]
	zip code  : [00100]
	payrate   : [000000000000200]
	commission 000
	Do you want to see the next record? (y/n)

Keep pressing 'y' to advance in the loop and display all the three records in the sample table.


## Building from source

### Windows (Visual Studio)

In the top-level directory, beside the main solution file for Gix-IDE (`gix-ide.sln`) you will find a second solution file (`gixsql.sln`). You can use Visual Studio 2019 to build it, but first you likely will have to adjust the include and library definitions for the PostgreSQL and MySQL client libraries (32/and or 64 bit). The preprocessor (`gixpp`) and the main library (`libgixsql`) do not have any specific dependency.

### Linux

*All comands and packages refer to Ubuntu 20.04, You might need to adjust them depending on your distribution or environment.*

Move to the `gixsql` subdirectory under the top-level directory and run:

    make -f Makefile.linux

It should compile all the libraries, then the preprocessing library and the preprocessor. You can install with:

	sudo make -f Makefile.linux install

The default forectory for install is `/opt/gixsql`, you can select a different directory by setting the `DEST_DIR` variable, e.g.

	DEST_DIR=/usr/local/gixsql make -f Makefile.linux install

You will also need the development packages for the DBMS client libraries, e.g.:

	apt install libmysqlclient-dev libpq-dev unixodbc-dev flex

You will also need a modern enough version of bison. If you are using Ubuntu 20.04, you can download it from Debian's repositories and install it over the current one:

	wget http://ftp.debian.org/debian/pool/main/b/bison/bison_3.7.6+dfsg-1_amd64.deb

and

	sudo dpkg -i bison_3.7.6+dfsg-1_amd64.deb
	
### Windows (MinGW)
Currently there are no specific Makefiles for MinGW x86/x64, you can adjust the PATHs for include and library files and reuse the Windows ones