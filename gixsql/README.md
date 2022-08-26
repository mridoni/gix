
## GixSQL

GixSQL is an ESQL preprocessor and a series of runtime libraries to enable GnuCOBOL to access PostgreSQL, ODBC, MySQL, Oracle and SQLite databases.

_**For updated news and current developments [look here](https://mridoni.github.io/) on the development blog.**_

It originated as a (private) fork of [ocesql](https://github.com/GitMensch/Open-COBOL-ESQL) but has been almost completely rewritten: while the semantics related to the GnuCOBOL interface are similar and several support functions have been kept, the parser, scanner and library frameworks have been developed in C++ and have a different organization, with dynamically loadable modules as "database drivers".

The core of GixSQL has been incorporated in a more generic "preprocessing library" (libgixpp) that is used in [Gix-IDE](https://github.com/mridoni/gix) to parse COBOL files and derive some metadata used for navigation and debugging.

GixSQL is available both as an integrated module in Gix-IDE and as an external set of tools and libraries. As of v1.0.4 GixSQL is standard C++ and does not depend any more on Qt.

As of v1.0.7 GixSQL comes also in a standalone install package, so it can also be used without installing the full-blown IDE.

As of v1.0.13 GixSQL has been moved to its own repository.

### Architecture
 
GixSQL comprises a preprocessor (a standalone executable or a library) and a set of runtime libraries. libgixsql.dll/.so is the main library and it is the one that will be linked to your COBOL modules. The other libraries (e.g. libgisql-odbc.dll/.so) will be dynamically loaded at runtime depending on the DB you chose in your configuration (see below). It is possible, if so desired, to develop additional libraries for specific DBMSs not covered in the standard install.

### Connecting to a database from COBOL

*Note: the "connection string" format has changed in v1.0.8 and is incompatible with the old one. Given the amount of fixes and improvements, it is strongly suggested to upgrade to the latest version anyway.*

There is no "bind" procedure in GixSQL, you will have to manually open a connection to a database. This can be done in different ways: this is an example of a syntax that is quite similar to the one used in Micro Focus COBOL:

```cobol
    ACCEPT DATASRC FROM ENVIRONMENT-VALUE.                        
    ACCEPT DBAUTH FROM ENVIRONMENT-VALUE.                      
    EXEC SQL
      CONNECT TO :DATASRC USER :DBAUTH
    END-EXEC. 
```

In this case the two values are retrieved from the environment variables `DBNAME` and `DBAUTH` and passed to the CONNECT function.

`DATASRC` is a "connection string"-style alphanumeric field, whose standard format (see below) is basically

    <dbtype>://<host>[:port][/dbname][?[opt1=val1]&...]

e.g. (if using PostgreSQL)

    pgsql://localhost:5432/testdb?default_schema=myschema

In this case the username and password are provided in the second parameter (`DBAUTH` in this case) and follow the format (yes, that's a dot):

    username.password

You can also use other formats for your connection statements, like

	CONNECT TO :DATASOURCE USER :USERNAME USING :PASSWORD

or

	CONNECT :USERNAME IDENTIFIED BY :PASSWORD USING :DATASOURCE

All the identifiers for data sources, usernames and passwords can be either COBOL variables (prefixed by a semi-colon) or string literals.

#### Connection string formats

Starting from version 1.0.16dev1 there are three supported formats for "connection strings":

1) GixSQL "standard", e.g.:
    - `pgsql://test.test@localhost:5432/testdb1`
    - `pgsql://localhost:5432/testdb1`

2) GixSQL with "no DB driver"
    - `localhost:5432/testdb1`
    - `test.test@localhost:5432/testdb1`

3) OCESQL-compatible
    - `testdb1@localhost:5432`

For cases 2) and 3) the DB/driver type is inferred by:

- setting a compile-time constant (compile-time means "when GixSQL is compiled"):
	 - manually in default_driver.h (e.g. `#define GIXSQL_DEFAULT_DRIVER	"pgsql"`)
	 - using the `--with-default-driver=pgsql|odbc|mysql|none` when executing the configure script (Linux/MinGW only)
- if this constant is missing or empty (default), the content of the environment variable `GIXSQL_DEFAULT_DRIVER` is used. If no default driver is available (either set by the user or provided by the compile-time configuration) and none has been specified in the user-supplied connection string, the library will return an error.



### Multiple connections

Starting with version 1.0.8 of GixSQL/Gix-IDE, it is possible to open and manage multiple connections:

	CONNECT TO :db_data_source AS :db_conn_id USER :username.:opt_password [ USING password ]

or 

	CONNECT :username IDENTIFIED BY :password [ AT :db_conn_id ] USING :db_data_source
	
where **db_conn_id** is an identifier for your connection.

Then you can use this identifier in your SQL statements, e.g.:

	       EXEC SQL
              CONNECT TO :DATASRC-1 AS CONN1 USER :DBUSR-1
           END-EXEC.    
           
           EXEC SQL
              CONNECT TO :DATASRC-2 AS CONN2 USER :DBUSR-2
           END-EXEC.   
           
           EXEC SQL AT CONN1 DROP TABLE IF EXISTS TAB1 END-EXEC.

           EXEC SQL AT CONN2 DROP TABLE IF EXISTS TAB2 END-EXEC.
           



### Declaring SQL host variables

For the time being the `BEGIN DECLARE SECTION`/`END DECLARE SECTIONS` statements are processed but ignored. You can use any COBOL field in SQL statements, e.g.:

```cobol
	      WORKING-STORAGE SECTION. 

           01 T1     PIC 9(4) VALUE 0.  
	       
	       ...
	       
	       EXEC SQL AT CONN1
               SELECT SUM(FLD1) INTO :T1 FROM TAB1
           END-EXEC.
```

As a special case, if you need to declare and use field to be associated with variable length database fields (i.e. VARBINARY or VARCHAR), you can do it in one of the following ways:

1) use the `SQL TYPE IS` clause:  

    ```cobol
            01 VBFLD SQL TYPE IS VARBINARY(100).
    ```

    This will treat the underlying (generated) field as a `VARCHAR`, applying the appropriate padding rules.

    You can also generate what is commonly known as a "variable-length group". If you declare a variable as:

    ```cobol
            01 VBFLD PIC X(100) VARYING.
    ```

    The following fields will be generated:

    ```cobol
            01 VBFLD.
                 49 VBFLD-LEN PIC 9(8) COMP-5.
                 49 VBFLD-ARR PIC X(100).
    ```

    As it is standard practice in ESQL COBOL, "level 49" fields are used to store a `VARCHAR`/`VARBINARY` field, handling separately its length in the first child data-item and the actual data in the second.

    The default suffixes `-LEN` and `-ARR` can be customized by using the `-Y`/`--varying` option in `gixpp`.

    Another `gixpp` option (`-P`/`--picx-as arg`) allows to choose the default handling for standard `PIC X` fields (as `CHAR`, with padding or as `VARCHAR`, auto-trimmed when written to the database). Please not that this does not apply to variable-length groups, whose actual length is always determined by the length indicator field.

    *(Note: you can select 2 or 4 bytes for the data item length indicator, the standard being 4. This option can be currently changed at compile time of GixSQL only, by defining the `USE_VARLEN_16` constant).*

2) manually define "level 49" fields as in case 1, this is the case with some legacy code.  
As noted above ensure that the length (2 or 4 bytes) matches your installation of GixSQL.

3) use the `EXEC SQL VAR` syntax for a given field:
 
    ```cobol
             WORKING-STORAGE SECTION. 
     
                  01 VARD PIC X(120).

             ....

             EXEC SQL VAR
                  VARD IS VARCHAR(120)
             END-EXEC.   
    ```

Note: You can also use the `EXEC SQL VAR` syntax to declare other SQL-typed fields, e.g:

```sql
   EXEC SQL VAR NUM3 IS FLOAT END-EXEC.
        
   EXEC SQL VAR NUM4 IS FLOAT(6,2) END-EXEC.
```

As of v1.0.10 the supported SQL types are `FLOAT`, `REAL`, `INTEGER`, `DECIMAL`. `VARCHAR2` is supported at a syntactic level but for now is treated as a standard `VARCHAR`.

### Prepared statements
Starting from v1.0.10 GixSQL supports prepared statements:

```cobol
       WORKING-STORAGE SECTION. 
           ...

           01  DYNSTMT1   SQL TYPE IS VARCHAR(100).

       PROCEDURE DIVISION. 

           ...
		   
           MOVE 'INSERT INTO TAB1(F1, F2) VALUES(?, ?)' TO DYNSTMT1.
           
           EXEC SQL 
               PREPARE SQLSTMT1 FROM :DYNSTMT1
           END-EXEC.

           MOVE 1 TO T1.
           MOVE 2 TO T2.
           EXEC SQL EXECUTE SQLSTMT1 USING :T1, :T2 END-EXEC.
```

`EXECUTE IMMEDIATE` is also supported (as is the case above, you can use literals or host references):

```
           EXEC SQL EXECUTE IMMEDIATE 
                'UPDATE TAB1 SET FLD1=FLD1+100, FLD2=FLD2+300'
           END-EXEC.
```

### Driver options and notes

Starting from v1.0.8 it is possible to pass options to the backend "drivers", i.e., the submodules of GixSQL that interface with a specific DBMS. For now only a few options are supported, but their number will probably grow in the next releases:

- `client_encoding`: sets the default text encoding for client connections (supported in MySQL and PostgreSQL)
- `autocommit`: sets autocommit on or off (default: off, supported in MySQL and PostgreSQL)
- `default_schema`: selects the default schema (supported in PostgreSQL, maps to the search_path)

Driver options are passed in the connection string, e.g.:

	pgsql://localhost/mydb?autocommit=on&client_encoding=UTF8&default_schema=myschema

For “boolean" options you can use either `on`/`off` or `1`/`0` to enable or disable them.

### Setting client encoding and autocommit

You can set client encoding and autocommit options for a given connection in two different ways.

- using a parameter in the datasource, like indicated above e.g.

      pgsql://localhost:5432/testdb?default_schema=myschema&client_encoding=LATIN1

- using  an environment variable, e.g.
	
	  export GIXSQL_CLIENT_ENCODING=UTF8
	  export GIXSQL_AUTOCOMMIT=on

Notes:

- A setting specified in the datasource definition (the first method) takes precedence over a setting specified in an environment variable
- Defaults are:
	-	client encoding: UTF8
	-	autocommit: off  
- Client encoding identifiers are driver-specific, i.e. you should use:  
	+	`export GIXSQL_CLIENT_ENCODING=utf8mb4` for MySQL
	+	`export GIXSQL_CLIENT_ENCODING=UTF8` for PostgreSQL
	
### Logging

Starting from version 1.0.16 GixSQL supports an improved logging engine, based on [spdlog](https://github.com/gabime/spdlog). Logging options can be controlled by using two environment variables:

- **GIXSQL_LOG_LEVEL**  
Sets the debug level. It can be `off`, `critical`, `error` (default), `warn`, `info`, `debug` or `trace`. Be aware that the `trace` option: 1) exposes a lot of internal information, including possibly sensitive data. 2) causes a slowdown of about 30%.

- **GIXSQL_LOG_FILE**  
Specifies the file where the debug output (if any) is written. Defaults to "gixsql.log"

*Pre-v1.0.18* the two environment variables were named `GIXSQL_DEBUG_LOG_LEVEL` and `GIXSQL_DEBUG_LOG_FILE`. The default log level was `off`.

*Pre -v1.0.16*: you can use the environment variables `GIXSQL_DEBUG_LOG-ON=1` (which defaults to 0=OFF) and `GIXSQL_DEBUG_LOG` (defaults to "gixsql.log" in your temp directory). This mechanism has been removed in v1.0.16+

### Examples

You can find a sample project collection for GixSQL (TEST001.gix) in the folder `%USERPROFILE%\Documents\Gix\Examples` (`$HOME/Documents/gix/examples` on GNU/Linux) that should have been created when you installed Gix-IDE.  
Under the project directory (`%USERPROFILE%\Documents\Gix\Examples\TEST001` or `$HOME/Documents/gix/examples/TEST001` on GNU/Linux) there is a SQL file with a DDL query and some data you can use to run the sample project.

### Using GixSQL

If you want to manually precompile COBOL programs for ESQL, you can use the preprocessor binary (**gixpp** or **gixpp.exe**) you will find in the **bin** folder in Gix-IDE's install directory. When you run it from the console, ensure you have the same **bin** directory in your `PATH`/`LD_LIBRARY_PATH` since it contains some libraries that are needed by **gixpp**. These are the command line options available, that correspond to those described earlier:

```text
gixpp - the ESQL preprocessor for Gix-IDE/GixSQL
Version: 1.0.18
libgixpp version: 1.0.18

Options:
  -h, --help                  displays help on commandline options
  -V, --version               displays version information
  -I, --copypath arg          COPY file path list
  -i, --infile arg            input file
  -o, --outfile arg           output file
  -s, --symfile arg           output symbol file
  -e, --esql                  preprocess for ESQL
  -p, --esql-preprocess-copy  ESQL: preprocess all included COPY files
  -E, --esql-copy-exts arg    ESQL: copy files extension list (comma-separated)
  -z, --param-style arg (=d)  ESQL: generated parameters style (=a|d|c
  -S, --esql-static-calls     ESQL: emit static calls
  -g, --debug-info            generate debug info
  -c, --consolidate           consolidate source to single-file
  -k, --keep                  keep temporary files
  -v, --verbose               verbose
  -d, --verbose-debug         verbose (debug)
  -m, --map                   emit map file
  -C, --cobol85               emit COBOL85-compliant code
  -Y, --varying arg           length/data suffixes for varlen fields (=LEN,ARR)
  -P, --picx-as arg (=char)   text field options (=char|charf|varchar)
  --no-rec-code arg           custom code for "no record" condition(=nnn)
```	  

When you want to build and link from the console, remember also to add the `<gix-install-dir>/share/gix/copy` directory to the COPY path list (it contains SQLCA) and to include **libgixsql** (and the appropriate path, depending on your architecture) to the compiler's command line.

As an alternative you may call **gixsql** which is a wrapper around the gixsql binary.

### Basic command line example

First of all, you will (rather obviously) need a database server: this can be PostgreSQL, MySQL, or any other database that has a ODBC driver (DB2 works too). The DDL for this example targets PostgreSQL and might need to be modified to work with your DBMS of choice. 

You will find the files needed to run this example in the "examples/test001" subfolder, inside the "Gix" folder that the installer created in your "Documents" folder. On Windows this should be: 

`C:\Users\%USERNAME%\Documents\gix\test001`

On Linux (this may vary according to your distribution) it should be 

`$HOME/Documents/gix/test001`

Create an empty database or a schema, ensure you can access it with a given username and password, then use the DDL file `test001.sql` to create the test table we are going to use (named `emptable`). 

Make sure that the preprocessor (gixpp) is in your path, then preprocess the COBOL source file:

	gixpp -e -S -I. -ext ".,*.cpy,*.CPY" -i TEST001.cbl -o TEST001.cbsql
	gixsql TEST001.cbl TEST001.cbsql -S -I. -ext ".,*.cpy,*.CPY"

where:

- `-e `: preprocess for ESQL (mandatory for preprocessing)
- `-S`: use static calls when emitting GixSQL library calls (this is the mode to be normally used)
- `-I.`: use the current directory for included COPY files (SQLCA is included from GixSQL's own directory)
- `-ext ".,.cpy,.CPY"`: look for COPY files having one of these extensions (comma-separated list)
- `-i` and `-o`: input and output file paths

Another interesting option is `--picx-as`: this indicates how standard `PIC(X)` fields should be treated when sent to the DBMS. There are three possible options:

- `char`: treat `PIC(X)` fields as standard `CHAR` fields (preserving trailing spaces)
- `charf`: (synonymous for `char`)
- `varchar`: treat `PIC(X)` fields as `VARCHAR` fields (remove trailing spaces)

*Please note that this does NOT affect variable-length groups, whose data part (by default the sub-field having an `-ARR` suffix) is always output with the length specified in the corresponding length indicator field.*

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
	
- Add the path for libgixsql (and the driver library, in this case libgixsql-pgsql.dll) to the path
	
		set PATH=%PATH%;C:\Program Files\Gix-IDE\lib\x64\msvc
	
	or (on Linux)
	
		export PATH=$PATH:/opt/gix-ide/lib/x64/gcc
	
	
- Set the two environment variables needed by the COBOL code itself (obviously they can be named as you wish):

		set DBNAME=pgsql://192.168.1.1:5432/testdb
		set DBAUTH=test.test

The first variable uses the standard format for a GixSQL connection string used in the `EXEC SQL CONNECT TO` command: `host:port/dbname`.

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

GixSQL requires a C++17-compatible compiler, with support of the `filesystem` namespace. This usually means GCC 8+ and Visual Studio 2017 onwards (minimum 15.7) or, better, Visual Studio 2019/2022. Building GixSQL with older compilers might need adding `LIBS=-lstdc++fs` to your configure line (old gcc/lang) or `LIBS=-lc++fs` (old LLVM).

### Windows (Visual Studio)

After cloning or downloading the repository you will find a solution file (`gixsql.sln`). You can use Visual Studio 2019 to build it, but first you  will likely have to check the include and library definitions for the PostgreSQL and MySQL client libraries (32/and or 64 bit). The preprocessor (`gixpp`) does not have any specific dependency, while the main runtime library (`libgixsql`)  - starting from v1.0.18 - depends on [spdlog](https://github.com/gabime/spdlog) and [fmt](https://github.com/fmtlib/fmt).

The solution file is already set up to build with libraries from [vcpkg](https://vcpkg.io/en/index.html), so you can simply do:

`vcpkg install libpq:x64-windows libmariadb:x64-windows fmt:x64-windows-static-md spdlog:x64-windows-static-md`

for x64, or :

`vcpkg install libpq:x86-windows libmariadb:x86-windows fmt:x86-windows-static-md spdlog:x86-windows-static-md`

in case you want to to build a 32-bit version.

### Linux

*All commands and packages refer to Ubuntu 20.04, You might need to adjust them depending on your distribution or environment.*

You will need the development packages for the DBMS client libraries, e.g.:

	apt install libmariadb-dev libpq-dev unixodbc-dev flex

*(it is still possible to use libmysqlclient-dev, should you prefer it).*

Starting from v1.0.16 you will also need the development packages for spdlog and fmt, if not already installed:

	apt install ibspdlog-dev libfmt-dev

You will also need a modern enough version of bison (3.7+). If you do not already have it installed and you are using Ubuntu 20.04, you can download it from Debian's repositories and install it over the current one (or you can download and compile it from [here](https://www.gnu.org/software/bison/).

	wget http://ftp.debian.org/debian/pool/main/b/bison/bison_3.7.5+dfsg-1_amd64.deb

and

	sudo dpkg -i bison_3.7.5+dfsg-1_amd64.deb

Download the .tar.gz.package from the Releases page, e.g.

	gixsql-1.0.16-xxxx.tar.gz

Untar the package:

	tar xzvf gixsql-1.0.16-641.tar.gz

cd to the directory created by the tar command and run configure (in this case we will install to /opt/gixsql)

	cd gixsql-1.0.16-641
	./configure --prefix=/opt/gixsql

By default configure tries to build all the drivers. If you nly need one, you can disable the others. For instance, to build only the PostgreSQL driver:

	./configure --prefix=/opt/install --disable-mysql --disable-odbc

If all goes well you can just do:

    make

It should compile all the libraries, then the preprocessing library and the preprocessor. You can install with:

	sudo make install
	
### Windows (MinGW/MSYS2)

Starting from v1.0.16, the `configure` script can also be used to build with MSYS2 (MinGW32/64). You will need to install the following packages with `pacman`:

`pacman -S mingw-w64-x64-pkg-config autoconf make automake libtool bison flex mingw-w64-x64-gcc mingw-w64-x64-postgresql mingw-w64-x64-libmariadbclient mingw-w64-x64-unixodbc mingw-w64-x64-spdlog `

Reasonably up-to-date installs of MinGW already have a correct version of `bison`.

## Usage notes

### Using GixSQL from Gix-IDE
When you create a project in Gix-IDE, you are asked whether you want to enable it for ESQL preprocessing. This is not an absolute requirement. At any point you can set the project property "Preprocess for ESQL" (under "General") to "Yes". There are several properties you can configure here that affed code generation by the preprocessor:

- **Preprocess COPY files**: if set to "Yes" all copy files (not only those in EXEC SQL INCLUDE sections) will be parsed by the ESQL preprocessor. This is useful when you have copy files containing code that includes EXEC SQL statements.
- **Use anonymous parameters**: if set to "Yes", parameters in SQL statements will be represented as `?`, otherwise a numeric indicator (i.e `$1`) will be used.
- **Emit static calls**: if set to "Yes", the calls to the gixsql library functions will be emitted as static. This should be normally set to "Yes".

When you run your code from the IDE, the path for the runtime libraries needed for the DBMS you chose are automatically added to your PATH. Obviously, when you are running outside the IDE, you will have to do this manually: the runtime libraries and their dependencies reside in `{gix-install-dir}\lib\{platform}\{architecture}` (**platform** can be either x64 or x86, **architecture** can either be msvc or gcc, depending on the compiler type you are using).

At the moment  Gix-IDE always uses its embedded version of GixSQL. In the future this will be extended to allow for other preprocessors.

### GixSQL-specific error codes

When an error occurs, in the runtime libraries or in the DBMS, GixSQL does its best to return standard-compliant  `SQLSTATE` and `SQLCODE` error codes and messages (the "no record found" condition is a special case, see below). There are a few instances where an operation fails due to "internal" issues (logic errors, consistency checks, unsupported features, possible driver bugs, etc.). In these case GixSQL will use a custom error code and message for `SQLCODE` and `SQLERRM`. The table below details the internal error codes that are currently use and a brief explanation for each of them (error messages in `SQLERRM` may be slightly different due to space limitation in the field).

When one of these errors occur and there isn't a self-evident explanation (e.g. your program did not properly initialize a data field used for a prepared statement) you can use the logging system (see above) to try and diagnose the problem.


| ID                          | Number | Description                                                 |
|-----------------------------|--------|-------------------------------------------------------------|
| DBERR_NO_ERROR              | 0      | No error occurred                                           |
| DBERR_CONNECTION_FAILED     | -100   | Connection to the database has failed                       |
| DBERR_BEGIN_TX_FAILED       | -101   | A transaction could not be started                          |
| DBERR_END_TX_FAILED         | -102   | A transaction could not be ended                            |
| DBERR_CONN_NOT_FOUND        | -103   | Connection ID not found                                     |
| DBERR_CONN_RESET_FAILED     | -104   | Connection close failed                                     |
| DBERR_EMPTY_QUERY           | -105   | Empty query                                                 |
| DBERR_SQL_ERROR             | -106   | Generic SQL/driver error                                    |
| DBERR_TOO_MANY_ARGUMENTS    | -107   | Too many arguments for a given function                     |
| DBERR_TOO_FEW_ARGUMENTS     | -108   | Too few arguments for a given function                      |
| DBERR_NO_PARAMETERS         | -109   | Parameters were expected but not supplied                   |
| DBERR_CURSOR_EXISTS         | -110   | The cursor already exists                                   |
| DBERR_NO_SUCH_CURSOR        | -111   | There is no such cursor                                     |
| DBERR_CLOSE_CURSOR_FAILED   | -112   | Cursor could not be closed                                  |
| DBERR_DISCONNECT_FAILED     | -113   | Could not disconnect from the DB                            |
| DBERR_OUT_OF_MEMORY         | -114   | Out of memory                                               |
| DBERR_DECLARE_CURSOR_FAILED | -115   | Cursor declaration failed                                   |
| DBERR_OPEN_CURSOR_FAILED    | -116   | Cursor could not be opened                                  |
| DBERR_FETCH_ROW_FAILED      | -117   | Could not fetch a row from the cursor                       |
| DBERR_INVALID_COLUMN_DATA   | -118   | Column data is not valid                                    |
| DBERR_CURSOR_CLOSED         | -119   | Cursor is closed                                            |
| DBERR_MOVE_TO_FIRST_FAILED  | -120   | Cannot move to first row in a resultset                     |
| DBERR_FIELD_COUNT_MISMATCH  | -121   | Result field count does not match with the one in the query |
| DBERR_NO_DATA               | -122   | No data rows when data rows were expected                   |
| DBERR_TOO_MUCH_DATA         | -123   | Received more data rows than expected                       |
| DBERR_PREPARE_FAILED        | -124   | Prepare statement failed                                    |
| DBERR_CONN_INIT_ERROR       | -201   | Connection initialization error                             |
| DBERR_CONN_INVALID_DBTYPE   | -202   | Invalid DB type                                             |

### SQL Parameter generation and conversion

Each DBMS uses a different set of placeholders for parameters in SQL statements. For instance:

- PostgreSQL uses a `$` prefix followed by a numeric index (e.g. `$1`, `$2`, etc.)
- Oracle uses a colon (`:`) followed by an alphanumeric identifier (e.g. `:param1`, `:client_no`, `:1`)
- ODBC only accepts "anonymous" parameters whose placeholder is a question mark (`?`)
- SQLite can use any of the above

GixSQL will generate the appropriate parameter placeholders for each case (using the `-z` parameter in gixpp):

- `-z d` : parameter placeholders are generated with a *dollar* prefix, followed by a numerical index  (e.g. `$1`, `$2`, etc.). This is the default.
- `-z a` : parameter placeholders are generated as *anonymous*  (`?`)
- `-z c` : parameter placeholders are generated with a *colon* prefix, followed by a numerical index (e.g. `:1`, `:2`, etc.)

This obviously does not apply to prepared statements, whose text is compiled (or generated) in the program code itself. This means that a statement like `SELECT mycol FROM mytab WHERE mykey = ?` will succeed on ODBC (if syntactically and semantically correct, of course) and fail on PostgreSQL, because the latter will not recognize the `?` placeholder. 

GixSQL can optionally convert all the parameter placeholders in a prepared statements to the ones used by the database driver. This feature can be activated in one of two ways:

- Pass the `fixup_params` options in a connection string when connecting (e.g. `pgsql://localhost/mydb?fixup_params=on`)
- set the `GIXSQL_FIXUP_PARAMS` to `on` or `1`

### Customization of SQLCODE on "no record found"

The return values for `SQLCODE`, as it is common knowledge, are not standard. Many DBMSs, when no data is found (e.g. when trying to fetch after the last row in a cursor has been reached, or when a `SELECT` statements returns no results) supply `100` as a return code when flagging this condition. While this behaviour is widespread, it is far from being a standard. Oracle, for instance, uses `1403`. GixSQL, in this condition, returns a (more standardized) `02000` for `SQLSTATE` and, until v1.0.18, used to return a fixed value of `100` in `SQLCODE`. From v1.0.18 onwards, this value can be customized, to conform to the standards used by the combination of your COBOL code and DBMS of choice. A custom code for the "no record found" condition can (actually should) be set in two different places:

- While preprocessing, by using the `--no-rec-code` option and adding a numeric value (e.g. `--no-rec-code 1403`). This instructs the preprocessor to correctly handle the `NOT FOUND` clauses in `EXEC SQL WHENEVER` statements.
- At runtime, by setting the `GIXSQL_NOREC_CODE` environment variable (e.g. `export GIXSQL_NOREC_CODE=1403` (Linux) or `set GIXSQL_NOREC_CODE=1403` (Windows)
- 
## Driver notes

### PostgreSQL

The PostgreSQL drivers implements two ways of dealing with cursors: "native" (the default one) and "emulated". Native cursors use the SQL language features provided by PostgreSQL (e.g. `DECLARE crsr CURSOR FOR...`, `FETCH NEXT`, etc.) to manage cursor operations. Emulated cursors, on the other hand, execute the `SELECT` statement provided in the cursor definition and handle the resultset internally. Native cursors are generally to be preferred: they are a bit (just a bit) faster and support dynamic (updatable) behaviour. On the negative side, they **must** be executed inside a transaction, so you will need to add at least a `START TRANSACTION`/`COMMIT` pair of statements to your programs, if they are not already there.

The PostgreSQL driver has specific options that can be added to the connection string:

- `default_schema`: used to set the default schema(s), maps directly to PostgreSQL's `search_path`
- `decode_binary` : binary (`bytea`) fields are decoded when their data is read into a COBOL field. The default is `on`. If, for any reason, you want to preserve the original encoding, set it to `off` in the connection string. Since encoded binary data takes more space, in this case you should make sure that your data fields are large enough to accommodate the data being read.

### ODBC
A lot of features in the ODBC driver depend in turn on the underlying driver (MySQL ODBC Connector, psqlODBC, etc.) and on its settings. Tests have usually been conducted using the latest versions of the available ODBC drivers (e.g. 13.x for PostgreSQL).

For instance: when using the PostgreSQL driver (psqlODBC) you should set the "rollback on error" option to "nop", to let GixSQL handle database errors. Otherwise the ODBC driver will always roll back the transaction in case of errors and not allow your program to handle errors, roll back to savepoints, etc. 

### Oracle

The Oracle driver currently supports connecting only with a service name (e.g. `oracle://<oracle host>/<service name>/`), no SID. Other connection options and parameters will be added in the future.

### SQLite

The connection string for SQLite databases directly encodes the filename, e.g.:
- `sqlite:///home/user/mydb.db` 
- `sqlite://c:/Users/myuser/mydb.db`
As usual with SQLite, if the SQLite file does not exist, it will be created. Currently no options are available for the SQLite driver.
