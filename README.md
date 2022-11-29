# Gix-IDE
Gix-IDE (or Gix) is an IDE and platform for GnuCOBOL, providing a native debugger, ESQL and HTTP REST services.

_**For updated news and current developments [look here](https://mridoni.github.io/) on my development blog.**_

## Current state, development and plans for the future
Gix-IDE is "mostly working" but it probably has many bugs lurking below the surface (or not really below) and several features missing. I want to improve it, make it more stable and add more features (e.g.  reintegrating macOS support - including a native debugger, adding SOAP support, a better DB manager); and there is a lot of documentation to write...

It was releaed because, in order to do this, I need to put it out in the wild, to get some feedback, but also to find some form of sponsorship to help the development along.

### The IDE
The centerpiece is a Qt-based IDE that provides syntax highlighting (thanks to Scintilla), support for different versions of the compiler, a DB manager,  navigation of COBOL fields and an integrated native debugger.

### Native debugging
Gix-IDE incorporates a custom module that provides native debugging (only supported with GnuCOBOL 3.1+) including breakpoints, stepping, variable inspection of both executable and callable modules.

Starting from version 1.1.0 a new debugger is available (currently labeled "Experimental" that will allow advanced fetures like attaching to a process and performing remote debugging).

### ESQL support
Gix-IDE ships with GixSQL, an ESQL preprocessor and drivers for ODBC, MySQL and PostgreSQL. The preprocessor has been developed starting from OceSQL, but has been almost completely rewritten in its last iterations. It has been at the center of several production applications for a few years now. The ESQL subsystem, though having been rewritten and refactored for the most part, started as a (private) fork of [OceSQL](https://github.com/opensourcecobol/Open-COBOL-ESQL). GixSQL is bundled with Gix-IDE but is also available in a [separate repository](https://github.com/mridoni/gixsql).

### HTTP REST Services
An HTTP server is provided that can natively call GnuCOBOL modules, using HTTP methods and JSON for input/output. Such modules can be run and debugged from inside the IDE. This module (named gix-http) is largely based on [QtWebApp](http://stefanfrings.de/qtwebapp/index-en.html) by Stefan Frings.

### DB management
Gix-IDE also incorporates a DB manager that can generate COPY files from a DB connection (DCLGEN-style) and look at basic table/field properties.

### Compiler packages
While Gix-IDE runs standard GnuCOBOL versions that can be added to the install, the binary installer also provides the ability to download pre-packaged version of the compilers, in Visual Studio and MinGW versions. The compiler packages are also [available separately](https://github.com/mridoni/gnucobol-binaries), to be used outside Gix-IDE.

### Multi-platform support
Gix-IDE is mainly developed on Windows, but a Linux versions of the IDE is available (including native debugging support), including Ubuntu (20.04LTS and 22.04 LTS) packages.

### License
Gix-IDE is GPL 3 licensed, with some exceptions:

 - The ESQL subsystem runtime: libgixsql (main library) and all the libgixsql-* runtime libraries (DB-specific drivers) are LGPL 3+
  - gix-http is licensed as LGPL3+
  - libdwarf is LGPL 2.1
  - Scintilla comes under its own GPL-compatible license

## Documentation

 - [Building and/or installing](doc/building_and_installing.md)
 - [Tutorial](doc/tutorial.md)
 - [GixSQL](doc/gixsql.md)
 - [Web modules](doc/web_modules.md)

 