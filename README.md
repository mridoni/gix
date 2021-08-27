﻿# Gix-IDE
Gix-IDE (or Gix) is an IDE and platform for GnuCOBOL, providing a native debugger, ESQL and HTTP REST services.

## Current state, development and plans for the future
Gix-IDE is "mostly working" but it probably has many bugs lurking below the surface (or not really below) and several features missing. I want to improve it, make it more stable and add more features (e.g.  reintegrating macOS support - including a native debugger, adding SOAP support, a better DB manager); and there is a lot of documentation to write...

I am releasing it because, in order to do this, I need to put it out in the wild, to get some feedback, but also to find some form of sponsorship to help the development along.

### The IDE
The centerpiece is a Qt-based IDE that provides syntax highlighting (thanks to Scintilla), support for different versions of the compiler, a DB manager, variable navigation and integrated native debugging.

### Native debugging
Gix-IDE incorporates a custom module that provides native debugging (only supported with GnuCOBOL 3.1+) including breakpoints, stepping, variable inspection of both executable and callable modules.

### ESQL support
Gix-IDE provides an ESQL preprocessor and drivers for ODBC, MySQL and PostgreSQL. The preprocessor has been developed starting from OceSQL, but has been almost completely rewritten in its last iterations. It has been at the center of several production applications for a few years now. The ESQL subsystem, though having been rewritten and refactored for the most part, started as a (private) fork of [OceSQL](https://github.com/opensourcecobol/Open-COBOL-ESQL).

### HTTP REST Services
An HTTP server is provided that can natively call GnuCOBOL modules, using HTTP methods and JSON for input/output. Such modules can be run and debugged from inside the IDE. This module (named gix-http) is largely based on [QtWebApp](http://stefanfrings.de/qtwebapp/index-en.html) by Stefan Frings.

### DB management
Gix-IDE also incorporates a DB manager that can generate COPY files from a DB connection (DCLGEN-style) and look at basic table/field properties.

### Compiler packages
While Gix-IDE runs standard GnuCOBOL versions that can be added to the install, the binary installer also provides pre-packaged version of the compilers, in Visual Studio and MingW versions.

### Multi-platform support
While the focus of Gix-IDE is currently on Windows, a Linux versions of the IDE can be successfully built and run,

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
