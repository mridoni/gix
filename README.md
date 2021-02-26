# gix
Gix-IDE (or Gix) is an IDE and platform for GnuCOBOL, providing a native debugger, ESQL and HTTP REST services.

## Current state, development and plans for the future
Gix-IDE is "mostly working" but it probably has many bugs lurking below the surface (or not really below). I want to improve it, make it more stable and add more features (e.g.  reintegrating macOS and Linux support - including a native debugger, adding SOAP support, a better DB manager)

I am releasing it because, in order to do this, I need to put it out in the wild and find some form of sponsorship to help the development along.

### The IDE
The centerpiece is a Qt-based IDE that provides syntax highlighting (thanks to Scintilla), support for different versions of the compiler, a DB manager, variable navigation and integrated debugging.

### Native debugging
Gix-IDE incorporates a custom module that provides native debugging (only supported with GnuCOBOL 3.1+) including breakpoints, stepping, variable inspection of both executable and callable modules.

### ESQL support
Gix-IDE provides an ESQL preprocessor and drivers for ODBC, MySQL and PostgreSQL. The preprocessor has been developed starting from OceSQL, but has been almost completely rewritten in its last iterations. It has been at the center of several production applications for a few years now.

### HTTP REST Services
An HTTP server is provided that can natively call GnuCOBOL modules, using HTTP methods and JSON for input/output. Such modules can be run and debugged from inside the IDE.

### DB management
Gix-IDE also incorporates a DB manager that can generate COPY files from a DB connection (DCLGEN-style) and look at basic table/field properties.

### Compiler packages
While Gix-IDE runs standard GnuCOBOL versions that can be added to the install, the binary installer also provides pre-packaged version of the compilers, in Visual Studio and MingW versions.

### Multi-platform support
While the focus of Gix-IDE is currently on Windows, both macOS and Linux versions of the IDE and the tools have been successfully built and run,

### License
Gix-IDE is GPL 2 licensed, except for runtime components (essentially the ESQL libraries and the HTTP server) that fall under LGPL.
