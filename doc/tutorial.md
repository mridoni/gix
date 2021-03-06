# Gix-IDE - A brief tutorial

***Note: Gix-IDE is still undergoing heavy development: it is likely that some of the features and procedures described here will not work flawlessly. Should you find something that does not perform as described or expected, please file an issue.***

## Projects and Project Collections
In Gix-IDE GnuCOBOL modules and copy files are organized into "projects". A project is a logicaly entity that does not necessarily correspond to a compilation unit (DLL or EXE, in Windows parlance). *Please note that in this text, for the sake of brevity, I'll be referring to  the build type as EXE or DLL, but the same concepts also apply to the corresponding kinds of compilation units on other platforms (e.g. .so for DLLs under Linux/Unix)*. Gix-IDE projects can be one of three types:

 - **Single Binary Project**: the output of the project is a single DLL or EXE that contains one or more COBOL modules. Gix-IDE compiles the single modules and links them into a single unit. If we're building an EXE-based Single Binary Project, one of the modules must be designated as the "startup item".
 - **Multiple Binaries Project**: this kind of project aims at reducing the clutter in the interface when you have many programs of the same kind: each module in the project will be compiled as a single EXE or DLL, as indicated in the "Default Build Type" property. All the projects share the same properties. As an exception, individual modules can have a different build type than the rest of the project (e.g. in a Multiple Binaries Project you can have 20 called modules compiled as DLLs and a single EXE that is responsible for launching the other modules).
 - **Web Project**: this type of project builds modules that can be invoked as Web Services. Each module is compiled into a single compilation unit, always a DLL.

Project collections are groupings of one or more projects.

## The interface
This is Gix-IDE during a debugging session.![Gix_IDe during a debugging session.](https://www.mediumgray.info/img/ss-gix-commented-01.png)

The toolbar contains several groups of buttons:


 - **Load/Save**: used to load and save project collections, projects and files.
 - **Configuration/Platform**: used to select the current configuration (normally Release or Debug) and platform. The availability of different platforms (e.g. x86, x64) depends on the compiler selected: while some installs of GnuCOBOL can target both x86 and x64, for instance, others can only target one architecture at a time.
 - **Run/Debug**: used to run or debug a project, stop debugging, step through the code, etc.
 - **Search/Replace**: text search and replace in the currently selected editor window
 - **Bookmarks**: navigate the code using bookmarks

 The dockable window areas are:
  - **Project Collection**: provides a treeview of the currently loaded project collection
  - **Properties**: list and allows editing of the properties pertaining to the item currently selected in the project collection window
  - Dependencies: list COPY file dependencies for the currently active editor window
  - **Navigator**: lists and allow navigation of the paragraphs in the currently displayed editor window
  - **DB Manager**: allows to connect to a DB and perform some basic operations (inspect schema, generate a COPY file from a table, etc.)
  - **Output**: the IDE output (e.g. compile messages, error messages, diagnostics, etc.)
  - **Watch** (only active when debugging): allows to inspect variables while debugging
  - **Data Section**: allows to navigate the data section of the module in the currently displayed editor window
  - **Console**: a write-only console (no keyboard input) for displaying the output of the currently running process.

