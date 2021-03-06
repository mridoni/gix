# Gix-IDE - A brief tutorial

***Note: Gix-IDE is still undergoing heavy development: it is likely that some of the features and procedures described here will not work flawlessly. Should you find something that does not perform as described or expected, please file an issue.***

## Projects and Project Collections
In Gix-IDE GnuCOBOL modules and copy files are organized into "projects". A project is a logical entity that does not necessarily correspond to a compilation unit (DLL or EXE, in Windows parlance). *Please note that in this text, for the sake of brevity, I'll be referring to  the build type as EXE or DLL, but the same concepts also apply to the corresponding kinds of compilation units on other platforms (e.g. .so for DLLs under Linux/Unix)*. Gix-IDE projects can be one of three types:

 - **Single Binary Project**: the output of the project is a single DLL or EXE that contains one or more COBOL modules. Gix-IDE compiles the single modules and links them into a single unit. If we're building an EXE-based Single Binary Project, one of the modules must be designated as the "startup item".
 - **Multiple Binaries Project**: this kind of project aims at reducing the clutter in the interface when you have many programs of the same kind: each module in the project will be compiled as a single EXE or DLL, as indicated in the "Default Build Type" property. All the projects share the same properties. As an exception, individual modules can have a different build type than the rest of the project (e.g. in a Multiple Binaries Project you can have 20 called modules compiled as DLLs and a single EXE that is responsible for launching the other modules).
 - **Web Project**: this type of project builds modules that can be invoked as Web Services. Each module is compiled into a single compilation unit, always a DLL.

Project collections are groupings of one or more projects. In Gix-IDE you work on project collections, that contain one or more projects of the same or different types.
Project collections are files with a ".gix". Beside each project collection file there is a folder which contains a single project, that in turn resides in a ".gixprj" file along all its files:  
![Gix project tree](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/gix-prjtree.png)

As you can see in the image, a "bin" directory is present under the project directory. This is created by Gix-IDE during the compilation process for a given configuration/platform combination, and used to store intermediate files.
There are also ".gixstate" files, not present in this example, that contain the state of the project (open windows, breakpoints, etc.) for a given project.

## The interface
This is Gix-IDE during a debugging session:  
![Gix-IDE during a debugging session.](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/ss-gix-commented-01.png)

The toolbar contains several groups of buttons:

 - **Load/Save**: used to load and save project collections, projects and files.
 - **Configuration/Platform**: used to select the current configuration (normally Release or Debug) and platform. The availability of different platforms (e.g. x86, x64) depends on the compiler selected: while some installs of GnuCOBOL can target both x86 and x64, for instance, others can only target one architecture at a time.
 - **Run/Debug**: used to run or debug a project, stop debugging, step through the code, etc.
 - **Search/Replace**: text search and replace in the currently selected editor window
 - **Bookmarks**: navigate the code using bookmarks

 The dockable window areas are:
  - **Project Collection**: provides a tree view of the currently loaded project collection
  - **Properties**: list and allows editing of the properties pertaining to the item currently selected in the project collection window
  - Dependencies: list COPY file dependencies for the currently active editor window
  - **Navigator**: lists and allow navigation of the paragraphs in the currently displayed editor window. You can "go to" a paragraph definition by double-clicking on it.
  - **DB Manager**: allows to connect to a DB and perform some basic operations (inspect schema, generate a COPY file from a table, etc.)
  - **Output**: the IDE output (e.g. compile messages, error messages, diagnostics, etc.)
  - **Watch** (only active when debugging): allows to inspect variables while debugging
  - **Data Section**: allows to browse and navigate the data section of the module in the currently displayed editor window. You can "go to" a symbol definition by double-clicking on it.
  - **Console**: a write-only console (no keyboard input) for displaying the output of the currently running process.

## Settings

### Setting up compilers
If you have installed a binary package with included compilers, unless you want to change the default compiler used by Gix-IDE, you can start immediately. Otherwise, click on the "gear" icon on the toolbar and bring up the settings window:  
![Settings Window](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/gix-settings-01.png)

Since you are already there, check the "Show debug output" option, at least for now: Gix-IDE is still young, and though a bit noisy, this will help to trace problems that could occur.

Now go to "GnuCOBOL" tab. This is where you can set up compilers. You can set up a compiler for Release builds and one for Debug builds. If you installed from a binary package with included compilers, just the compiler install you would like to use:  
![Settings -> Compilers](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/gix-settings-02-compilers.png)

If you want to add a new compiler install to those available, click on the "Add a compiler" button.  
![Settings -> Compilers -> Add Compiler](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/gix-settings-03-add-compiler.png)
Fill in all the fields, then click on the "+" button to add at least one platform (e.g. x64).

**Notes**:
 - You must choose a base directory before adding a platform
 - The version field must be in the X.Y. or in X.Y.Z format (e.g. 2.2 or 3.1.2). Gix-IDE does not check if the version you entered corresponds to the one actually present in the compiler directory.
 - You must add at least one platform. Obviously the compiler must support it (and before anyone asks, ARM is there as a wishful thinking moment).
 
In the platform window, fill all the directory fields (each corresponding to a directory in a GnuCOBOL install. Depending on how the compiler was built, some directories can be in a not-so-obvious location (e.g. "config" and "copy" might be under share/gnucobol, in the base directory tree):  
![Settings -> Compilers -> compiler platform](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/gix-settings-04-add-compiler-platform.png)

Now click OK, select your newly-added compiler in the Release and/or Debug drop-down menu, and click OK again to save the settings. Since you have already selected the "Show debug output" option, you should see the new compiler being examined in the output window, where you should check for any errors.

## Compiling a project

Select "Open Project/Project Collection" and navigate to your Documents folder. You will find a "Gix" directory, and inside that, a directory named "examples". If you have opted to build from source, the "examples" folder will be inside the "deploy" directory under the main source tree.
Open the "test000.gix" and click on the "Build" button on the toolbar. The project should build without errors and the resulting binaries should be inside the build directory under your project directory (e.g. test000/bin/debug/x64).

## Debugging a project

First build your project, than open in the editor the module you want to debug and set one or more breakpoints, by clicking on the bar between the line numbers and the left margin of the editor window.  
![Debugging: Breakpoint](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/gix-set_brkp.png)

Then click on the "Debug" button on the toolbar, The program should start and then stop on your breakpoint.  
![Debugging: wait on breakpoint](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/gix-dbgr-01.png)

Now you can use the "Watch" window to add variables to be inspected while you debug the program. These variables, as the breakpoints, will be preserved when you stop and restart the debugging, or when you close and then re-open a project.

You can also navigate the Working Storage section in the "Data Section" window and select "Add to watch" from there.  
![Debugging: select variable for watch](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/gix-dbgr-add-to-watch.png)

If your module is a simple batch program that just output a few display statements, you might choose to display its output in the integrated console, by setting the "Run in separate console" option in the "Debug" section of your project properties to "No".  
![Debugging: run in internal console](https://raw.githubusercontent.com/mridoni/gix/main/doc/img/gix-dbgr-console.png)

To step through your code use F10. To run the code until the next breakpoint (or until the program ends), press F8. You can stop the debugging at any moment by clicking on the "Stop Debug" button in the toolbar.
