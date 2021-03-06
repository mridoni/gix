# Building and/or installing Gix-IDE

***Note: the main development - for the moment - takes place on Windows with Visual Studio, mostly to make the work easier and faster. This means that while the intention is to have full cross-platform support, both with regard to feature parity and stability, for obvious reasons the Windows+MingW and Linux builds can, at a given point in time, lack a feature, or display some quirk or instability.***

## Installing from a binary package

Binary packages are currently available for Windows only (Linux packages will be available shortly) and come in two flavors: one that includes a set of GnuCOBOL compilers (in Visual Studio and MingW/GCC varieties) and a "barebone" package that only installs the IDE and tools. The latter version of the package can be customized to point at your current GnuCOBOL installation, if so desired, or to a compiler install of your choice. The packages can be downloaded from the [Releases](https://github.com/mridoni/gix/releases) page on GitHub.

The installer places Gix-IDE in C:\Program Files and creates a data directory, either in C:\ProgramData or in %LOCALAPPDATA%, where it stores the compilers (if provided with the installer) and the compiler definition files.

The compilers available on [Arnold Trembley's page](https://www.arnoldtrembley.com/GnuCOBOL.htm) have been successfully tested with Gix-IDE.

## Building from source

### Windows,, with Visual Studio or Visual Studio Tools

 - Download the [Qt GPL SDK](https://www.qt.io/download-qt-installer) and install it, selecting the "msvc" package (the VS2017 version works perfectly under VS2019). Gix-IDE uses 5.14.2, so that version is recommended, but a newer version should do as well.
 - If you don't have Visual Studio installed on your computer download
   and install Visual Studio C++ Build Tools 2019 from here:
   https://visualstudio.microsoft.com/it/thank-you-downloading-visual-studio/?sku=BuildTools&rel=16#
   or install Visual Studio Community. While the Build Tools are enough
   to build Gix-IDE, having the full VS IDE available will obviously
   make applying small fies to solution and project files (e.g. paths,
   environment variables) much easier.  Download the binary
   prerequisites and uncompress them in a folder:
	 - [MySQL x64 binaries](https://dev.mysql.com/get/Downloads/MySQL-5.7/mysql-5.7.31-winx64.zip)
   	 - [PostgreSQL x64 binaries](https://sbp.enterprisedb.com/getfile.jsp?fileid=1257551&_ga=2.17284795.341452640.1615031602-1234917009.1613646523)
 You can probably use newer versions without problems, the versions linked are the ones currently in use to build the binary packages. Since we are only using the client libraries and headers, older versions should not be a problem.
 - Clone the repository or download the snapshot
 - You will need the **Qt Visual Studio Tools**:  unfortunately they are only available as a VSIX extension, so if you are using the VS C++ Build Tools (no full VS), go into the "build-tools" directory and extract the QtMsBuild.zip archive somewhere (this a repackaged version of the Qt Visual Studio Tools). If you are using the full Visual Studio IDE, just download and install the [Qt Visual Studio Tools](https://marketplace.visualstudio.com/items?itemName=TheQtCompany.QtVisualStudioTools2019) (follow the link or use The Extension Manager in Visual Studio).
 - If you are using the VS C++ Build Tools, you will have to manually download the nuget.exe executable from [here](https://www.nuget.org/downloads).
   
 - Set up a few environment variables (if you are using the full Visual Studio IDE, obviously do that in Windows **before** launching Visual Studio) like in the example below (obviously using the correct paths:
	- MYSQL_HOME=C:\mysql-5.7.31-winx64
	- PGSQL_HOME=C:\pgsql-9.5.22-x64
	- MYSQL32_HOME=C:\mysql-5.7.31-win32
	- PGSQL32_HOME=C:\pgsql-9.5.23-x86
	- GNUCOBOL_HOME=C:\GnuCOBOL-2.2
	- QTDIR=C:\Qt\5.14.2\msvc2017_64
	- QtMsBuild=C:\Users\gix-builder\QtMsBuild
	- QtToolsPath=C:\Qt\5.14.2\msvc2017_64\bin
	- NUGET_EXE=C:\Users\gix-builder\nuget.exe
**Note**:
		- the *32_HOME variables are only used to build 32-bit versions of the ESQL libraries (useful if you want to target x86 beside x64). If you're targeting x64 only you can safely ignore them. If you are using them, you will also need to download x86 binary versions of MySQL and PostgreSQL
		- GNUCOBOL_HOME must point to an existing install of GnuCOBOL
		
- Open a Visual Studio "Developer Command Prompt" and navigate to the directory where you have uncompressed the Gix-IDE source.
- Run `<path to nuget.exe>\nuget.exe restore` (this should download and install a NuGet Package)
- Run `msbuild gix-ide.sln /p:Configuration=Release /p:Platform=x64` to build Gix-IDE

If all goes well, you should find the binaries in the **x64** directory.

### Windows, with MingW64/MSYS2

 - Gix-IDE uses C++17 features, so ensure your GCC version is "not really old"
 - Download and install the Qt SDK (same versions as above)
 - Clone or download the repository
 - Set up the environment variables as above (you cam obviously ignore QtMsBuild and QtToolsPath)
 - run `qmake` (or `qmake CONFIG+=debug` if you want to build a debug-enabled version)
 - run make

If all goes well, you should find the binaries in the **x64/Release** directory, the main executable is **gix-ide.exe**. 

You might need to add Qt's library path to your PATH variable, in order to launch gix-ide.

### Linux
 - Download and install the Qt SDK (same versions as above)
 - Clone or download the repository
 - Set up the environment variables as above (you cam obviously ignore QtMsBuild and QtToolsPath)
 - run `qmake` (or `qmake CONFIG+=debug` if you want to build a debug-enabled version)
 - run make

If all goes well, you should find the binaries in the **x64/Release** directory, the main executable is **gix-ide**.

You might need to add Qt's library path to your PATH variable, in order to launch gix-ide.


