@echo off

:: This script should only to be used to test the InnoSetup script
:: It does not include the MinGW version(s) of GixSQL

set DIST_DIR=%TEMP%\gix-dist
set INCLUDE_COMPILERS=0
set HOST_PLATFORM=x64
set MSBUILD_PLATFORM=x64
set WORKSPACE=C:\Users\%USERNAME%\source\repos\gix-ide
set QTDIR=C:\Qt\5.14.2\msvc2017_64
set MSVC_BUILD_TOOLS=https://aka.ms/vs/17/release/vs_BuildTools.exe
set MSVC_RUNTIME_X86=https://aka.ms/vs/17/release/vc_redist.x86.exe
set MSVC_RUNTIME_X64=https://aka.ms/vs/17/release/vc_redist.x64.exe
set GIXIDEMAJ=1
set GIXIDEMIN=1
set GIXIDEREL=1
set GIX_REVISION=9999
set GIXSQLMAJ=1
set GIXSQLMIN=0
set GIXSQLREL=19
set DEFAULT_VS_COMPILER=
set DEFAULT_GCC_COMPILER=
set MSVCRT_PATH_X86=C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Redist\MSVC\14.34.31931\x86\Microsoft.VC143.CRT
set MSVCRT_PATH_X64=C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Redist\MSVC\14.34.31931\x64\Microsoft.VC143.CRT

set GIX_IDE_X64_BIN_DIR=%WORKSPACE%\%MSBUILD_PLATFORM%\Release

mkdir %DIST_DIR%\bin
mkdir %DIST_DIR%\lib
mkdir %DIST_DIR%\lib\x64\msvc
mkdir %DIST_DIR%\lib\x64\gcc
mkdir %DIST_DIR%\lib\x86\msvc
mkdir %DIST_DIR%\lib\x86\gcc        
mkdir %DIST_DIR%\lib\copy    

mkdir %WORKSPACE%\redist\msvc\x86
mkdir %WORKSPACE%\redist\msvc\x64
copy "%MSVCRT_PATH_X86%\*.dll" %WORKSPACE%\redist\msvc\x86
copy "%MSVCRT_PATH_X64%\*.dll" %WORKSPACE%\redist\msvc\x64

copy %GIX_IDE_X64_BIN_DIR%\gixpp.exe %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\gix-http.exe %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\gix-ide.exe %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\gixdbgr.exe %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\gixpp.exe %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\gix-common.dll %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\nng.dll %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\ScintillaEdit4.dll %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\gix-debugger-x64.exe %DIST_DIR%\bin
:: copy ${{ env.GIX_IDE_X86_BIN_DIR }}\gix-debugger-x86.exe %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\libgixsql.lib %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\libgixsql.dll %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-mysql.dll %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-odbc.dll %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-pgsql.dll %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-oracle.dll %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-sqlite.dll %DIST_DIR%\bin
copy %GIX_IDE_X64_BIN_DIR%\libgixsql.lib %DIST_DIR%\lib\x64\msvc
copy %GIX_IDE_X64_BIN_DIR%\libgixsql.dll %DIST_DIR%\lib\x64\msvc
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-mysql.dll %DIST_DIR%\lib\x64\msvc
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-odbc.dll %DIST_DIR%\lib\x64\msvc
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-pgsql.dll %DIST_DIR%\lib\x64\msvc
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-oracle.dll %DIST_DIR%\lib\x64\msvc
copy %GIX_IDE_X64_BIN_DIR%\libgixsql-sqlite.dll %DIST_DIR%\lib\x64\msvc
:: copy ${{ env.GIX_IDE_X86_BIN_DIR }}\libgixsql.lib %DIST_DIR%\lib\x86\msvc
:: copy ${{ env.GIX_IDE_X86_BIN_DIR }}\libgixsql.dll %DIST_DIR%\lib\x86\msvc
:: copy ${{ env.GIX_IDE_X86_BIN_DIR }}\libgixsql-mysql.dll %DIST_DIR%\lib\x86\msvc
:: copy ${{ env.GIX_IDE_X86_BIN_DIR }}\libgixsql-odbc.dll %DIST_DIR%\lib\x86\msvc
:: copy ${{ env.GIX_IDE_X86_BIN_DIR }}\libgixsql-pgsql.dll %DIST_DIR%\lib\x86\msvc
:: copy ${{ env.GIX_IDE_X86_BIN_DIR }}\libgixsql-oracle.dll %DIST_DIR%\lib\x86\msvc
:: copy ${{ env.GIX_IDE_X86_BIN_DIR }}\libgixsql-sqlite.dll %DIST_DIR%\lib\x86\msvc        
:: copy ${{ env.GIXSQL_MINGW_X86_BIN_DIR }}\lib\libgixsql.a %DIST_DIR%\lib\x86\gcc
:: copy ${{ env.GIXSQL_MINGW_X86_BIN_DIR }}\bin\libgixsql.dll %DIST_DIR%\lib\x86\gcc
:: copy ${{ env.GIXSQL_MINGW_X86_BIN_DIR }}\bin\libgixsql-mysql.dll %DIST_DIR%\lib\x86\gcc
:: copy ${{ env.GIXSQL_MINGW_X86_BIN_DIR }}\bin\libgixsql-odbc.dll %DIST_DIR%\lib\x86\gcc
:: copy ${{ env.GIXSQL_MINGW_X86_BIN_DIR }}\bin\libgixsql-pgsql.dll %DIST_DIR%\lib\x86\gcc
:: copy ${{ env.GIXSQL_MINGW_X86_BIN_DIR }}\bin\libgixsql-oracle.dll %DIST_DIR%\lib\x86\gcc
:: copy ${{ env.GIXSQL_MINGW_X86_BIN_DIR }}\bin\libgixsql-sqlite.dll %DIST_DIR%\lib\x86\gcc        
:: copy ${{ env.GIXSQL_MINGW_X64_BIN_DIR }}\lib\libgixsql.a %DIST_DIR%\lib\x64\gcc
:: copy ${{ env.GIXSQL_MINGW_X64_BIN_DIR }}\bin\libgixsql.dll %DIST_DIR%\lib\x86\gcc
:: copy ${{ env.GIXSQL_MINGW_X64_BIN_DIR }}\bin\libgixsql-mysql.dll %DIST_DIR%\lib\x64\gcc
:: copy ${{ env.GIXSQL_MINGW_X64_BIN_DIR }}\bin\libgixsql-odbc.dll %DIST_DIR%\lib\x64\gcc
:: copy ${{ env.GIXSQL_MINGW_X64_BIN_DIR }}\bin\libgixsql-pgsql.dll %DIST_DIR%\lib\x64\gcc
:: copy ${{ env.GIXSQL_MINGW_X64_BIN_DIR }}\bin\libgixsql-oracle.dll %DIST_DIR%\lib\x64\gcc
:: copy ${{ env.GIXSQL_MINGW_X64_BIN_DIR }}\bin\libgixsql-sqlite.dll %DIST_DIR%\lib\x64\gcc      

echo "C:\Program Files (x86)\Inno Script Studio\ISStudio.exe"