@echo off

:: Check these variables and in case adjust them depending on your environment
set HOSTNAME=x64
set QTDIR=C:\Qt\5.14.2\msvc2017_64
set VCPKG_ROOT=C:\VCPKG
:: Check these variables (end)

:: These indicates the current version of Gix-IDE
set GIXIDEMAJ=1
set GIXIDEMIN=1
set GIXIDEREL=0dev1
set GIX_REVISION=1922
:: These indicates the current version of Gix-IDE (end)

:: These indicates the current version of GixSQL included in Gix-IDE
set GIXSQLMAJ=1
set GIXSQLMIN=0
set GIXSQLREL=18b
:: These indicates the current version of GixSQL included in Gix-IDE (end)

SET MYPATH=%~dp0
set SCRIPT_DIR=%MYPATH:~0,-1%

REM set version in resource file
echo Configuring version (%GIXIDEMAJ%.%GIXIDEMIN%.%GIXIDEREL%-%GIX_REVISION%) in resource file
del /Q %SCRIPT_DIR%\gix-ide\gix-ide.rc > NUL 2>&1
copy %SCRIPT_DIR%\gix-ide\gix-ide.rc.tpl %SCRIPT_DIR%\gix-ide\gix-ide.rc > NUL
call %SCRIPT_DIR%\build-tools\jrepl.bat "{GIXIDEMAJ}" "%GIXIDEMAJ%" /F %SCRIPT_DIR%\gix-ide\gix-ide.rc /O -
call %SCRIPT_DIR%\build-tools\jrepl.bat "{GIXIDEMIN}" "%GIXIDEMIN%" /F %SCRIPT_DIR%\gix-ide\gix-ide.rc /O -
call %SCRIPT_DIR%\build-tools\jrepl.bat "{GIXIDEREL}" "%GIXIDEREL%" /F %SCRIPT_DIR%\gix-ide\gix-ide.rc  /O -
call %SCRIPT_DIR%\build-tools\jrepl.bat "{GIXIDEBLD}" "%GIX_REVISION%" /F %SCRIPT_DIR%\gix-ide\gix-ide.rc /O -

REM set version in header file
echo Configuring version (%GIXIDEMAJ%.%GIXIDEMIN%.%GIXIDEREL%-%GIX_REVISION%) in header file
del /Q %SCRIPT_DIR%\gix-ide\GixVersion.h > NUL 2>&1
copy %SCRIPT_DIR%\gix-ide\GixVersion.h.tpl %SCRIPT_DIR%\gix-ide\GixVersion.h > NUL
call %SCRIPT_DIR%\build-tools\jrepl.bat "{GIXIDEMAJ}" "%GIXIDEMAJ%" /F %SCRIPT_DIR%\gix-ide\GixVersion.h /O -
call %SCRIPT_DIR%\build-tools\jrepl.bat "{GIXIDEMIN}" "%GIXIDEMIN%" /F %SCRIPT_DIR%\gix-ide\GixVersion.h /O -
call %SCRIPT_DIR%\build-tools\jrepl.bat "{GIXIDEREL}" "%GIXIDEREL%" /F %SCRIPT_DIR%\gix-ide\GixVersion.h /O -
call %SCRIPT_DIR%\build-tools\jrepl.bat "{GIXIDEBLD}" "%GIX_REVISION%" /F %SCRIPT_DIR%\gix-ide\GixVersion.h  /O -

echo Configuring version (%GIXSQLMAJ%.%GIXSQLMIN%.%GIXSQLREL%) in header file for GixSQL
echo #define VERSION "%GIXSQLMAJ%.%GIXSQLMIN%.%GIXSQLREL%" > %SCRIPT_DIR%\gixsql\config.h

echo Configuring libdwarf for Visual Studio"
copy %SCRIPT_DIR%\libs\libdwarf\config.h.msvc %SCRIPT_DIR%\libs\libdwarf\config.h > NUL

:: we only need to copy this if compiling with QMake/MinGW, but we copy them just in case
echo Configuring GixSQL for QMake
copy %SCRIPT_DIR%\build-tools\gixsql-pro\gixsql.pro %SCRIPT_DIR%\gixsql\ > NUL
copy %SCRIPT_DIR%\build-tools\gixsql-pro\gixpp.pro %SCRIPT_DIR%\gixsql\gixpp\ > NUL
copy %SCRIPT_DIR%\build-tools\gixsql-pro\libcpputils.pro %SCRIPT_DIR%\gixsql\libcpputils\ > NUL
copy %SCRIPT_DIR%\build-tools\gixsql-pro\libgixpp.pro %SCRIPT_DIR%\gixsql\libgixpp\ > NUL

copy %SCRIPT_DIR%\build-tools\gixsql-pro\libgixsql.pro %SCRIPT_DIR%\gixsql\runtime\libgixsql\ > NUL
copy %SCRIPT_DIR%\build-tools\gixsql-pro\libgixsql-mysql.pro %SCRIPT_DIR%\gixsql\runtime\libgixsql-mysql\ > NUL
copy %SCRIPT_DIR%\build-tools\gixsql-pro\libgixsql-odbc.pro %SCRIPT_DIR%\gixsql\runtime\libgixsql-odbc\ > NUL
copy %SCRIPT_DIR%\build-tools\gixsql-pro\libgixsql-oracle.pro %SCRIPT_DIR%\gixsql\runtime\libgixsql-oracle\ > NUL
copy %SCRIPT_DIR%\build-tools\gixsql-pro\libgixsql-pgsql.pro %SCRIPT_DIR%\gixsql\runtime\libgixsql-pgsql\ > NUL
copy %SCRIPT_DIR%\build-tools\gixsql-pro\libgixsql-sqlite.pro %SCRIPT_DIR%\gixsql\runtime\libgixsql-sqlite\ > NUL

echo Done