MYSQL_HOME=C:\mysql-5.7.31-winx64
PGSQL_HOME=C:\pgsql-9.5.22
GNUCOBOL_HOME=C:\GnuCOBOL-2.2
MINGW32_HOME=C:\MingW
QTDIR=C:\Qt\5.14.2\msvc2017_64
QtMsBuild=C:\Users\gix-builder\QtMsBuild
QtToolsPath=C:\Qt\5.14.2\msvc2017_64\bin
NUGET_EXE=C:\Users\gix-builder\nuget.exe
REM *******************************
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\Common7\Tools\VsDevCmd.bat"
C:\Users\gix-builder\nuget.exe restore gix-ide.sln
REM *******************************
@echo off

REM set WORKSPACE=C:\Users\gix-builder\Desktop\gix-ide
REM set BUILD_NUMBER=99

REM rmdir /Q /S %WORKSPACE%\deploy\compilers

REM ---------------------
set DOWNLOAD_DIR=C:\inetpub\gix-download
set QTDIR=C:\Qt\5.14.2\msvc2017_64
set INNO_SETUP_EXE=C:\Program Files (x86)\Inno Setup 6\iscc.exe

mkdir %WORKSPACE%\deploy\compilers
if NOT %ERRORLEVEL% EQU 0 goto exiterr

cd %WORKSPACE%\deploy\compilers
if NOT %ERRORLEVEL% EQU 0 goto exiterr

svn co file:///c:/Repositories/gix-compiler-pkgs/dist/windows/msvc .
if NOT %ERRORLEVEL% EQU 0 goto exiterr

del /Q /S /F /AH .svn
if NOT %ERRORLEVEL% EQU 0 goto exiterr

rmdir /Q /S .svn
if NOT %ERRORLEVEL% EQU 0 goto exiterr

"%INNO_SETUP_EXE%" %WORKSPACE%\deploy\installers\msvc-x64\gix-ide.iss
if NOT %ERRORLEVEL% EQU 0 goto exiterr

copy %WORKSPACE%\deploy\installers\msvc-x64\Gix-IDE-1.0.0-installer.exe %DOWNLOAD_DIR%
if NOT %ERRORLEVEL% EQU 0 goto exiterr

exit /B 0

:exiterr

exit /B 1