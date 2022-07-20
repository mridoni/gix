#define QTDIR GetEnv('QTDIR')
#define WORKSPACE GetEnv('WORKSPACE')
#define GIX_REVISION GetEnv('GIX_REVISION')
#define VER_GIXSQLMAJ GetEnv('GIXSQLMAJ')
#define VER_GIXSQLMIN GetEnv('GIXSQLMIN')
#define VER_GIXSQLREL GetEnv('GIXSQLREL')

#define REDIST_DIR GetEnv('REDIST_DIR')

#define DIST_DIR GetEnv('DIST_DIR')

#define INCLUDE_VS GetEnv('INCLUDE_VS')

#define CONFIG "Release"
#define HOST_PLATFORM GetEnv('HOST_PLATFORM')

; We try to avoid problems with x86 defined as Win32 in MSVC
#if "x64" == HOST_PLATFORM
#define DEST_PLATFORM "x64"
#define SECONDARY_PLATFORM "x86"
#else
#define DEST_PLATFORM "x86"
#define SECONDARY_PLATFORM ""
#endif

[Setup]
AppName=GixSQL
AppVersion={#VER_GIXSQLMAJ}.{#VER_GIXSQLMIN}.{#VER_GIXSQLREL}-{#GIX_REVISION}
AppCopyright=Marco Ridoni
DefaultDirName={pf}\GixSQL
OutputDir={#WORKSPACE}\deploy\installers\gixsql-{#HOST_PLATFORM}
OutputBaseFilename=gixsql-{#VER_GIXSQLMAJ}.{#VER_GIXSQLMIN}.{#VER_GIXSQLREL}-{#GIX_REVISION}-installer
ArchitecturesInstallIn64BitMode=x64
DefaultGroupName=GixSQL
LicenseFile={#WORKSPACE}\LICENSE
RestartIfNeededByRun=False
DisableWelcomePage=False

[Files]
; main binaries
Source: "{#DIST_DIR}\bin\gixpp.exe"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#DIST_DIR}\lib\*"; DestDir: "{app}\lib"; Flags: ignoreversion createallsubdirs recursesubdirs

; COPY files
Source: "{#WORKSPACE}\copy\SQLCA.cpy"; DestDir: "{app}\lib\copy"; Flags: ignoreversion createallsubdirs recursesubdirs

; examples and docs
Source: "{#WORKSPACE}\gixsql-tests\data\*.cbl"; DestDir: "{userdocs}\GixSQL\Examples"; Flags: ignoreversion createallsubdirs recursesubdirs onlyifdoesntexist
Source: "{#WORKSPACE}\gixsql-tests\data\*.cpy"; DestDir: "{userdocs}\GixSQL\Examples"; Flags: ignoreversion createallsubdirs recursesubdirs onlyifdoesntexist
Source: "{#WORKSPACE}\README"; DestDir: "{userdocs}\GixSQL\Documentation"; Flags: ignoreversion

; MS runtimes
;Source: "{#REDIST_DIR}\ms\common\*"; DestDir: "{tmp}\redist\ms\common"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#REDIST_DIR}\ms\x86\*"; DestDir: "{tmp}\redist\ms\x86"; Flags: ignoreversion createallsubdirs recursesubdirs
#if "x64" == HOST_PLATFORM
Source: "{#REDIST_DIR}\ms\x64\*"; DestDir: "{tmp}\redist\ms\x64"; Flags: ignoreversion createallsubdirs recursesubdirs
#endif

; dependencies for DB runtime libraries
#if "x64" == HOST_PLATFORM
#if "1" == INCLUDE_VS
Source: "{#REDIST_DIR}\pgsql\x64\msvc\*"; DestDir: "{app}\lib\x64\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#REDIST_DIR}\mysql\x64\msvc\*"; DestDir: "{app}\lib\x64\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
#endif
Source: "{#REDIST_DIR}\pgsql\x64\gcc\*"; DestDir: "{app}\lib\x64\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#REDIST_DIR}\mysql\x64\gcc\*"; DestDir: "{app}\lib\x64\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
#endif

Source: "{#REDIST_DIR}\pgsql\x86\msvc\*"; DestDir: "{app}\lib\x86\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#REDIST_DIR}\pgsql\x86\gcc\*"; DestDir: "{app}\lib\x86\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist

Source: "{#REDIST_DIR}\mysql\x86\msvc\*"; DestDir: "{app}\lib\x86\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#REDIST_DIR}\mysql\x86\gcc\*"; DestDir: "{app}\lib\x86\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist

[Run]
#if "1" == INCLUDE_VS
#if "x64" == HOST_PLATFORM
Filename: "{tmp}\redist\ms\x64\VC_redist.x64.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}\redist\ms\x64\"; Flags: waituntilterminated; Description: "Visual C++ 2019 redistributable package (x64)"
#endif
Filename: "{tmp}\redist\ms\x86\VC_redist.x86.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}\redist\ms\x86\"; Flags: waituntilterminated; Description: "Visual C++ 2019 redistributable package (x86)"
; Filename: "{tmp}\redist\ms\x86\vcredist_x86_vs2013.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}\redist\ms\x86\"; Flags: waituntilterminated; Description: "Visual C++ 2013 redistributable package (x86)"
#endif

[Registry]
Root: "HKLM"; Subkey: "Software\MediumGray\gixsql"; ValueType: string; ValueName: "version"; ValueData: "{#VER_GIXSQLMAJ}.{#VER_GIXSQLMIN}.{#VER_GIXSQLREL}-{#GIX_REVISION}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKLM"; Subkey: "Software\MediumGray\gixsql"; ValueType: string; ValueName: "HomeDir"; ValueData: "{app}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gixsql"; ValueType: string; ValueName: "DataDir"; ValueData: "{localappdata}\Gix"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKLM"; Subkey: "Software\MediumGray\gixsql"; Flags: uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gixsql"; Flags: uninsdeletekey

[Dirs]
Name: "{app}\bin"
Name: "{app}\lib\copy"
Name: "{app}\lib\{#HOST_PLATFORM}\msvc"
Name: "{app}\lib\{#HOST_PLATFORM}\gcc"
Name: "{userdocs}\GixSQL"
Name: "{userdocs}\GixSQL\Documentation"
Name: "{userdocs}\GixSQL\Examples"

[Components]
Name: "gix_sql"; Description: "GixSQL"; Types: compact custom full; Flags: fixed

