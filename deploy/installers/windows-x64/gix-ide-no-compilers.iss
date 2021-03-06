#define QTDIR GetEnv('QTDIR')
#define WORKSPACE GetEnv('WORKSPACE')
#define BUILD_NUMBER GetEnv('BUILD_NUMBER')

#define CONFIG "Release"
#define HOST_PLATFORM "x64"
#define OTH_PLATFORM "x86"

[Setup]
AppName=Gix-Ide
AppVersion=1.0.{#BUILD_NUMBER}
AppCopyright=Marco Ridoni
DefaultDirName={pf}\Gix-IDE
OutputDir={#WORKSPACE}\deploy\installers\msvc-x64
OutputBaseFilename=Gix-IDE-1.0.0-installer-no-compilers
ArchitecturesInstallIn64BitMode=x64
DefaultGroupName=Gix-IDE

[Files]
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\cobpp.exe"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\gix-http.exe"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\gix-ide.exe"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\gixpp.exe"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\gix-common.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\ScintillaEdit4.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\libs\dbghelp.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\libs\libdwarf\msys-2.0.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs

; gixsql libs for IDE
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql-mysql.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql-odbc.dll "; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql-pgsql.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql.lib"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs

; gixsql runtime libs for debugged/run programs (x64)
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql-mysql.dll"; DestDir: "{app}\lib\{#HOST_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql-odbc.dll "; DestDir: "{app}\lib\{#HOST_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql-pgsql.dll"; DestDir: "{app}\lib\{#HOST_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql.dll"; DestDir: "{app}\lib\{#HOST_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#HOST_PLATFORM}\{#CONFIG}\libgixsql.lib"; DestDir: "{app}\lib\{#HOST_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs

; gixsql runtime libs for debugged/run programs (x86)
Source: "{#WORKSPACE}\{#OTH_PLATFORM}\{#CONFIG}\libgixsql-mysql.dll"; DestDir: "{app}\lib\{#OTH_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#OTH_PLATFORM}\{#CONFIG}\libgixsql-odbc.dll "; DestDir: "{app}\lib\{#OTH_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#OTH_PLATFORM}\{#CONFIG}\libgixsql-pgsql.dll"; DestDir: "{app}\lib\{#OTH_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#OTH_PLATFORM}\{#CONFIG}\libgixsql.dll"; DestDir: "{app}\lib\{#OTH_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\{#OTH_PLATFORM}\{#CONFIG}\libgixsql.lib"; DestDir: "{app}\lib\{#OTH_PLATFORM}"; Flags: ignoreversion createallsubdirs recursesubdirs

Source: "{#QTDIR}\bin\Qt5Core.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\bin\Qt5Gui.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\bin\Qt5Network.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\bin\Qt5Widgets.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\bin\Qt5Xml.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\bin\Qt5XmlPatterns.dll"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\plugins\bearer\*"; DestDir: "{app}\bin\bearer"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\plugins\iconengines\*"; DestDir: "{app}\bin\iconengines"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\plugins\imageformats\*"; DestDir: "{app}\bin\imageformats"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\plugins\platforms\*"; DestDir: "{app}\bin\platforms"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#QTDIR}\plugins\styles\*"; DestDir: "{app}\bin\styles"; Flags: ignoreversion createallsubdirs recursesubdirs
;Source: "{#QTDIR}\plugins\translations\*"; DestDir: "{app}\bin\translations"; Flags: ignoreversion createallsubdirs recursesubdirs

; Source: "{#WORKSPACE}\deploy\compilers\compilers\*"; DestDir: "{autoappdata}\Gix\compiler-pkgs"; Flags: ignoreversion createallsubdirs recursesubdirs
; Source: "{#WORKSPACE}\deploy\compilers\defs\*"; DestDir: "{autoappdata}\Gix\compiler-defs"; Flags: ignoreversion createallsubdirs recursesubdirs

Source: "{#WORKSPACE}\deploy\examples\*"; DestDir: "{userdocs}\Gix\Examples"; Flags: ignoreversion createallsubdirs recursesubdirs

[Registry]
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "version"; ValueData: "1.0.{#BUILD_NUMBER}"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "DataDir"; ValueData: "{autoappdata}\Gix"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "HomeDir"; ValueData: "{app}"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "ReleaseCompilerId"; ValueData: "gnucobol-3.1.2-std-vs-all"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "DebugCompilerId"; ValueData: "gnucobol-3.1.2-std-vs-all"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "editor_font_name"; ValueData: "Courier New"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "editor_font_size"; ValueData: "9"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "grid_font_name"; ValueData: "MS Shell Dlg 2"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "grid_font_size"; ValueData: "9"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "treeview_font_name"; ValueData: "MS Shell Dlg 2"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "treeview_font_size"; ValueData: "9"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "default_eol_mode"; ValueData: "0"; Flags: createvalueifdoesntexist deletevalue uninsdeletevalue uninsdeletekeyifempty

[Dirs]
Name: "{app}\bin"
Name: "{app}\lib\{#HOST_PLATFORM}"
Name: "{app}\lib\{#OTH_PLATFORM}"
; Name: "{autoappdata}\Gix\compiler-pkgs"
Name: "{userdocs}\Gix"
Name: "{userdocs}\Gix\Examples"

[Icons]
Name: "{group}\Gix-IDE"; Filename: "{app}\bin\gix-ide.exe"; WorkingDir: "{app}"
