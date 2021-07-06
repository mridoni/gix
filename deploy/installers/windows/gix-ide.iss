#define QTDIR GetEnv('QTDIR')
#define WORKSPACE GetEnv('WORKSPACE')
#define GIX_REVISION GetEnv('GIX_REVISION')
#define VER_GIXIDEMAJ GetEnv('GIXIDEMAJ')
#define VER_GIXIDEMIN GetEnv('GIXIDEMIN')
#define VER_GIXIDEREL GetEnv('GIXIDEREL')

#define REDIST_DIR GetEnv('REDIST_DIR')

#define DIST_DIR GetEnv('DIST_DIR')

#define DIST_DIR GetEnv('DIST_DIR')
#define INCLUDE_COMPILERS GetEnv('INCLUDE_COMPILERS')
#define DEFAULT_VS_COMPILER GetEnv('DEFAULT_VS_COMPILER')
#define DEFAULT_GCC_COMPILER GetEnv('DEFAULT_GCC_COMPILER')

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
AppName=Gix-Ide
AppVersion={#VER_GIXIDEMAJ}.{#VER_GIXIDEMIN}.{#VER_GIXIDEREL}-{#GIX_REVISION}
AppCopyright=Marco Ridoni
DefaultDirName={pf}\Gix-IDE
OutputDir={#WORKSPACE}\deploy\installers\msvc-{#HOST_PLATFORM}
OutputBaseFilename=Gix-IDE-{#VER_GIXIDEMAJ}.{#VER_GIXIDEMIN}.{#VER_GIXIDEREL}-{#GIX_REVISION}-installer
ArchitecturesInstallIn64BitMode=x64
DefaultGroupName=Gix-IDE
LicenseFile={#WORKSPACE}\GPL-3.0.txt
RestartIfNeededByRun=False
DisableWelcomePage=False

[Files]
; main binaries
Source: "{#DIST_DIR}\*"; DestDir: "{app}"; Flags: ignoreversion createallsubdirs recursesubdirs

; COPY files
Source: "{#WORKSPACE}\gixsql\copy\SQLCA.cpy"; DestDir: "{app}\copy"; Flags: ignoreversion createallsubdirs recursesubdirs

; Qt
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

; compilers
#if "1" == INCLUDE_COMPILERS
Source: "{#WORKSPACE}\deploy\compilers\gcc\pkgs\*"; DestDir: "{localappdata}\Gix\compiler-pkgs"; Flags: ignoreversion createallsubdirs recursesubdirs; Components: gnucobol_mingw_compilers
Source: "{#WORKSPACE}\deploy\compilers\gcc\defs\*.def"; DestDir: "{localappdata}\Gix\compiler-defs"; Flags: ignoreversion createallsubdirs recursesubdirs; Components: gnucobol_mingw_compilers

Source: "{#WORKSPACE}\deploy\compilers\msvc\pkgs\*"; DestDir: "{localappdata}\Gix\compiler-pkgs"; Flags: ignoreversion createallsubdirs recursesubdirs; Components: gnucobol_vs_compilers
Source: "{#WORKSPACE}\deploy\compilers\msvc\defs\*.def"; DestDir: "{localappdata}\Gix\compiler-defs"; Flags: ignoreversion createallsubdirs recursesubdirs; Components: gnucobol_vs_compilers
#endif

; examples and docs
Source: "{#WORKSPACE}\deploy\examples\*"; DestDir: "{userdocs}\Gix\Examples"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#WORKSPACE}\doc\*"; DestDir: "{userdocs}\Gix\Documentation"; Flags: ignoreversion createallsubdirs recursesubdirs

; MS runtimes
Source: "{#REDIST_DIR}\ms\common\*"; DestDir: "{tmp}\redist\ms\common"; Flags: ignoreversion createallsubdirs recursesubdirs
Source: "{#REDIST_DIR}\ms\x86\*"; DestDir: "{tmp}\redist\ms\x86"; Flags: ignoreversion createallsubdirs recursesubdirs
#if "x64" == HOST_PLATFORM
Source: "{#REDIST_DIR}\ms\x64\*"; DestDir: "{tmp}\redist\ms\x64"; Flags: ignoreversion createallsubdirs recursesubdirs
#endif

; dependencies for DB runtime libraries
#if "x64" == HOST_PLATFORM
Source: "{#REDIST_DIR}\pgsql\x64\msvc\*"; DestDir: "{app}\lib\x64\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#REDIST_DIR}\pgsql\x64\gcc\*"; DestDir: "{app}\lib\x64\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist

Source: "{#REDIST_DIR}\mysql\x64\msvc\*"; DestDir: "{app}\lib\x64\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#REDIST_DIR}\mysql\x64\gcc\*"; DestDir: "{app}\lib\x64\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
#endif

Source: "{#REDIST_DIR}\pgsql\x86\msvc\*"; DestDir: "{app}\lib\x86\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#REDIST_DIR}\pgsql\x86\gcc\*"; DestDir: "{app}\lib\x86\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist

Source: "{#REDIST_DIR}\mysql\x86\msvc\*"; DestDir: "{app}\lib\x86\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#REDIST_DIR}\mysql\x86\gcc\*"; DestDir: "{app}\lib\x86\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist

[Run]
#if "x64" == HOST_PLATFORM
Filename: "{tmp}\redist\ms\x64\VC_redist.x64.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}\redist\ms\x64\"; Flags: waituntilterminated; Description: "Visual C++ 2019 redistributable package (x64)"
#endif
Filename: "{tmp}\redist\ms\x86\VC_redist.x86.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}\redist\ms\x86\"; Flags: waituntilterminated; Description: "Visual C++ 2019 redistributable package (x86)"
Filename: "{tmp}\redist\ms\x86\vcredist_x86_vs2013.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}\redist\ms\x86\"; Flags: waituntilterminated; Description: "Visual C++ 2013 redistributable package (x86)"
#if "1" == INCLUDE_COMPILERS
Filename: "{tmp}\redist\ms\common\vs_buildtools_2019.exe"; Parameters: "--passive --norestart --add Microsoft.VisualStudio.Workload.VCTools --includeRecommended --add Microsoft.VisualStudio.Component.VC.CLI.Support --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 --add Microsoft.VisualStudio.Component.Windows10SDK.19041"; WorkingDir: "{tmp}\redist\ms\common\"; Flags: postinstall; Description: "Visual C++ 2019 build tools"; Check: IsComponentSelected('gnucobol_vs_compilers')
#endif

[Registry]
Root: "HKLM"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "version"; ValueData: "1.0.{#GIX_REVISION}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKLM"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "HomeDir"; ValueData: "{app}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "DataDir"; ValueData: "{autoappdata}\Gix"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
#if "1" == INCLUDE_COMPILERS
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "ReleaseCompilerId"; ValueData: {code:DefaultCompiler}; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "DebugCompilerId"; ValueData: {code:DefaultCompiler}; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
#endif
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "editor_font_name"; ValueData: "Courier New"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "editor_font_size"; ValueData: "9"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "grid_font_name"; ValueData: "MS Shell Dlg 2"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "grid_font_size"; ValueData: "9"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "treeview_font_name"; ValueData: "MS Shell Dlg 2"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "treeview_font_size"; ValueData: "9"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "default_eol_mode"; ValueData: "0"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKLM"; Subkey: "Software\MediumGray\gix-ide"; Flags: uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; Flags: uninsdeletekey

[Dirs]
Name: "{app}\bin"
Name: "{app}\lib\{#HOST_PLATFORM}\msvc"
Name: "{app}\lib\{#HOST_PLATFORM}\gcc"
Name: "{app}\copy"
Name: "{localappdata}\Gix\compiler-pkgs"
Name: "{userdocs}\Gix"
Name: "{userdocs}\Gix\Documentation"
Name: "{userdocs}\Gix\Examples"

[Icons]
Name: "{group}\Gix-IDE"; Filename: "{app}\bin\gix-ide.exe"; WorkingDir: "{app}"
Name: "{userdesktop}\Gix-IDE"; Filename: "{app}\bin\gix-ide.exe"; WorkingDir: "{app}"

[Components]
Name: "gix_ide"; Description: "Gix-IDE"; Types: compact custom full; Flags: fixed
#if "1" == INCLUDE_COMPILERS
Name: "gnucobol_mingw_compilers"; Description: "MinGW-based GnuCOBOL compilers"; Types: compact custom full
Name: "gnucobol_vs_compilers"; Description: "Visual Studio-based GnuCOBOL compilers"; Types: compact custom full
#endif

[Code]
procedure CurPageChanged(CurPageID: Integer);
begin
  if (CurPageID = wpFinished) and (IsComponentSelected('gnucobol_vs_compilers')) then
    WizardForm.RunList.ItemEnabled[0] := False;
end;

function DefaultCompiler(Param: string) : String;
begin
  if IsComponentSelected('gnucobol_vs_compilers') then
    Result := '{#DEFAULT_VS_COMPILER}'
  else
    Result := '{#DEFAULT_GCC_COMPILER}';
end;