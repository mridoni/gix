#define QTDIR GetEnv('QTDIR')
#define WORKSPACE GetEnv('WORKSPACE')
#define GIX_REVISION GetEnv('GIX_REVISION')
#define VER_GIXIDEMAJ GetEnv('GIXIDEMAJ')
#define VER_GIXIDEMIN GetEnv('GIXIDEMIN')
#define VER_GIXIDEREL GetEnv('GIXIDEREL')

;#define REDIST_DIR GetEnv('REDIST_DIR')

#define DIST_DIR GetEnv('DIST_DIR')

#define INCLUDE_COMPILERS GetEnv('INCLUDE_COMPILERS')
#define DEFAULT_VS_COMPILER GetEnv('DEFAULT_VS_COMPILER')
#define DEFAULT_GCC_COMPILER GetEnv('DEFAULT_GCC_COMPILER')

#define MSVC_BUILD_TOOLS GetEnv('MSVC_BUILD_TOOLS')
#define MSVC_RUNTIME_X86 GetEnv('MSVC_RUNTIME_X86')
#define MSVC_RUNTIME_X64 GetEnv('MSVC_RUNTIME_X64')

#define CONFIG "Release"
#define HOST_PLATFORM GetEnv('HOST_PLATFORM')

#define COMPILER_PKGS_INDEX "https://raw.githubusercontent.com/mridoni/gnucobol-binaries/main/def/index"
#define P7ZIP "https://www.7-zip.org/a/7zr.exe"

[Setup]
AppName=Gix-IDE
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
Source: "{#WORKSPACE}\gixsql\copy\SQLCA.cpy"; DestDir: "{app}\lib\copy"; Flags: ignoreversion createallsubdirs recursesubdirs

#if 0
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
#endif

; compilers
Source: "{tmp}\compiler-pkgs\*"; DestDir: "{localappdata}\Gix\compiler-pkgs"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{tmp}\compiler-defs\*.def"; DestDir: "{localappdata}\Gix\compiler-defs"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist

; examples and docs
Source: "{#WORKSPACE}\deploy\examples\*"; DestDir: "{userdocs}\Gix\Examples"; Flags: ignoreversion createallsubdirs recursesubdirs onlyifdoesntexist
Source: "{#WORKSPACE}\doc\*"; DestDir: "{userdocs}\Gix\Documentation"; Flags: ignoreversion createallsubdirs recursesubdirs

; MS runtimes
;Source: "{#WORKSPACE}\deploy\redist\ms\common\*"; DestDir: "{tmp}\redist\ms\common"; Flags: ignoreversion createallsubdirs recursesubdirs
;Source: "{#WORKSPACE}\deploy\redist\ms\x86\*"; DestDir: "{tmp}\redist\ms\x86"; Flags: ignoreversion createallsubdirs recursesubdirs
;Source: "{#WORKSPACE}\deploy\redist\ms\x64\*"; DestDir: "{tmp}\redist\ms\x64"; Flags: ignoreversion createallsubdirs recursesubdirs

; dependencies for DB runtime libraries
#if "x64" == HOST_PLATFORM
Source: "{#WORKSPACE}\gixsql\deploy\redist\pgsql\x64\msvc\*"; DestDir: "{app}\lib\x64\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#WORKSPACE}\gixsql\deploy\redist\pgsql\x64\gcc\*"; DestDir: "{app}\lib\x64\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist

Source: "{#WORKSPACE}\gixsql\deploy\redist\mysql\x64\msvc\*"; DestDir: "{app}\lib\x64\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#WORKSPACE}\gixsql\deploy\redist\mysql\x64\gcc\*"; DestDir: "{app}\lib\x64\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
#endif

Source: "{#WORKSPACE}\gixsql\deploy\redist\pgsql\x86\msvc\*"; DestDir: "{app}\lib\x86\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#WORKSPACE}\gixsql\deploy\redist\pgsql\x86\gcc\*"; DestDir: "{app}\lib\x86\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist

Source: "{#WORKSPACE}\gixsql\deploy\redist\mysql\x86\msvc\*"; DestDir: "{app}\lib\x86\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist
Source: "{#WORKSPACE}\gixsql\deploy\redist\mysql\x86\gcc\*"; DestDir: "{app}\lib\x86\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs skipifsourcedoesntexist

[Run]
Filename: "{tmp}\vc_redist.x64.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}"; Flags: waituntilterminated skipifdoesntexist; Description: "Visual C++ 2022 redistributable package (x64)"
Filename: "{tmp}\vc_redist.x86.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}"; Flags: waituntilterminated skipifdoesntexist; Description: "Visual C++ 2022 redistributable package (x86)"
Filename: "{tmp}\vs_buildtools.exe"; Parameters: "--passive --norestart --add Microsoft.VisualStudio.Workload.VCTools --includeRecommended --add Microsoft.VisualStudio.Component.VC.CLI.Support --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 --add Microsoft.VisualStudio.Component.Windows10SDK.19041"; WorkingDir: "{tmp}"; Flags: skipifdoesntexist; Description: "Visual C++ 2022 build tools"; Check: IsComponentSelected('gnucobol_vs_compilers')

[Registry]
Root: "HKLM"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "version"; ValueData: "1.0.{#GIX_REVISION}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKLM"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "HomeDir"; ValueData: "{app}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "DataDir"; ValueData: "{localappdata}\Gix"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
#if "1" == INCLUDE_COMPILERS
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "ReleaseCompilerId"; ValueData: "{code:DefaultCompiler}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "DebugCompilerId"; ValueData: "{code:DefaultCompiler}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
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
Root: "HKLM"; Subkey: "SOFTWARE\Classes\.gix"; ValueData: "GixIdePrjColl"; Flags: createvalueifdoesntexist deletekey
Root: "HKLM"; Subkey: "SOFTWARE\Classes\GixIdePrjColl"; ValueData: "{app}\bin\gix-ide.exe,1"; Flags: createvalueifdoesntexist deletekey
Root: "HKLM"; Subkey: "SOFTWARE\Classes\.gixprj"; ValueData: "GixIdePrj"; Flags: createvalueifdoesntexist deletekey
Root: "HKLM"; Subkey: "SOFTWARE\Classes\GixIdePrj"; ValueData: "{app}\bin\gix-ide.exe,2"; Flags: createvalueifdoesntexist deletekey

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
Name: "{group}\Gix-IDE"; Filename: "{app}\bin\gix-ide.exe"; WorkingDir: "{app}\bin"; IconFilename: "{app}\bin\gix-ide.exe"
Name: "{userdesktop}\Gix-IDE"; Filename: "{app}\bin\gix-ide.exe"; WorkingDir: "{app}\bin"; IconFilename: "{app}\bin\gix-ide.exe"

[Components]


[Code]

#include "7zip.iss.inc"

var
  DownloadPage: TDownloadWizardPage;
  AvailableCompilers: TArrayOfString;
  SelectedCompilers: TArrayOfString;
  
  CompilerListInitialized : Boolean;
  ChooseCompilersPage: TWizardPage;
  CheckListBox: TNewCheckListBox;
  cbGCC, cbMSVC : Integer;
  
  CB1, CB2: Integer;
  OB1, OB2: Integer;   
  
function IsMSVCCompilerSelected : Boolean; forward;
function ReadCompilerIndex(IndexFile: String) : Boolean; forward;
function StrSplit(Text: String; Separator: String): TArrayOfString; forward;
function ParseCompilerEntry(crow : String; var release_tag : String; var id : String; var version : String; var host : String;
								var target : String; var linker : String; var description : String) : Boolean; forward;
  
function OnDownloadProgress(const Url, FileName: String; const Progress, ProgressMax: Int64): Boolean;
begin
  if Progress = ProgressMax then
    Log(Format('Successfully downloaded file to {tmp}: %s', [FileName]));
  Result := True;
end;

procedure InitializeWizard;
begin
  DownloadPage := CreateDownloadPage(SetupMessage(msgWizardPreparing), SetupMessage(msgPreparingDesc), @OnDownloadProgress);
  
  ChooseCompilersPage := CreateCustomPage(wpLicense, 'Choose the compilers you want to install (if any)', 'You can select one or more of the available compilers');

  CheckListBox := TNewCheckListBox.Create(ChooseCompilersPage);
  CheckListBox.Width := ChooseCompilersPage.SurfaceWidth;
  CheckListBox.Height := ScaleY(150);
  CheckListBox.Flat := True;
  CheckListBox.Parent := ChooseCompilersPage.Surface; 
end;

procedure CurPageChanged(CurPageID: Integer);
var
  i: Integer;
	cbtmp : Integer;
	crow : String;
	release_tag, id, version, host, target, linker, description : String;
begin
  if (CurPageID = wpFinished) and (IsComponentSelected('gnucobol_vs_compilers')) then
  begin
    WizardForm.RunList.ItemEnabled[0] := False;
  end;
  
  if CurPageID = ChooseCompilersPage.ID then
    begin
    
    CheckListBox.Items.Clear;
    cbMSVC := CheckListBox.AddCheckBox('GnuCOBOL - Linker type: MSVC', '', 0, False, True, False, True, nil);
    for i := 0 to GetArrayLength(AvailableCompilers) - 1 do 
    begin
      crow := AvailableCompilers[i];
      if not ParseCompilerEntry(crow, release_tag, id, version, host, target, linker, description) then continue;
      if linker <> 'msvc' then continue;
      
      cbtmp := CheckListBox.AddCheckBox(description, '', 1, False, True, False, True, TObject(i));
      Log('Adding compiler: ' + description);
    end;  
        
    cbGCC := CheckListBox.AddCheckBox('GnuCOBOL - Linker type: GCC/MinGW', '', 0, False, True, False, True, nil);    
    for i := 0 to GetArrayLength(AvailableCompilers) - 1 do 
    begin
      crow := AvailableCompilers[i];
      if not ParseCompilerEntry(crow, release_tag, id, version, host, target, linker, description) then continue;
      if linker <> 'gcc' then continue;
      
      cbtmp := CheckListBox.AddCheckBox(description, '', 1, False, True, False, True, TObject(i));
      Log('Adding compiler: ' + description);
    end;  
  end;
end;

function DefaultCompiler(Param: string) : String;
begin
  if IsComponentSelected('gnucobol_vs_compilers') then
    Result := '{#DEFAULT_VS_COMPILER}'
  else
    Result := '{#DEFAULT_GCC_COMPILER}';
end;

function NextButtonClick(CurPageID: Integer): Boolean;
var
  i, chk_count, cidx : Integer;
  crow, curl: String;
  release_tag, id, version, host, target, linker, description : String;
begin
  if CurPageID = wpLicense then begin

    DownloadPage.Clear;
    DownloadPage.Add('{#COMPILER_PKGS_INDEX}', 'compiler-pkgs.idx', '');
    DownloadPage.Show;
	
    try
      try
        DownloadPage.Download; // This downloads the files to {tmp}
        ReadCompilerIndex(ExpandConstant('{tmp}') + '\compiler-pkgs.idx');
        Result := True;
      except
        if DownloadPage.AbortedByUser then
          Log('Aborted by user.')
        else
          SuppressibleMsgBox(AddPeriod(GetExceptionMessage), mbCriticalError, MB_OK, IDOK);
        Result := False;
      end;
    finally
      DownloadPage.Hide;
    end;
   end else	 
	 Result := True;

   if CurPageID = ChooseCompilersPage.ID then
   begin
      chk_count := 0;
      SetArrayLength(SelectedCompilers, 0);
      for i := 0 to CheckListBox.Items.Count - 1 do
      begin
        if CheckListBox.ItemLevel[i] <> 1 then continue;
        
        cidx := Integer(CheckListBox.ItemObject[i]);
        if CheckListBox.Checked[i] then
        begin
          crow := AvailableCompilers[cidx];
          if not ParseCompilerEntry(crow, release_tag, id, version, host, target, linker, description) then continue;
          
          Log(id + ': selected')
          SetArrayLength(SelectedCompilers, GetArrayLength(SelectedCompilers) + 1);
          SelectedCompilers[chk_count] := crow;
          chk_count := chk_count + 1;
        end
        else
          Log(AvailableCompilers[cidx] + ': NOT selected');
      end;
      
      if chk_count = 0 then
      begin
        if MsgBox('You have not selected any compiler to be installed. Are you sure?', mbConfirmation, MB_YESNO or MB_DEFBUTTON2) = IDYES then
        begin
          Result := True;
        end
        else
          Result := False;
      end
      else
        Result := True;

   end; 
   
  if CurPageID = wpReady then begin
    DownloadPage.Clear;
    
    DownloadPage.Add('{#P7ZIP}', '7zr.exe', '');
    DownloadPage.Add('{#MSVC_RUNTIME_X86}', 'vc_redist.x86.exe', '');
    if '{#HOST_PLATFORM}' = 'x64' then
    begin
      DownloadPage.Add('{#MSVC_RUNTIME_X64}', 'vc_redist.x64.exe', '');
    end;
    
    if IsMSVCCompilerSelected then
    begin
      DownloadPage.Add('{#MSVC_BUILD_TOOLS}', 'vs_BuildTools.exe', '');
    end;
    
    for i := 0 to GetArrayLength(SelectedCompilers) -1 do
    begin
      if not ParseCompilerEntry(SelectedCompilers[i], release_tag, id, version, host, target, linker, description) then continue;

      curl := 'https://github.com/mridoni/gnucobol-binaries/releases/download/' + release_tag + '/' + id + '.7z';
      DownloadPage.Add(curl, id + '.7z', '');
      DownloadPage.Add(curl, id + '.def', '');      
      DownloadPage.Add(curl, id + '.7z.sig', '');
      DownloadPage.Add(curl, id + '.def.sig', '');

    end;
    
    DownloadPage.Show;
    try
      try
        DownloadPage.Download; 
        // TODO: check signatures        
        
        CreateDir(ExpandConstant('{tmp}') + '\compiler-pkgs');
        CreateDir(ExpandConstant('{tmp}') + '\compiler-defs');
        
        for i := 0 to GetArrayLength(SelectedCompilers) -1 do
        begin
          if not ParseCompilerEntry(SelectedCompilers[i], release_tag, id, version, host, target, linker, description) then continue;

          Unzip(ExpandConstant('{tmp}') + '\' + id + '.7z', ExpandConstant('{tmp}') + '\compiler-pkgs');
          FileCopy(ExpandConstant('{tmp}') + '\' + id + '.def', ExpandConstant('{tmp}') + '\compiler-defs', False);

        end;
        
        Result := True;
      except
        if DownloadPage.AbortedByUser then
          Log('Aborted by user.')
        else
          SuppressibleMsgBox(AddPeriod(GetExceptionMessage), mbCriticalError, MB_OK, IDOK);
        Result := False;
      end;
    finally
      DownloadPage.Hide;
    end;
  end else
    Result := True;
	
end;

function ReadCompilerIndex(IndexFile: String) : Boolean;
var
	clist : TArrayOfString;
	crow : String;
	centry : TArrayOfString;
	res : Boolean;
	i, cidx : Integer;
	release_tag, id, version, host, target, linker, description : String;
begin
  Log('Index File: ' + IndexFile);
  CompilerListInitialized := False;
	res := LoadStringsFromFile(IndexFile, clist);
	if not Res Then 
	begin
		Result := False;
		Exit;
	end;
	
  for i := 0 to GetArrayLength(clist) - 1 do
  begin
    crow := clist[I];
	if not ParseCompilerEntry(crow, release_tag, id, version, host, target, linker, description) then continue;
	
	if (host <> '{#HOST_PLATFORM}') then continue;
	
	cidx := GetArrayLength(AvailableCompilers);
	SetArrayLength(AvailableCompilers, cidx + 1);
	AvailableCompilers[cidx] := crow;
  Log ('Adding compiler: ' + id);
	cidx := cidx + 1;
  end;
	
  CompilerListInitialized := True;
	Result := True;
end;

function StrSplit(Text: String; Separator: String): TArrayOfString;
var
  i, p: Integer;
  Dest: TArrayOfString; 
begin
  i := 0;
  repeat
    SetArrayLength(Dest, i+1);
    p := Pos(Separator,Text);
    if p > 0 then begin
      Dest[i] := Copy(Text, 1, p-1);
      Text := Copy(Text, p + Length(Separator), Length(Text));
      i := i + 1;
    end else begin
      Dest[i] := Text;
      Text := '';
    end;
  until Length(Text)=0;
  Result := Dest
end;

function ParseCompilerEntry(crow : String; var release_tag : String; var id : String; var version : String; var host : String;
								var target : String; var linker : String; var description : String) : Boolean;
var
	centry : TArrayOfString;
begin
	centry := StrSplit(crow, ';');
	if GetArrayLength(centry) <> 7 then 
	begin
		Result := False;
		Exit;
	end;
	
	release_tag := centry[0];
	id := centry[1];
	version := centry[2];
	host := centry[3];
	target := centry[4];
	linker := centry[5];
	description := centry[6];
	
	Result := True;
end;								

function IsMSVCCompilerSelected : Boolean;
var
	crow : String;
	i: Integer;
	release_tag, id, version, host, target, linker, description : String;
begin
  for i := 0 to GetArrayLength(SelectedCompilers) - 1 do
  begin
    if not ParseCompilerEntry(SelectedCompilers[i], release_tag, id, version, host, target, linker, description) then continue;
    
    if linker = 'msvc' then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;