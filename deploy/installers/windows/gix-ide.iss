#define QTDIR GetEnv('QTDIR')
#define HOST_PLATFORM GetEnv('HOST_PLATFORM')
#define WORKSPACE GetEnv('WORKSPACE')
#define GIX_REVISION GetEnv('GIX_REVISION')
#define VER_GIXIDEMAJ GetEnv('GIXIDEMAJ')
#define VER_GIXIDEMIN GetEnv('GIXIDEMIN')
#define VER_GIXIDEREL GetEnv('GIXIDEREL')
#define DIST_DIR GetEnv('DIST_DIR')
#define MSVC_BUILD_TOOLS GetEnv('MSVC_BUILD_TOOLS')
#define MSVC_RUNTIME_X86 GetEnv('MSVC_RUNTIME_X86')
#define MSVC_RUNTIME_X64 GetEnv('MSVC_RUNTIME_X64')

#define CONFIG "Release"

#define COMPILER_PKGS_INDEX "https://raw.githubusercontent.com/mridoni/gnucobol-binaries/main/def/index"
#define P7ZIP "https://www.7-zip.org/a/7zr.exe"

[Setup]
AppName=Gix-IDE
AppVersion={#VER_GIXIDEMAJ}.{#VER_GIXIDEMIN}.{#VER_GIXIDEREL}-{#GIX_REVISION}
AppCopyright=Marco Ridoni
DefaultDirName={autopf}\Gix-IDE
OutputDir={#WORKSPACE}\deploy\installers\msvc-{#HOST_PLATFORM}
OutputBaseFilename=Gix-IDE-{#VER_GIXIDEMAJ}.{#VER_GIXIDEMIN}.{#VER_GIXIDEREL}-{#GIX_REVISION}-installer
ArchitecturesInstallIn64BitMode=x64
DefaultGroupName=Gix-IDE
LicenseFile={#WORKSPACE}\GPL-3.0.txt
RestartIfNeededByRun=False
DisableWelcomePage=False
SetupLogging=yes
PrivilegesRequired=lowest

[Files]
; main binaries
Source: "{#DIST_DIR}\*"; DestDir: "{app}"; Flags: ignoreversion createallsubdirs recursesubdirs
#if "x64" == HOST_PLATFORM
Source: "{#WORKSPACE}\redist\msvcrt\x64\*"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs; Check: UseLocalMSVCRT
Source: "{#WORKSPACE}\redist\msvcrt\x64\*"; DestDir: "{app}\lib\x64\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs; Check: UseLocalMSVCRT
#else
Source: "{#WORKSPACE}\redist\msvcrt\x86\*"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs; Check: UseLocalMSVCRT
#endif
Source: "{#WORKSPACE}\redist\msvcrt\x86\*"; DestDir: "{app}\lib\x86\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs; Check: UseLocalMSVCRT

; COPY files
Source: "{#WORKSPACE}\gixsql\copy\SQLCA.cpy"; DestDir: "{app}\lib\copy"; Flags: ignoreversion createallsubdirs recursesubdirs

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

; examples and docs
Source: "{#WORKSPACE}\deploy\examples\*"; DestDir: "{userdocs}\Gix\Examples"; Flags: ignoreversion createallsubdirs recursesubdirs onlyifdoesntexist
Source: "{#WORKSPACE}\doc\*"; DestDir: "{userdocs}\Gix\Documentation"; Flags: ignoreversion createallsubdirs recursesubdirs

; dependencies for DB runtime libraries
#if "x64" == HOST_PLATFORM
Source: "{#WORKSPACE}\redist\deps\x64\msvc\*"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs 
Source: "{#WORKSPACE}\redist\deps\x64\gcc\*"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs 
Source: "{#WORKSPACE}\redist\deps\x64\msvc\*"; DestDir: "{app}\lib\x64\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs 
Source: "{#WORKSPACE}\redist\deps\x64\gcc\*"; DestDir: "{app}\lib\x64\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs 
#else
Source: "{#WORKSPACE}\redist\deps\x86\msvc\*"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs 
Source: "{#WORKSPACE}\redist\deps\x86\gcc\*"; DestDir: "{app}\bin"; Flags: ignoreversion createallsubdirs recursesubdirs 
#endif
Source: "{#WORKSPACE}\redist\deps\x86\msvc\*"; DestDir: "{app}\lib\x86\msvc"; Flags: ignoreversion createallsubdirs recursesubdirs 
Source: "{#WORKSPACE}\redist\deps\x86\gcc\*"; DestDir: "{app}\lib\x86\gcc"; Flags: ignoreversion createallsubdirs recursesubdirs 

[Run]
#if "x64" == HOST_PLATFORM
Filename: "{tmp}\vc_redist.x64.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}"; Flags: waituntilterminated runascurrentuser shellexec skipifdoesntexist; Description: "Visual C++ 2022 redistributable package (x64)"; Verb: "runas"; Check: UseDownloadedMSVCRT
#endif
Filename: "{tmp}\vc_redist.x86.exe"; Parameters: "/install /passive /norestart"; WorkingDir: "{tmp}"; Flags: waituntilterminated runascurrentuser shellexec skipifdoesntexist; Description: "Visual C++ 2022 redistributable package (x86)"; Verb: "runas"; Check: UseDownloadedMSVCRT
Filename: "{tmp}\vs_buildtools.exe"; Parameters: "--passive --norestart --add Microsoft.VisualStudio.Workload.VCTools --includeRecommended"; WorkingDir: "{tmp}"; Flags: waituntilterminated postinstall runascurrentuser shellexec; Description: "Install Visual C++ 2022 Build Tools"; StatusMsg: "Installing Visual C++ 2022 Build Tools"; Verb: "runas"; Check: IsMSVCCompilerSelected

[Registry]
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "version"; ValueData: "{#VER_GIXIDEMAJ}.{#VER_GIXIDEMIN}.{#VER_GIXIDEREL}-{#GIX_REVISION}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "HomeDir"; ValueData: "{app}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "DataDir"; ValueData: "{localappdata}\Gix"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "ReleaseCompilerId"; ValueData: "{code:DefaultCompiler}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey; Check: HasValidDefaultCompiler
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "DebugCompilerId"; ValueData: "{code:DefaultCompiler}"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey; Check: HasValidDefaultCompiler
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "editor_font_name"; ValueData: "Courier New"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "editor_font_size"; ValueData: "9"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "grid_font_name"; ValueData: "MS Shell Dlg 2"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "grid_font_size"; ValueData: "9"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: string; ValueName: "treeview_font_name"; ValueData: "MS Shell Dlg 2"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "treeview_font_size"; ValueData: "9"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; ValueType: dword; ValueName: "default_eol_mode"; ValueData: "0"; Flags: createvalueifdoesntexist deletevalue uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; Flags: uninsdeletekey
Root: "HKA"; Subkey: "Software\MediumGray\gix-ide"; Flags: uninsdeletekey
Root: "HKA"; Subkey: "Software\Classes\.gix"; ValueType: string; ValueData: "GixIdePrjColl"; Flags: createvalueifdoesntexist
Root: "HKA"; Subkey: "Software\Classes\GixIdePrjColl\DefaultIcon"; ValueType: expandsz; ValueData: "{app}\bin\gix-ide.exe,1"; Flags: createvalueifdoesntexist deletekey
Root: "HKA"; Subkey: "Software\Classes\.gixprj"; ValueType: string; ValueData: "GixIdePrj"; Flags: createvalueifdoesntexist deletekey
Root: "HKA"; Subkey: "Software\Classes\GixIdePrj\DefaultIcon"; ValueType: expandsz; ValueData: "{app}\bin\gix-ide.exe,2"; Flags: createvalueifdoesntexist deletekey

[Dirs]
Name: "{app}\bin"
Name: "{app}\lib\{#HOST_PLATFORM}\msvc"
Name: "{app}\lib\{#HOST_PLATFORM}\gcc"
Name: "{app}\copy"
Name: "{localappdata}\Gix\compiler-pkgs"
Name: "{localappdata}\Gix\compiler-defs"
Name: "{userdocs}\Gix"
Name: "{userdocs}\Gix\Documentation"
Name: "{userdocs}\Gix\Examples"

[Icons]
Name: "{group}\Gix-IDE"; Filename: "{app}\bin\gix-ide.exe"; WorkingDir: "{app}\bin"; IconFilename: "{app}\bin\gix-ide.exe"
Name: "{userdesktop}\Gix-IDE"; Filename: "{app}\bin\gix-ide.exe"; WorkingDir: "{app}\bin"; IconFilename: "{app}\bin\gix-ide.exe"

[Components]


[Code]

#include "7zip.iss.inc"
const
  MSVCRT_LOCAL = 0;
  MSVCRT_DOWNLOAD = 1;
  MSVCRT_NOINSTALL = 2;
  
var
  DownloadPage: TDownloadWizardPage;
  AvailableCompilers: TArrayOfString;
  SelectedCompilers: TArrayOfString;
  
  CompilerListInitialized : Boolean;
  ChooseCompilersPage: TWizardPage;
  DefaultCompilerPage: TInputOptionWizardPage;
  MsvcRtOptPage: TInputOptionWizardPage;
  DefaultCompilerId: String;
  ChooseCompCheckListBox: TNewCheckListBox;
  MsvcRtOptCheckListBox: TNewCheckListBox;
  cbGCC, cbMSVC : Integer;
  optMsvcRuntime : Integer;
  
function IsMSVCCompilerSelected : Boolean; forward;
function IsCompilerSelected(crow : String) : Boolean; forward;
function ReadCompilerIndex(IndexFile: String) : Boolean; forward;
function StrSplit(Text: String; Separator: String): TArrayOfString; forward;
function ParseCompilerEntry(crow : String; var release_tag : String; var id : String; var version : String; var host : String;
                                var target : String; var linker : String; var description : String) : Boolean; forward;

function GetLastError: Cardinal;
  external 'GetLastError@kernel32.dll stdcall';                
  
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
 
  DefaultCompilerPage := CreateInputOptionPage(ChooseCompilersPage.ID,
    'Default compiler', 'Choose a default compiler',
    'Please choose the default GnuCOBOL that will be used by Gix-IDE. This can be changed later in the Settings menu',
    True, True); 
  
  MsvcRtOptPage := CreateInputOptionPage(DefaultCompilerPage.ID, 
    'VC++ runtime', 'Choose how you want to install the Microsoft VC++ runtime', 
    'The Microsoft VC++ runtime is needed for Gix-IDE and for the runtime libraries of GixSQL. You can choose how you want to perform the install.',
    True, True);
  
  ChooseCompCheckListBox := TNewCheckListBox.Create(ChooseCompilersPage);
  ChooseCompCheckListBox.Width := ChooseCompilersPage.SurfaceWidth;
  ChooseCompCheckListBox.Height := ScaleY(150);
  ChooseCompCheckListBox.Flat := True;
  ChooseCompCheckListBox.Parent := ChooseCompilersPage.Surface; 

  MsvcRtOptPage.Add('Use the embedded version and install for Gix-IDE/GixSQL only');
  MsvcRtOptPage.Add('Download and install for all applications (requires administrative rights)');
  MsvcRtOptPage.Add('Do not install');
  MsvcRtOptPage.Values[0] := True;
 
end;

procedure CurPageChanged(CurPageID: Integer);
var
  i: Integer;
    cbtmp : Integer;
    crow : String;
  is_checked : Boolean;
    release_tag, id, version, host, target, linker, description : String;
begin
  if (CurPageID = wpFinished) and (IsMSVCCompilerSelected) and (WizardForm.RunList.Items.Count > 0) then
  begin
    WizardForm.RunList.ItemEnabled[0] := False;
  end;

  if CurPageID = DefaultCompilerPage.ID then
  begin
    DefaultCompilerPage.CheckListBox.Items.Clear;
    for i := 0 to GetArrayLength(SelectedCompilers) -1 do
    begin
      if not ParseCompilerEntry(SelectedCompilers[i], release_tag, id, version, host, target, linker, description) then continue;

      DefaultCompilerPage.Add(description);
    end;
  end;
  
  if CurPageID = ChooseCompilersPage.ID then
  begin
    
    ChooseCompCheckListBox.Items.Clear;
    cbMSVC := ChooseCompCheckListBox.AddCheckBox('GnuCOBOL - Linker type: MSVC', '', 0, False, True, False, True, nil);
    for i := 0 to GetArrayLength(AvailableCompilers) - 1 do 
    begin
      crow := AvailableCompilers[i];
      if not ParseCompilerEntry(crow, release_tag, id, version, host, target, linker, description) then continue;
      if linker <> 'msvc' then continue;
      
      is_checked := IsCompilerSelected(crow);
      cbtmp := ChooseCompCheckListBox.AddCheckBox(description, '', 1, is_checked, True, False, True, TObject(i));
      Log('Adding compiler: ' + description);
    end;  
        
    cbGCC := ChooseCompCheckListBox.AddCheckBox('GnuCOBOL - Linker type: GCC/MinGW', '', 0, False, True, False, True, nil);    
    for i := 0 to GetArrayLength(AvailableCompilers) - 1 do 
    begin
      crow := AvailableCompilers[i];
      if not ParseCompilerEntry(crow, release_tag, id, version, host, target, linker, description) then continue;
      if linker <> 'gcc' then continue;

      is_checked := IsCompilerSelected(crow);      
      cbtmp := ChooseCompCheckListBox.AddCheckBox(description, '', 1, is_checked, True, False, True, TObject(i));
      Log('Adding compiler: ' + description);
    end;  
  end;
    
end;

function DefaultCompiler(Param: string) : String;
begin
   Result := DefaultCompilerId;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
var
  release_tag, id, version, host, target, linker, description : String;
begin
  if (PageID = DefaultCompilerPage.ID) then
  begin
    if (GetArrayLength(SelectedCompilers) = 0) then
    begin
      Log('No compilers selected to be installed, so no default will have to be chosen');
      Result:= True;
      Exit;
    end;
    
    if (GetArrayLength(SelectedCompilers) = 1) then
    begin
      if not ParseCompilerEntry(SelectedCompilers[0], release_tag, id, version, host, target, linker, description) then 
      begin
        Result:= False;
        Exit;      
      end;
      
      Log('Only one compiler selected to be installed, so the default will be ' + id);
      DefaultCompilerId := id;
      Result:= True;
      Exit;
    end
  end;
  Result := False;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
var
  i, chk_count, cidx : Integer;
  has_msvc: Boolean;
  crow, curl: String;
  release_tag, id, version, host, target, linker, description : String;
  Error: Cardinal;
begin

  if CurPageID = DefaultCompilerPage.ID then
  begin
    i := DefaultCompilerPage.SelectedValueIndex;
    if (i < 0) then 
    begin
      MsgBox('Please select a compiler', mbError, MB_OK);
      Result := False;
      Exit;
    end;
    if not ParseCompilerEntry(SelectedCompilers[i], release_tag, id, version, host, target, linker, description) then 
    begin
      MsgBox('Invalid compiler', mbError, MB_OK);
      Result:= False;
      Exit;
    end;
    DefaultCompilerId := id;
    Log('Default compiler selected: ' + id);
    Result := True;
  end;

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
      for i := 0 to ChooseCompCheckListBox.Items.Count - 1 do
      begin
        if ChooseCompCheckListBox.ItemLevel[i] <> 1 then continue;
        
        cidx := Integer(ChooseCompCheckListBox.ItemObject[i]);
        if ChooseCompCheckListBox.Checked[i] then
        begin
          crow := AvailableCompilers[cidx];
          if not ParseCompilerEntry(crow, release_tag, id, version, host, target, linker, description) then continue;
          
          Log(id + ': selected')
          SetArrayLength(SelectedCompilers, GetArrayLength(SelectedCompilers) + 1);
          SelectedCompilers[chk_count] := crow;
          chk_count := chk_count + 1;
          if linker = 'msvc' then has_msvc := True;
        end
        else
          Log(AvailableCompilers[cidx] + ': NOT selected');
      end;
      
      if chk_count = 0 then
      begin
        if MsgBox('You have not selected any compiler to be installed. Are you sure?', mbConfirmation, MB_YESNO or MB_DEFBUTTON2) = IDYES then
          Result := True
        else
          Result := False;
      end
      else
      begin
        if Not IsAdmin and has_msvc then
        begin
            if MsgBox('You have selected an MSVC-based compiler to be installed. This means that at the end of this install you will be prompted to install or upgrade the Visual Studio 2022 C++Build Tools, that require administrator privileges, which might not be available to you. Are you sure?', mbConfirmation, MB_YESNO or MB_DEFBUTTON2) = IDYES then
              Result := True
            else
              Result := False;        
            end
        else
        begin
          Result := True;
        end;
      end;
   end; 
   
  if CurPageID = wpReady then begin
    DownloadPage.Clear;
    
    DownloadPage.Add('{#P7ZIP}', '7zr.exe', '');
    if optMsvcRuntime = MSVCRT_DOWNLOAD then
    begin
        DownloadPage.Add('{#MSVC_RUNTIME_X86}', 'vc_redist.x86.exe', '');
        if '{#HOST_PLATFORM}' = 'x64' then
        begin
        DownloadPage.Add('{#MSVC_RUNTIME_X64}', 'vc_redist.x64.exe', '');
        end;
    end;
    
    if IsMSVCCompilerSelected then
    begin
      DownloadPage.Add('{#MSVC_BUILD_TOOLS}', 'vs_BuildTools.exe', '');
    end;
    
    for i := 0 to GetArrayLength(SelectedCompilers) -1 do
    begin
      if not ParseCompilerEntry(SelectedCompilers[i], release_tag, id, version, host, target, linker, description) then continue;

      curl := 'https://github.com/mridoni/gnucobol-binaries/releases/download/' + release_tag + '/' + id;
      DownloadPage.Add(curl + '.7z', id + '.7z', '');
      DownloadPage.Add(curl + '.def', id + '.def', '');      
      DownloadPage.Add(curl + '.7z.sig', id + '.7z.sig', '');
      DownloadPage.Add(curl + '.def.sig', id + '.def.sig', '');

    end;
    
    DownloadPage.Show;
    try
      try
        DownloadPage.Download; 
        // TODO: check signatures        
        
        If Not DirExists(ExpandConstant('{localappdata}') + '\Gix') then
        begin
            If Not CreateDir(ExpandConstant('{localappdata}') + '\Gix') then 
            begin
              Error := GetLastError;
                Log(Format('Failed with code %d (0x%x) - %s', [ Error, Error, SysErrorMessage(Error) ]));
                RaiseException('Cannot create directory ' + ExpandConstant('{localappdata}') + '\Gix' );
            end;        
        end;
        
        If Not DirExists(ExpandConstant('{localappdata}') + '\Gix\compiler-pkgs') then
        begin
            If Not CreateDir(ExpandConstant('{localappdata}') + '\Gix\compiler-pkgs') then 
            begin
                Error := GetLastError;
                Log(Format('Failed with code %d (0x%x) - %s', [ Error, Error, SysErrorMessage(Error) ]));
                RaiseException('Cannot create directory ' + ExpandConstant('{localappdata}') + '\Gix\compiler-pkgs' );
            end;
        end;
            
        If Not DirExists(ExpandConstant('{localappdata}') + '\Gix\compiler-defs') then
        begin
            If Not CreateDir(ExpandConstant('{localappdata}') + '\Gix\compiler-defs') then 
            begin
                Error := GetLastError;
                Log(Format('Failed with code %d (0x%x) - %s', [ Error, Error, SysErrorMessage(Error) ]));
                RaiseException('Cannot create directory ' + ExpandConstant('{localappdata}') + '\Gix\compiler-defs' );
            end;
        end;
        
        for i := 0 to GetArrayLength(SelectedCompilers) -1 do
        begin
          if not ParseCompilerEntry(SelectedCompilers[i], release_tag, id, version, host, target, linker, description) then continue;

          Log('Uncompressing ' + ExpandConstant('{tmp}') + '\' + id + '.7z');
          DownloadPage.SetText('Uncompressing', ExpandConstant('{tmp}') + '\' + id + '.7z');
          Unzip(ExpandConstant('{tmp}') + '\' + id + '.7z', ExpandConstant('{localappdata}') + '\Gix\compiler-pkgs');
          
          Log ('Copying ' + ExpandConstant('{tmp}') + '\' + id + '.def to ' + ExpandConstant('{localappdata}') + '\Gix\compiler-defs\' + id + '.def');
          DownloadPage.SetText('Installing definition file', 'Copying ' + ExpandConstant('{tmp}') + '\' + id + '.def to ' + ExpandConstant('{localappdata}') + '\Gix\compiler-defs\' + id + '.def');
          if Not FileCopy(ExpandConstant('{tmp}') + '\' + id + '.def', ExpandConstant('{localappdata}') + '\Gix\compiler-defs\' + id + '.def', False) then 
          begin
            Error := GetLastError;
            Log(Format('Failed with code %d (0x%x) - %s', [ Error, Error, SysErrorMessage(Error) ]));
            RaiseException('Cannot copy compiler definition file');
          end;
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
  end;
  
  if CurPageID = MsvcRtOptPage.ID then
  begin
    optMsvcRuntime := MsvcRtOptPage.SelectedValueIndex;
    Log('MSVCRT install option: ' + IntToStr(optMsvcRuntime));
  end;
  
  
  if (Result) then
    Log('NextButtonClick (CurPageID: ' + IntToStr(CurPageID) + ', Result is True')
  else
    Log('NextButtonClick (CurPageID: ' + IntToStr(CurPageID) + ', Result is False')
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

function IsCompilerSelected(crow : String) : Boolean;
var
    i: Integer;
begin
  for i := 0 to GetArrayLength(SelectedCompilers) - 1 do
  begin   
    if SelectedCompilers[i] = crow then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function HasValidDefaultCompiler : Boolean;
var
    i: Integer;
begin

  if DefaultCompilerId <> '' then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
end;

function UseDownloadedMSVCRT : Boolean;
begin
  Result := optMsvcRuntime = MSVCRT_DOWNLOAD;
end;

function UseLocalMSVCRT : Boolean;
begin
  // Visual Studio Build Tools will install the VC++ runtime anyway
  Result := (optMsvcRuntime = MSVCRT_LOCAL) and not IsMSVCCompilerSelected;
end;