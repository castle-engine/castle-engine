{
  Copyright 2022-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for Delphi (both VCL and FMX), only design-time.

  - Sets OnGetDesignTimeProjectPath so that other code
    (design-time or not only design-time) can use it, if assigned.

  - Sets up Delphi IDE menu items to configure project for Castle Game Engine. }
unit CastleInternalDelphiDesignUtils;

interface

//procedure Register;

implementation

{ Documentation:

  Embarcadero:
  https://docwiki.embarcadero.com/RADStudio/Athens/en/Extending_the_IDE_Using_the_Tools_API
  https://docwiki.embarcadero.com/RADStudio/Athens/en/Adding_an_Item_to_the_Main_Menu_of_the_IDE
  https://docwiki.embarcadero.com/RADStudio/Athens/en/Creating_or_Extending_a_Package_to_Use_the_Tools_API
  https://docwiki.embarcadero.com/RADStudio/Athens/en/Obtaining_Tools_API_Services

  Others:
  https://www.gexperts.org/open-tools-api-faq/#menuitem
    Note: Do not actually search by menu item Caption, because IDE may be localized,
    instead hardcode the Name.
    Embarcadero docs say so.
  https://www.davidghoyle.co.uk/WordPress/?p=777

  See sources, e.g.
  C:\Program Files (x86)\Embarcadero\Studio\22.0\source\ToolsAPI\ToolsAPI.pas

  See tests in
  https://github.com/michaliskambi/delphi-test-package-design-features
}

uses SysUtils, Classes,
  { In practice, this unit is Windows-only, just like Delphi IDE.
    But we make some effort to use only cross-platform API,
    and put platform-specific API under $ifdef, in the hope to use
    in a cross-platform IDE some day. }
  {$ifdef MSWINDOWS} Windows, ShellApi, {$endif}
  ToolsAPI, // design-time only unit
  Vcl.Menus, Vcl.Dialogs, Vcl.FileCtrl, Vcl.ActnList,
  CastleInternalDelphiUtils, CastleConfig, CastleApplicationProperties,
  CastleUtils, CastleInternalTools, CastleStringUtils, CastleOpenDocument;

{ Utilities ------------------------------------------------------------------ }

{ Current Delphi project path, exception if no project is open now. }
function GetProjectPath: String;
begin
  if GetActiveProject = nil then
    raise Exception.Create('No active Delphi project');

  Result := ExtractFilePath(GetActiveProject.FileName);
end;

{ Run a process.
  Does not wait for process to finish,
  does not capture any process output (stdout, stderr),
  does not pass anything to process input.

  @param(ExeName Executable filename.
    Make it always an absolute path, though it actually supports now
    also filenames relative to current working dir.)

  @param(Parameters Parameters to pass.

    Can specify multiple parameters,
    but for now suffering from the same weirdness as underlying Windows API
    -- instead of taking a list of String,
    all parameters are specified as one long String with parameters
    separated by spaces and optionally surrounded by double quotes.)

  @param(WorkingDirectory Working directory for the process.)
}
procedure ExecuteProcess(const ExeName, Parameters, WorkingDirectory: String);
{$ifdef MSWINDOWS}
var
  ShExecInfo: TShellExecuteInfo;
  Service: IOTAServices;
begin
  if not FileExists(ExeName) then
    raise Exception.CreateFmt('File to execute not found: "%s"', [ExeName]);

  Service := BorlandIDEServices as IOTAServices;

  FillChar(ShExecInfo, SizeOf(ShExecInfo), 0);
  ShExecInfo.cbSize := SizeOf(ShExecInfo);
  ShExecInfo.Wnd := Service.GetParentHandle;
  ShExecInfo.lpVerb := 'open';
  ShExecInfo.lpFile := PWideChar(ExeName);
  ShExecInfo.lpParameters := PWideChar(Parameters);
  ShExecInfo.lpDirectory := PWideChar(WorkingDirectory);
  ShExecInfo.nShow := SW_SHOWNORMAL;
  if not ShellExecuteEx(@ShExecInfo) then
    RaiseLastOSError;
{$else}
begin
  raise Exception.Create('Executing process not implemented on this platform');
{$endif}
end;

{ TCastleDelphiIdeIntegration ----------------------------------------------- }

type
  TCastleDelphiIdeIntegration = class(TComponent)
  strict private
    FEnginePath: String;
    FEnginePathInitialized: Boolean;
    MenuEngineRoot,
      MenuSetEnginePath, MenuOpenEditor, MenuAddPaths, MenuRemovePaths,
      MenuWebsite, MenuApiReference, MenuDonate: TMenuItem;
    ActionSetEnginePath, ActionOpenEditor, ActionAddPaths, ActionRemovePaths,
      ActionWebsite, ActionApiReference, ActionDonate: TAction;

    procedure EnsureConfigInitialized;

    function GetEnginePath: String;
    procedure SetEnginePath(const Value: String);
    property EnginePath: String read GetEnginePath write SetEnginePath;

    { Does given project source dir (absolute or relative to GetProjectPath)
      is equal to EngineSrcDir (relative to CGE src/).
      Tolerates various ways how the paths can be written,
      to avoid adding duplicates (in ClickAddPaths)
      and to remove correctly (in ClickRemovePaths). }
    function ProjectDirIsEngineDir(
      const ProjectSrcDir, EngineSrcDir, EngineRelativeToProject: String): Boolean;

    { Action event handlers }
    procedure ClickSetEnginePath(Sender: TObject);
    procedure ClickOpenEditor(Sender: TObject);
    procedure ClickAddPaths(Sender: TObject);
    procedure ClickRemovePaths(Sender: TObject);
    procedure ClickWebsite(Sender: TObject);
    procedure ClickApiReference(Sender: TObject);
    procedure ClickDonate(Sender: TObject);
    procedure UpdateEnabledIfProjectAndCgeInitialized(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TCastleDelphiIdeIntegration.Create(AOwner: TComponent);
var
  Services: INTAServices;
begin
  inherited;
  Services := BorlandIDEServices as INTAServices;

  ActionSetEnginePath := TAction.Create(Self);
  ActionSetEnginePath.Caption := 'Set Engine Path...';
  ActionSetEnginePath.OnExecute := ClickSetEnginePath;
  MenuSetEnginePath := TMenuItem.Create(Self);
  MenuSetEnginePath.Action := ActionSetEnginePath;

  ActionOpenEditor := TAction.Create(Self);
  ActionOpenEditor.Caption := 'Open Editor';
  ActionOpenEditor.OnExecute := ClickOpenEditor;
  ActionOpenEditor.OnUpdate := UpdateEnabledIfProjectAndCgeInitialized;
  MenuOpenEditor := TMenuItem.Create(Self);
  MenuOpenEditor.Action := ActionOpenEditor;

  ActionAddPaths := TAction.Create(Self);
  ActionAddPaths.Caption := 'Configure Currrent Project to Use Engine';
  ActionAddPaths.OnExecute := ClickAddPaths;
  ActionAddPaths.OnUpdate := UpdateEnabledIfProjectAndCgeInitialized;
  MenuAddPaths := TMenuItem.Create(Self);
  MenuAddPaths.Action := ActionAddPaths;

  ActionRemovePaths := TAction.Create(Self);
  ActionRemovePaths.Caption := 'Remove Engine Configuration from the Currrent Project';
  ActionRemovePaths.OnExecute := ClickRemovePaths;
  ActionRemovePaths.OnUpdate := UpdateEnabledIfProjectAndCgeInitialized;
  MenuRemovePaths := TMenuItem.Create(Self);
  MenuRemovePaths.Action := ActionRemovePaths;

  MenuSeparator := TMenuItem.Create(Self);
  MenuSeparator.Caption := '-';

  ActionWebsite := TAction.Create(Self);
  ActionWebsite.Caption := 'Open Website';
  ActionWebsite.OnExecute := ClickWebsite;
  MenuWebsite := TMenuItem.Create(Self);
  MenuWebsite.Action := ActionWebsite;

  ActionApiReference := TAction.Create(Self);
  ActionApiReference.Caption := 'Open API Reference';
  ActionApiReference.OnExecute := ClickApiReference;
  MenuApiReference := TMenuItem.Create(Self);
  MenuApiReference.Action := ActionApiReference;

  ActionDonate := TAction.Create(Self);
  ActionDonate.Caption := 'Support us on Patreon';
  ActionDonate.OnExecute := ClickDonate;
  MenuDonate := TMenuItem.Create(Self);
  MenuDonate.Action := ActionDonate;

  MenuEngineRoot := TMenuItem.Create(Self);
  MenuEngineRoot.Caption := 'Castle Game Engine';
  MenuEngineRoot.Name := 'CastleGameEngineMenu';

  { Use hardcoded menu item name, like
    - ToolsToolsItem ("Configure Tools")
    - ToolsMenu ("Tools" from main menu)
    - ViewTranslationManagerMenu ("Translation Manager")
    This is the proper approach, acoording to
    https://docwiki.embarcadero.com/RADStudio/Athens/en/Adding_an_Item_to_the_Main_Menu_of_the_IDE .

    Note: Do not add menu items after "Configure Tools" (name 'ToolsToolsItem').
    These will be removed when Delphi IDE is started, and replaced with
    menu items that correspond to stuff confiured in "Configure Tools".
    E.g. this will not work reliably, stuff will disappear after Delphi restart:

      Services.AddActionMenu('ToolsToolsItem', nil, MenuEngineRoot, true, true);

    So a custom "Tools" submenu should be added before "Configure Tools". }
  Services.AddActionMenu('ViewTranslationManagerMenu', nil, MenuEngineRoot, true, false);

  { We can add submenu items using MenuEngineRoot.Add (see above) or by adding
    using Services.AddActionMenu.
    There doesn't seem to be any difference. }
  // MenuEngineRoot.Add(MenuSetEnginePath);
  // MenuEngineRoot.Add(MenuOpenEditor);
  // MenuEngineRoot.Add(MenuAddPaths);
  // MenuEngineRoot.Add(MenuRemovePaths);
  Services.AddActionMenu('CastleGameEngineMenu', ActionSetEnginePath, MenuSetEnginePath, true, true);
  Services.AddActionMenu('CastleGameEngineMenu', ActionOpenEditor, MenuOpenEditor, true, true);
  Services.AddActionMenu('CastleGameEngineMenu', ActionAddPaths, MenuAddPaths, true, true);
  Services.AddActionMenu('CastleGameEngineMenu', ActionRemovePaths, MenuRemovePaths, true, true);
  Services.AddActionMenu('CastleGameEngineMenu', nil, MenuSeparator, true, true);
end;

destructor TCastleDelphiIdeIntegration.Destroy;
begin
  { Note: It seems that MenuEngineRoot is not removed from ToolsMenu automatically when
    TTestDelphiIdeIntegration instance is destroyed,
    even when MenuEngineRoot is owned by TTestDelphiIdeIntegration (was created as
    "MenuEngineRoot := TMenuItem.Create(Self)").
    Freing it explicitly here works.

    Note that we shouldn't free subitems like MenuSetEnginePath,
    they will be freed automatically
    (trying to free them would result in invalid pointer exceptions).

    Why? It seems Delphi IDE does something funny and changes ownership
    of items added using Services.AddActionMenu -- so they are owned by their parents
    effectively?

    We create them all now with Owner=nil, to avoid confusion.
  }

  FreeAndNil(MenuEngineRoot);
  // FreeAndNil(MenuSetEnginePath);
  // FreeAndNil(MenuOpenEditor);
  // FreeAndNil(MenuAddPaths);
  // FreeAndNil(MenuRemovePaths);

  inherited;
end;

procedure TCastleDelphiIdeIntegration.UpdateEnabledIfProjectAndCgeInitialized(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (GetActiveProject <> nil) and
    (EnginePath <> '');
end;

procedure TCastleDelphiIdeIntegration.EnsureConfigInitialized;
begin
  { Read our config file on demand.

      Reason: for robustness: in initialization / finalization of this unit,
      we try to do as little as possible, because this happens when Delphi IDE
      starts / stops, and any errors are hard to debug there
      (hard to even pinpoint "this is CGE code fault").
      We prefer to do all possible work in response to use clicking
      on CGE menu items.

      Note: Since the introduction of UpdateEnabledIfProjectAndCgeInitialized,
      this is somewhat pointless: UpdateEnabledIfProjectAndCgeInitialized will
      read EnginePath as soon as any project is opened.
      And we decided to go with it: the UX improvement thanks
      to UpdateEnabledIfProjectAndCgeInitialized (clearly communicates
      that engine path must be set) is worth it.

    We store our config in CGE UserConfig.
    We do not use registry and Delphi GetBaseRegistryKey.

      Reason: It is actually nice that CGE config is cross-Delphi-version.
      And this way we can read it easily from other CGE code,
      if it will ever be needed (but we don't plan to need it -- CGE
      path set in Delphi should be independent from everything else,
      to be simple).
  }
  if not FEnginePathInitialized then
  begin
    FEnginePathInitialized := true;
    ApplicationProperties.ApplicationName := 'delphi_ide_with_castle_game_engine';
    UserConfig.Load;
    FEnginePath := UserConfig.GetValue('engine_path', '');
  end;
  Assert(FEnginePathInitialized);
end;

function TCastleDelphiIdeIntegration.GetEnginePath: String;
begin
  EnsureConfigInitialized;
  Result := FEnginePath;
end;

procedure TCastleDelphiIdeIntegration.SetEnginePath(const Value: String);
begin
  // make sure we initialized UserConfig
  EnsureConfigInitialized;
  if FEnginePath <> Value then
  begin
    FEnginePath := Value;
    UserConfig.SetDeleteValue('engine_path', FEnginePath, '');
    UserConfig.Save;
  end;
end;

procedure TCastleDelphiIdeIntegration.ClickSetEnginePath(Sender: TObject);
var
  S: String;
  Directories: TArray<string>;
begin
  S := EnginePath;

  { Note: Delphi has 3 SelectDirectory overloads with vastly different UI,
    see http://docwiki.embarcadero.com/Libraries/Sydney/en/Vcl.FileCtrl.SelectDirectory . }

  { Do not use this style: Looks really dated.
  if SelectDirectory(S, [], 0) then
    EnginePath := S;
  }

  { Do not use this style:

    The validation message is bad UX, main Delphi IDE form is then
    shown, but modal form to choose another directory blocks it.
    Maybe it is better if one passes non-nil Parent (last parameter),
    but we don't have it, we only have Services.GetParentHandle:HWND.
    (from IOTAServices)

  if SelectDirectory('Castle Game Engine Path', '', S,
      [sdShowEdit, sdNewUI, sdValidateDir], nil) then
  }

  if SelectDirectory('', Directories, [], 'Castle Game Engine Path') then
  begin
    S := Directories[0];
    if not SysUtils.DirectoryExists(S) then
      raise Exception.CreateFmt('Directory "%s" does not exist', [S]);
    if not SysUtils.DirectoryExists(InclPathDelim(S) + 'src') then
      raise Exception.CreateFmt('Directory "%s" does not look like CGE root directory, missing "src" subdirectory', [S]);
    EnginePath := S;
  end;
end;

procedure TCastleDelphiIdeIntegration.ClickOpenEditor(Sender: TObject);
var
  ExeName, Proj, ProjManifest: String;
begin
  // if this does not look like CGE project, abort before even asking for CGE location - this is less confusing
  Proj := GetProjectPath;
  ProjManifest := InclPathDelim(Proj) + 'CastleEngineManifest.xml';
  if not FileExists(ProjManifest) then
    raise Exception.CreateFmt('Missing CastleEngineManifest.xml.' + NL + NL +
      'To open project using Castle Game Engine editor, it needs to have CastleEngineManifest.xml file. See the https://castle-engine.io/control_on_form for details.' + NL + NL +
      'Project path: %s', [
        Proj
      ]);

  // prompt to choose engine path, if not already chosen
  if EnginePath = '' then
    ClickSetEnginePath(nil);

  // if engire path chosen, run editor
  if EnginePath <> '' then
  begin
    ExeName := InclPathDelim(EnginePath) + 'bin' + PathDelim + 'castle-editor' + ExeExtension;
    ExecuteProcess(ExeName, '"' + ProjManifest + '"', Proj);
  end;
end;

function TCastleDelphiIdeIntegration.ProjectDirIsEngineDir(
  const ProjectSrcDir, EngineSrcDir, EngineRelativeToProject: String): Boolean;

  { Normalize directory: only slashes (no \), and not ending with / or \. }
  function NormalizePath(const S: String): String;
  begin
    Result := ExclPathDelim(S);
    Result := SReplaceChars(Result, '\', '/');
  end;

var
  ProjectSrcDirNormalized: String;
begin
  ProjectSrcDirNormalized := NormalizePath(ProjectSrcDir);
  Result :=
    SameFileName(ProjectSrcDirNormalized, NormalizePath(InclPathDelim(EnginePath) + 'src' + PathDelim + EngineSrcDir)) or
    SameFileName(ProjectSrcDirNormalized, NormalizePath(InclPathDelim(EngineRelativeToProject) + 'src' + PathDelim + EngineSrcDir));
end;

procedure TCastleDelphiIdeIntegration.ClickAddPaths(Sender: TObject);
var
  AddedCount: Cardinal;

  { Add EngineSrcDir (relative to CGE src/) to project source paths, unless
    it is there already. }
  procedure AddEngineSrcDir(const EngineSrcDir: String; const ProjectSrcDirs: TStringList;
    const EngineRelativeToProject: String);
  var
    HasEngineSrcDir: Boolean;
    ProjectSrcDir, NewDir: String;
  begin
    HasEngineSrcDir := false;
    for ProjectSrcDir in ProjectSrcDirs do
      if ProjectDirIsEngineDir(ProjectSrcDir, EngineSrcDir,
           EngineRelativeToProject) then
        HasEngineSrcDir := true;
    if not HasEngineSrcDir then
    begin
      NewDir := InclPathDelim(EnginePath) + 'src' + PathDelim + EngineSrcDir;
      { Use platform-specific path delimiter consistently, to look nice.
        Do not use /, since Delphi IDE uses \ in project options by convention,
        so we want our paths to look consistent. }
      NewDir := SReplaceChars(NewDir, '/', PathDelim);
      ProjectSrcDirs.Add(NewDir);
      Inc(AddedCount);
    end;
  end;

var
  EngineSrcDir, EngineRelativeToProject: String;
  ProjectSrcDirs: TStringList;
begin
  if GetActiveProject = nil then
    raise Exception.Create('No active Delphi project');

  // prompt to choose engine path, if not already chosen
  if EnginePath = '' then
    ClickSetEnginePath(nil);

  if EnginePath <> '' then
  begin
    ProjectSrcDirs := TStringList.Create;
    try
      ProjectSrcDirs.Delimiter := ';';
      ProjectSrcDirs.DelimitedText  := GetActiveProject.ProjectOptions.Values['SrcDir'];
      { calculate it once, otherwise every ProjectDirIsEngineDir would need to do this }
      EngineRelativeToProject := ExtractRelativePath(GetProjectPath, EnginePath);
      AddedCount := 0;

      for EngineSrcDir in EnginePaths do
        AddEngineSrcDir(EngineSrcDir, ProjectSrcDirs, EngineRelativeToProject);
      for EngineSrcDir in EnginePathsDelphi do
        AddEngineSrcDir(EngineSrcDir, ProjectSrcDirs, EngineRelativeToProject);

      if AddedCount <> 0 then
      begin
        GetActiveProject.ProjectOptions.Values['SrcDir'] := ProjectSrcDirs.DelimitedText;
        ShowMessageFmt('Added %d paths to the project source directories', [AddedCount]);
      end else
        ShowMessage('No paths added, the project already uses the engine correctly');
    finally FreeAndNil(ProjectSrcDirs) end;
  end;
end;

procedure TCastleDelphiIdeIntegration.ClickRemovePaths(Sender: TObject);

  { Is project source path equivalent to some engine standard dir. }
  function ProjectDirIsSomeEngineDir(const ProjectSrcDir, EngineRelativeToProject: String): Boolean;
  var
    EngineSrcDir: String;
  begin
    for EngineSrcDir in EnginePaths do
      if ProjectDirIsEngineDir(ProjectSrcDir, EngineSrcDir, EngineRelativeToProject) then
        Exit(true);
    for EngineSrcDir in EnginePathsDelphi do
      if ProjectDirIsEngineDir(ProjectSrcDir, EngineSrcDir, EngineRelativeToProject) then
        Exit(true);
    Result := false;
  end;

var
  RemovedCount: Cardinal;
  EngineRelativeToProject: String;
  ProjectSrcDirs: TStringList;
  I: Integer;
begin
  if GetActiveProject = nil then
    raise Exception.Create('No active Delphi project');

  // prompt to choose engine path, if not already chosen
  if EnginePath = '' then
    ClickSetEnginePath(nil);

  if EnginePath <> '' then
  begin
    ProjectSrcDirs := TStringList.Create;
    try
      ProjectSrcDirs.Delimiter := ';';
      ProjectSrcDirs.DelimitedText  := GetActiveProject.ProjectOptions.Values['SrcDir'];
      { calculate it once, otherwise every ProjectDirIsEngineDir would need to do this }
      EngineRelativeToProject := ExtractRelativePath(GetProjectPath, EnginePath);
      RemovedCount := 0;

      I := 0;
      while I < ProjectSrcDirs.Count do
      begin
        if ProjectDirIsSomeEngineDir(ProjectSrcDirs[I], EngineRelativeToProject) then
        begin
          ProjectSrcDirs.Delete(I);
          Inc(RemovedCount);
        end else
          Inc(I);
      end;

      if RemovedCount <> 0 then
      begin
        GetActiveProject.ProjectOptions.Values['SrcDir'] := ProjectSrcDirs.DelimitedText;
        ShowMessageFmt('Removed %d paths from the project source directories', [RemovedCount]);
      end else
        ShowMessage('No paths removed, the project did not use the engine');
    finally FreeAndNil(ProjectSrcDirs) end;
  end;
end;

procedure TCastleDelphiIdeIntegration.ClickWebsite(Sender: TObject);
begin
  OpenUrl('https://castle-engine.io/');
end;

procedure TCastleDelphiIdeIntegration.ClickApiReference(Sender: TObject);

  function ApiReferenceUrl: String;
  begin
    Result := ApiReferenceUrlCore(EnginePath);
  end;

begin
  OpenUrl(ApiReferenceUrl + 'index.html');
end;

procedure TCastleDelphiIdeIntegration.ClickDonate(Sender: TObject);
begin
  OpenUrl('https://patreon.com/castleengine/');
end;

{ initialization / finalization ---------------------------------------------- }

var
  DelphiIdeIntegration: TCastleDelphiIdeIntegration;

// procedure Register;
// begin
// end;

initialization
  { It seems it doesn't matter do we put this in Register or unit initialization.
    Sample on
    https://docwiki.embarcadero.com/CodeExamples/Athens/en/Third-Party_Help_Menu_Item_(Delphi)
    does this in unit initilization / finalization, so we follow it, }
  OnGetDesignTimeProjectPath := GetProjectPath;
  DelphiIdeIntegration := TCastleDelphiIdeIntegration.Create(nil);

  // Does not seem necessary in practice, so not doing this.
  { Without this, package is loaded (and so menu items added) only once
    CGE component is accessed. }
  // ForceDemandLoadState(dlDisable);
finalization
  // When unloading the package, make sure to remove menu items
  FreeAndNil(DelphiIdeIntegration);
end.
