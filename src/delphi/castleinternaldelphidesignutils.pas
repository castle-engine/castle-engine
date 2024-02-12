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
  Vcl.Menus, Vcl.Dialogs, Vcl.FileCtrl,
  CastleInternalDelphiUtils, CastleConfig, CastleApplicationProperties,
  CastleUtils;

function GetProjectPath: String;
begin
  if GetActiveProject = nil then
    raise Exception.Create('No active Delphi project');

  Result := ExtractFilePath(GetActiveProject.FileName);
end;

{ TCastleDelphiIdeIntegration ----------------------------------------------- }

type
  TCastleDelphiIdeIntegration = class(TComponent)
  strict private
    FEnginePath: String;
    FEnginePathInitialized: Boolean;
    CgeMenu, ChangeEnginePathMenu, OpenEditorMenu, AddPathsMenu, RemovePathsMenu: TMenuItem;

    procedure EnsureConfigInitialized;

    function GetEnginePath: String;
    procedure SetEnginePath(const Value: String);
    property EnginePath: String read GetEnginePath write SetEnginePath;

    { Menu item handlers. }
    procedure ClickChangeEnginePath(Sender: TObject);
    procedure ClickOpenEditor(Sender: TObject);
    procedure ClickAddPaths(Sender: TObject);
    procedure ClickRemovePaths(Sender: TObject);
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

  ChangeEnginePathMenu := TMenuItem.Create(Self);
  ChangeEnginePathMenu.Caption := 'Change Engine Path...';
  // Configure the path of Castle Game Engine (should have subdirectories like src, examples).
  ChangeEnginePathMenu.OnClick := ClickChangeEnginePath;

  OpenEditorMenu := TMenuItem.Create(Self);
  OpenEditorMenu.Caption := 'Open Editor';
  OpenEditorMenu.OnClick := ClickOpenEditor;

  AddPathsMenu := TMenuItem.Create(Self);
  AddPathsMenu.Caption := 'Configure Currrent Project to Use Engine';
  AddPathsMenu.OnClick := ClickAddPaths;

  RemovePathsMenu := TMenuItem.Create(Self);
  RemovePathsMenu.Caption := 'Remove Engine Configuration from the Currrent Project';
  RemovePathsMenu.OnClick := ClickRemovePaths;

  CgeMenu := TMenuItem.Create(Self);
  CgeMenu.Caption := 'Castle Game Engine';
  CgeMenu.Name := 'CastleGameEngineMenu';
//  CgeMenu.Add(ChangeEnginePathMenu);
//  CgeMenu.Add(OpenEditorMenu);
//  CgeMenu.Add(AddPathsMenu);
//  CgeMenu.Add(RemovePathsMenu);

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

      Services.AddActionMenu('ToolsToolsItem', nil, CgeMenu, true, true);

    So a custom "Tools" submenu should be added before "Configure Tools". }
  Services.AddActionMenu('ViewTranslationManagerMenu', nil, CgeMenu, true, false);

  { We can add submenu items using CgeMenu.Add (see above) or by adding
    using Services.AddActionMenu.
    There doesn't seem to be any difference. }
  Services.AddActionMenu('CastleGameEngineMenu', nil, ChangeEnginePathMenu, true, true);
  Services.AddActionMenu('CastleGameEngineMenu', nil, OpenEditorMenu, true, true);
  Services.AddActionMenu('CastleGameEngineMenu', nil, AddPathsMenu, true, true);
  Services.AddActionMenu('CastleGameEngineMenu', nil, RemovePathsMenu, true, true);
end;

destructor TCastleDelphiIdeIntegration.Destroy;
begin
  { Note: It seems that CgeMenu is not removed from ToolsMenu automatically when
    TTestDelphiIdeIntegration instance is destroyed,
    even when CgeMenu is owned by TTestDelphiIdeIntegration (was created as
    "CgeMenu := TMenuItem.Create(Self)").
    Freing it explicitly here works.

    Note that we shouldn't free subitems like ChangeEnginePathMenu,
    they will be freed automatically
    (trying to free them would result in invalid pointer exceptions).

    Why? It seems Delphi IDE does something funny and changes ownership
    of items added using Services.AddActionMenu -- so they are owned by their parents
    effectively?

    We create them all now with Owner=nil, to avoid confusion.
  }

  FreeAndNil(CgeMenu);
  // FreeAndNil(ChangeEnginePathMenu);
  // FreeAndNil(OpenEditorMenu);
  // FreeAndNil(AddPathsMenu);
  // FreeAndNil(RemovePathsMenu);

  inherited;
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

procedure TCastleDelphiIdeIntegration.ClickChangeEnginePath(Sender: TObject);
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

  { Run process.
    @param(Parameters Can specify multiple parameters,
      suffering from the same weirdness as underlying Windows API
      -- instead of taking a list of String,
      all parameters are specified as one long String with parameters
      separated by spaces and optionally surrounded by double quotes.) }
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

var
  ExeName, Proj, ProjManifest: String;
begin
  // if this does not look like CGE project, abort before even asking for CGE location - this is less confusing
  Proj := GetProjectPath;
  ProjManifest := InclPathDelim(Proj) + 'CastleEngineManifest.xml';
  if not FileExists(ProjManifest) then
    raise Exception.CreateFmt('Missing CastleEngineManifest.xml, this does not look like a Castle Game Engine project: %s', [
      Proj
    ]);

  // prompt to choose engine path, if not already chosen
  if EnginePath = '' then
    ClickChangeEnginePath(nil);

  // if engire path chosen, run editor
  if EnginePath <> '' then
  begin
    ExeName := InclPathDelim(EnginePath) + 'bin' + PathDelim + 'castle-editor' + ExeExtension;
    ExecuteProcess(ExeName, '"' + ProjManifest + '"', Proj);
  end;
end;

procedure TCastleDelphiIdeIntegration.ClickAddPaths(Sender: TObject);
var
  OptionName: TOTAOptionName;
  ProjectOptions: IOTAProjectOptions;
  Report: TStringList;
  ReportFileName, ValueStr: String;
begin
  ShowMessage('TODO: Not implemented yet');
  Exit;

  Report := TStringList.Create;
  try
    ProjectOptions := GetActiveProject.ProjectOptions;

    for OptionName in ProjectOptions.GetOptionNames do
    begin
      try
        ValueStr := ProjectOptions.Values[OptionName.Name];
      except
        ValueStr := 'Error reading as String';
      end;
      Report.Append(Format('%s: %s', [
        OptionName.Name,
        ValueStr
      ]));
    end;

    ReportFileName := 'd:/cygwin64/tmp/' + IntToStr(Random(100000));
    Report.SaveToFile(ReportFileName);

    ShowMessage(Format('Found %d options, saved to %s', [
      Length(ProjectOptions.GetOptionNames),
      ReportFileName
    ]));
  finally FreeAndNil(Report) end;
end;

procedure TCastleDelphiIdeIntegration.ClickRemovePaths(Sender: TObject);
begin
  ShowMessage('TODO: Not implemented yet');
  Exit;
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
