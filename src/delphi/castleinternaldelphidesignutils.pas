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
  At registration, this sets OnGetDesignTimeProjectPath so that other code
  (design-time or not only design-time) can use it, if assigned. }
unit CastleInternalDelphiDesignUtils;

interface

procedure Register;

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
}

uses SysUtils, Classes,
  ToolsAPI, // design-time only unit
  Vcl.Menus, Vcl.Dialogs,
  CastleInternalDelphiUtils;

function GetProjectPath: String;
begin
  Result := ExtractFilePath(GetActiveProject.FileName);
end;

{ TCastleDelphiIdeIntegration ----------------------------------------------- }

type
  TCastleDelphiIdeIntegration = class(TComponent)
  strict private
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
  CgeMenu, ChangeEnginePathMenu, OpenEditorMenu, AddPathsMenu, RemovePathsMenu: TMenuItem;
begin
  inherited ;
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
  CgeMenu.Add(ChangeEnginePathMenu);
  CgeMenu.Add(OpenEditorMenu);
  CgeMenu.Add(AddPathsMenu);
  CgeMenu.Add(RemovePathsMenu);

  // Use hardcoded 'ToolsMenu', this is good acoording to
  // https://docwiki.embarcadero.com/RADStudio/Athens/en/Adding_an_Item_to_the_Main_Menu_of_the_IDE
  Services.AddActionMenu('ToolsMenu', nil, CgeMenu, true, true);
end;

destructor TCastleDelphiIdeIntegration.Destroy;
begin
  // will automatically free menu items owned by this
  inherited;
end;

procedure TCastleDelphiIdeIntegration.ClickChangeEnginePath(Sender: TObject);
begin
end;

procedure TCastleDelphiIdeIntegration.ClickOpenEditor(Sender: TObject);
begin
end;

procedure TCastleDelphiIdeIntegration.ClickAddPaths(Sender: TObject);
var
  OptionName: TOTAOptionName;
  ProjectOptions: IOTAProjectOptions;
  Report: TStringList;
  ReportFileName, ValueStr: String;
begin
  ShowMessage('hello from menu item');

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
end;

{ initialization / finalization ---------------------------------------------- }

var
  DelphiIdeIntegration: TCastleDelphiIdeIntegration;

procedure Register;
begin
  OnGetDesignTimeProjectPath := GetProjectPath;

  if DelphiIdeIntegration = nil then
    DelphiIdeIntegration := TCastleDelphiIdeIntegration.Create(nil);
end;

initialization
finalization
  // When unloading the package, make sure to remove menu item
  FreeAndNil(DelphiIdeIntegration);
end.
