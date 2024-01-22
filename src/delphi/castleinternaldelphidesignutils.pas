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
    procedure TestMenuItemClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TCastleDelphiIdeIntegration.Create(AOwner: TComponent);
var
  Services: INTAServices;
  MainMenu: TMainMenu;
  TestMenuItem: TMenuItem;
begin
  inherited ;
  Services := BorlandIDEServices as INTAServices;
  MainMenu := Services.MainMenu;

  // TODO: Work in progress to add menu in proper place
  // TestMenuItem := TMenuItem.Create(Self);
  // TestMenuItem.Caption := 'Castle Test';
  // TestMenuItem.OnClick := TestMenuItemClick;

  // MainMenu.Items.Insert(0, TestMenuItem);
end;

destructor TCastleDelphiIdeIntegration.Destroy;
begin
  // will automatically free TestMenuItem owned by this
  inherited;
end;

procedure TCastleDelphiIdeIntegration.TestMenuItemClick(Sender: TObject);
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
