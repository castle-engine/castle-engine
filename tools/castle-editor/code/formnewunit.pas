{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Dialog to configure new unit properties (TNewUnitForm). }
unit FormNewUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ButtonPanel, ExtCtrls,
  ToolManifest;

type
  TNewUnitType = (utEmpty, utClass, utState);

  { Dialog to configure new unit properties. }
  TNewUnitForm = class(TForm)
    ButtonStateFile: TButton;
    ButtonUnitFile: TButton;
    ButtonPanel1: TButtonPanel;
    CheckStateInitialize: TCheckBox;
    ComboUnitType: TComboBox;
    EditClassName: TEdit;
    EditStateFile: TEdit;
    EditStateName: TEdit;
    EditUnitName: TEdit;
    EditUnitFile: TEdit;
    LabelClassName: TLabel;
    LabelStateFile: TLabel;
    LabelStateInitializeInfo: TLabel;
    LabelStateName: TLabel;
    LabelUnitName: TLabel;
    LabelCreateUnit: TLabel;
    LabelUnitFile: TLabel;
    PanelUnitClass: TPanel;
    PanelUnitState: TPanel;
    procedure ButtonUnitFileClick(Sender: TObject);
    procedure ButtonStateFileClick(Sender: TObject);
    procedure ComboUnitTypeChange(Sender: TObject);
  private
    FUnitType: TNewUnitType;
    { Absolute directory (with final path delim) of the directory where
      unit should be created. }
    FUnitOutputPath: String;
    { Current project manifest. }
    FProjectManifest: TCastleManifest;
    procedure SetUnitType(const AValue: TNewUnitType);
    property UnitType: TNewUnitType read FUnitType write SetUnitType default utEmpty;
    procedure RefreshUiDependingOnUnitType;
  public
    procedure InitializeUi(const AUnitType: TNewUnitType;
      const AUnitOutputPath: String; const AProjectManifest: TCastleManifest);
  end;

var
  NewUnitForm: TNewUnitForm;

implementation

uses CastleFilesUtils, CastleURIUtils, CastleLog, CastleUtils, CastleStringUtils,
  EditorUtils;

{$R *.lfm}

procedure TNewUnitForm.ButtonUnitFileClick(Sender: TObject);
begin
  // TODO: allow to choose directory only (filename is determined by lowercase(unit name) + .pas
  // only within the project
  // TODO: start EditUnitFile with directory from where user clicked the "New Unit" menu item
end;

procedure TNewUnitForm.SetUnitType(const AValue: TNewUnitType);
begin
  if FUnitType = AValue then Exit;
  FUnitType := AValue;
  RefreshUiDependingOnUnitType;
end;

procedure TNewUnitForm.InitializeUi(const AUnitType: TNewUnitType;
  const AUnitOutputPath: String; const AProjectManifest: TCastleManifest);
begin
  FUnitType := AUnitType;
  FUnitOutputPath := AUnitOutputPath;
  FProjectManifest := AProjectManifest;

  RefreshUiDependingOnUnitType;
end;

procedure TNewUnitForm.RefreshUiDependingOnUnitType;

  function CheckUnitToInitializeState(const FileName: String): Boolean;
  var
    Content: String;
  begin
    try
      Content := FileToString(FilenameToURISafe(FileName));
    except
      on E: Exception do
      begin
        WritelnWarning('Could not open main unit to initialize states "%s": %s', [
          FileName,
          E.Message
        ]);
        Exit(false);
      end;
    end;

    Result :=
      (Pos('{ CASTLE-STATE-CREATE-BEGIN }', Content) <> 0) and
      (Pos('{ CASTLE-STATE-CREATE-END }', Content) <> 0) and
      (Pos('{ CASTLE-INITIALIZATION-USES-BEGIN }', Content) <> 0) and
      (Pos('{ CASTLE-INITIALIZATION-USES-END }', Content) <> 0);
  end;

  { Find unit where state initialization takes place.
    Returns '' if not found.
    Returns relative filename (relative to project path) if found. }
  function FindUnitToInitializeState: String;
  var
    UnitNames: TCastleStringList;
    AUnitName, UnitFileNameAbsolute, UnitFileNameRelative: String;
  begin
    UnitNames := CreateTokens(FProjectManifest.GameUnits, WhiteSpaces + [',']);
    try
      for AUnitName in UnitNames do
      begin
        UnitFileNameAbsolute := FProjectManifest.SearchPascalUnit(AUnitName);
        if (UnitFileNameAbsolute <> '') and
           CheckUnitToInitializeState(UnitFileNameAbsolute) then
        begin
          UnitFileNameRelative := ExtractRelativePath(FProjectManifest.Path, UnitFileNameAbsolute);
          Exit(UnitFileNameRelative);
        end;
      end;

      Result := ''; // not found
    finally FreeAndNil(UnitNames) end;
  end;

var
  RelativeUnitPath: String;
  UnitToInitializeState: String;
begin
  ComboUnitType.OnChange := nil; // avoid recursive ComboUnitType.OnChange calls
  ComboUnitType.ItemIndex := Ord(FUnitType);
  ComboUnitType.OnChange := @ComboUnitTypeChange;

  RelativeUnitPath := ExtractRelativePath(FProjectManifest.Path, FUnitOutputPath);

  SetEnabledExists(PanelUnitClass, FUnitType = utClass);
  SetEnabledExists(PanelUnitState, FUnitType = utState);

  case UnitType of
    utEmpty:
      begin
        EditUnitName.Text := 'GameSomething';
      end;
    utClass:
      begin
        EditUnitName.Text := 'GameSomething';
        EditClassName.Text := 'TSomething';
      end;
    utState:
      begin
        UnitToInitializeState := FindUnitToInitializeState;

        EditUnitName.Text := 'GameStateSomething';
        EditStateName.Text := 'TStateSomething';
        EditStateFile.Text := 'castle-data:/gamestatesomething.castle-user-interface';
        CheckStateInitialize.Checked := UnitToInitializeState <> '';
        CheckStateInitialize.Enabled := UnitToInitializeState <> '';

        if UnitToInitializeState <> '' then
          LabelStateInitializeInfo.Caption := Format(
            'Select above checkbox to modify %s to add state initialization.',
            [UnitToInitializeState])
        else
          LabelStateInitializeInfo.Caption :=
            'WARNING: Cannot find unit with state initialization. We search units listed in game_units in CastleEngineManifest.xml, among the search paths, for special CASTLE-XXX comments (see the new project templates for example).' + NL + NL +
            'You will need to manually create the new state in Application.OnInitialize.';
      end;
  end;

  EditUnitFile.Text := RelativeUnitPath + LowerCase(EditUnitName.Text) + '.pas';
end;

procedure TNewUnitForm.ButtonStateFileClick(Sender: TObject);
begin
  // TODO: allow to choose directory and filename, but only within castle-data:/
end;

procedure TNewUnitForm.ComboUnitTypeChange(Sender: TObject);
begin
  UnitType := TNewUnitType(ComboUnitType.ItemIndex);
end;

end.

