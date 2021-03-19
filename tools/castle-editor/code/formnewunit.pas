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
  ButtonPanel, ExtCtrls;

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
    procedure SetUnitType(const AValue: TNewUnitType);
    procedure RefreshUiDependingOnUnitType;
  protected
    procedure DoShow; override;
  public
    { Absolute directory (with final path delim) of the current project.
      Set before ShowModal. }
    ProjectPath: String;

    { Absolute directory (with final path delim) of the current directory where
      unit should be created.
      Set before ShowModal.}
    CurrentPath: String;

    property UnitType: TNewUnitType read FUnitType write SetUnitType default utEmpty;
  end;

var
  NewUnitForm: TNewUnitForm;

implementation

uses CastleFilesUtils, CastleURIUtils, CastleLog, CastleUtils,
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
  { Refresh UI even when setting to the same value,
   to work regardless of the initial designed form state. }
  // if FUnitType = AValue then Exit;

  FUnitType := AValue;

  ComboUnitType.OnChange := nil; // avoid recursive ComboUnitType.OnChange calls
  ComboUnitType.ItemIndex := Ord(AValue);
  ComboUnitType.OnChange := @ComboUnitTypeChange;

  SetEnabledExists(PanelUnitClass, AValue = utClass);
  SetEnabledExists(PanelUnitState, AValue = utState);
end;

procedure TNewUnitForm.DoShow;
var
  RelativeCurrentPath: String;
begin
  inherited;

  // clean UI, except ComboUnitType that should be set by called setting UnitType

  RelativeCurrentPath := ExtractRelativePath(ProjectPath, CurrentPath);
  EditUnitName.Text := 'GameSomething';
  EditUnitFile.Text := RelativeCurrentPath + 'gamesomething.pas';

  RefreshUiDependingOnUnitType;
end;

procedure TNewUnitForm.RefreshUiDependingOnUnitType;

  function CheckUnitToInitializeState(const RelativeFileName: String): Boolean;
  var
    Content: String;
  begin
    try
      Content := FileToString(FilenameToURISafe(ProjectPath + RelativeFileName));
      Result :=
        (Pos('// CGE-USE-STATES', Content) <> 0) or
        (Pos('// CGE-CREATE-STATES', Content) <> 0);
    except
      on E: Exception do
      begin
        WritelnWarning('Could not open main unit to initialize states "%s": %s', [
          RelativeFileName,
          E.Message
        ]);
        Result := false;
      end;
    end;
  end;

var
  UnitToInitializeState: String;
  UnitToInitializeStateFound, UnitToInitializeStateValid: Boolean;
begin
  case UnitType of
    utClass:
      EditClassName.Text := 'TSomething';
    utState:
      begin
        UnitToInitializeState := 'code/gameinitialize.pas'; // TODO: should be taken from manifest
        UnitToInitializeStateFound := FileExists(ProjectPath + UnitToInitializeState);
        UnitToInitializeStateValid := UnitToInitializeStateFound and
          CheckUnitToInitializeState(UnitToInitializeState);

        EditStateName.Text := 'TStateSomething';
        EditStateFile.Text := 'castle-data:/state_something.castle-user-interface';
        CheckStateInitialize.Checked := UnitToInitializeStateValid;
        CheckStateInitialize.Enabled := UnitToInitializeStateValid;

        if UnitToInitializeStateValid then
          LabelStateInitializeInfo.Caption := Format(
            'Select above checkbox to modify %s to add state initialization.',
            [UnitToInitializeState])
        else
        if UnitToInitializeStateFound then
          LabelStateInitializeInfo.Caption := Format(
            'WARNING: Found %s, but it is missing special CGE-XXX comments (see the new project templates for example).' + NL +
            'You will need to manually create the new state in Application.OnInitialize.',
            [UnitToInitializeState])
        else
          LabelStateInitializeInfo.Caption := Format(
            'WARNING: Cannot find %s.' + NL +
            'You will need to manually create the new state in Application.OnInitialize.',
            [UnitToInitializeState]);
      end;
  end;
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

