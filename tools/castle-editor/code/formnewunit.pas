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
    ButtonStateDir: TButton;
    ButtonUnitDir: TButton;
    ButtonPanel1: TButtonPanel;
    CheckStateInitialize: TCheckBox;
    ComboUnitType: TComboBox;
    EditClassName: TEdit;
    EditDesignDir: TEdit;
    EditStateName: TEdit;
    EditUnitName: TEdit;
    EditUnitDir: TEdit;
    LabelFinalUnitFile: TLabel;
    LabelFinalDesignFile: TLabel;
    LabelClassName: TLabel;
    LabelDesignDir: TLabel;
    LabelStateInitializeInfo: TLabel;
    LabelStateName: TLabel;
    LabelUnitName: TLabel;
    LabelCreateUnit: TLabel;
    LabelUnitDir: TLabel;
    PanelUnitClass: TPanel;
    PanelUnitState: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure ButtonUnitDirClick(Sender: TObject);
    procedure ButtonStateDirClick(Sender: TObject);
    procedure ComboUnitTypeChange(Sender: TObject);
    procedure EditDesignDirChange(Sender: TObject);
    procedure EditUnitDirChange(Sender: TObject);
    procedure EditUnitNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    EditUnitNameOldText: String;
    FUnitType: TNewUnitType;
    { Absolute directory (with final path delim) of the directory where
      unit should be created. }
    FUnitOutputPath: String;
    { Current project manifest. }
    FProjectManifest: TCastleManifest;
    procedure GetFinalFilenames(out FinalUnitFile, FinalDesignFile: String);
    procedure SetUnitType(const AValue: TNewUnitType);
    procedure UpdateFinalFilenames;
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

{ Like ExtractRelativePath but prefer to end result with PathDelim,
  as it just seems cleaner (it is then obvious that edit box shows
  a directory). }
function ExtractRelativePathDelim(const Base, Target: String): String;
begin
  if DirectoryExists(Base) and
     SameFileName(InclPathDelim(Base), InclPathDelim(Target)) then
    Exit(''); // otherwise ExtractRelativePath returns '../my_dir_name/'
  Result := ExtractRelativePath(Base, Target);
  if Result <> '' then
    Result := InclPathDelim(Result);
end;

{ TNewUnitForm --------------------------------------------------------------- }

procedure TNewUnitForm.ButtonUnitDirClick(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := CombinePaths(FProjectManifest.Path, EditUnitDir.Text);
  SelectDirectoryDialog1.FileName := CombinePaths(FProjectManifest.Path, EditUnitDir.Text);
  if SelectDirectoryDialog1.Execute then
  begin
    EditUnitDir.Text := ExtractRelativePathDelim(FProjectManifest.Path, SelectDirectoryDialog1.FileName);
    UpdateFinalFilenames;
  end;
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

  FProjectManifest := AProjectManifest;

  FUnitOutputPath := AUnitOutputPath;
  { place code in code/ subdirectory instead of top-level, if possible }
  if SameFileName(InclPathDelim(FUnitOutputPath), InclPathDelim(FProjectManifest.Path)) and
     DirectoryExists(CombinePaths(FProjectManifest.Path, 'code')) then
    FUnitOutputPath := CombinePaths(FProjectManifest.Path, 'code');

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

const
  ButtonsMargin = 16;
var
  RelativeUnitPath: String;
  UnitToInitializeState: String;
begin
  ComboUnitType.OnChange := nil; // avoid recursive ComboUnitType.OnChange calls
  ComboUnitType.ItemIndex := Ord(FUnitType);
  ComboUnitType.OnChange := @ComboUnitTypeChange;

  RelativeUnitPath := ExtractRelativePathDelim(FProjectManifest.Path, FUnitOutputPath);
  EditUnitDir.Text := RelativeUnitPath;

  SetEnabledExists(PanelUnitClass, FUnitType = utClass);
  SetEnabledExists(PanelUnitState, FUnitType = utState);

  case UnitType of
    utEmpty:
      begin
        EditUnitName.Text := 'GameSomething';

        { adjust form height }
        ClientHeight := LabelFinalUnitFile.Top + LabelFinalUnitFile.Height + ButtonsMargin + ButtonPanel1.Height;
      end;
    utClass:
      begin
        EditUnitName.Text := 'GameSomething';
        EditClassName.Text := 'TSomething';

        { adjust form height }
        ClientHeight := PanelUnitClass.Top + PanelUnitClass.Height + ButtonsMargin + ButtonPanel1.Height;
      end;
    utState:
      begin
        UnitToInitializeState := FindUnitToInitializeState;

        EditUnitName.Text := 'GameStateSomething';
        EditStateName.Text := 'TStateSomething';
        EditDesignDir.Text := 'data/';
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

        { adjust form height }
        PanelUnitState.ClientHeight := LabelStateInitializeInfo.Top + LabelStateInitializeInfo.Height;
        ClientHeight := PanelUnitState.Top + PanelUnitState.Height + ButtonsMargin + ButtonPanel1.Height;
      end;
  end;

  EditUnitNameOldText := EditUnitName.Text;
  UpdateFinalFilenames;
end;

procedure TNewUnitForm.ButtonStateDirClick(Sender: TObject);
var
  DataPath: String;
begin
  SelectDirectoryDialog1.InitialDir := CombinePaths(FProjectManifest.Path, EditDesignDir.Text);
  SelectDirectoryDialog1.FileName := CombinePaths(FProjectManifest.Path, EditDesignDir.Text);
  if SelectDirectoryDialog1.Execute then
  begin
    DataPath := URIToFilenameSafe(ResolveCastleDataURL('castle-data:/'));
    if not IsPrefix(DataPath, InclPathDelim(SelectDirectoryDialog1.FileName), not FileNameCaseSensitive) then
    begin
      MessageDlg('Design outside data', 'You are saving a design outside of the project''s "data" directory.' + NL +
        NL +
        'The state design file will not be automatically referenced correctly from code (using "castle-data:/" protocol) and will not be automatically packaged in the project.' + NL +
        NL +
        'Unless you really know what you''re doing, we heavily advice to change the directory to be inside the project "data" directory.',
        mtWarning, [mbOK], 0);
    end;

    EditDesignDir.Text := ExtractRelativePathDelim(FProjectManifest.Path, SelectDirectoryDialog1.FileName);
    UpdateFinalFilenames;
  end;
end;

procedure TNewUnitForm.ComboUnitTypeChange(Sender: TObject);
begin
  UnitType := TNewUnitType(ComboUnitType.ItemIndex);
end;

procedure TNewUnitForm.EditDesignDirChange(Sender: TObject);
begin
  UpdateFinalFilenames;
end;

procedure TNewUnitForm.EditUnitDirChange(Sender: TObject);
begin
  UpdateFinalFilenames;
end;

procedure TNewUnitForm.EditUnitNameChange(Sender: TObject);
begin
  { automatically change lower edit boxes, if they matched }
  if SameText(EditClassName.Text, 'T' + EditUnitNameOldText) or
     SameText(EditClassName.Text, 'T' + PrefixRemove('game', EditUnitNameOldText, true)) then
    EditClassName.Text := 'T' + PrefixRemove('game', EditUnitName.Text, true);

  if SameText(EditStateName.Text, 'T' + EditUnitNameOldText) or
     SameText(EditStateName.Text, 'T' + PrefixRemove('game', EditUnitNameOldText, true)) then
    EditStateName.Text := 'T' + PrefixRemove('game', EditUnitName.Text, true);

  EditUnitNameOldText := EditUnitName.Text;

  UpdateFinalFilenames;
end;

procedure TNewUnitForm.FormShow(Sender: TObject);
begin
  ActiveControl := EditUnitName; // set focus on EditUnitName each time you open this form
end;

procedure TNewUnitForm.GetFinalFilenames(out FinalUnitFile, FinalDesignFile: String);
begin
  FinalUnitFile := EditUnitDir.Text + LowerCase(EditUnitName.Text) + '.pas';
  FinalDesignFile := EditDesignDir.Text + LowerCase(EditUnitName.Text) + '.castle-user-interface';
end;

procedure TNewUnitForm.UpdateFinalFilenames;
var
  FinalUnitFile, FinalDesignFile: String;
begin
  GetFinalFilenames(FinalUnitFile, FinalDesignFile);
  LabelFinalUnitFile.Caption := 'Final Unit File: ' + FinalUnitFile;
  LabelFinalDesignFile.Caption := 'Final Design File: ' + FinalDesignFile;
end;

end.

