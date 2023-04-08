{
  Copyright 2021-2023 Michalis Kamburelis.

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
  TNewUnitType = (utEmpty, utClass, utView, utBehavior);

  { Dialog to configure new unit properties. }
  TNewUnitForm = class(TForm)
    ButtonViewDir: TButton;
    ButtonUnitDir: TButton;
    ButtonPanel1: TButtonPanel;
    CheckViewInitialize: TCheckBox;
    ComboUnitType: TComboBox;
    EditClassName: TEdit;
    EditDesignDir: TEdit;
    EditBaseName: TEdit;
    EditUnitName: TEdit;
    EditUnitDir: TEdit;
    LabelFinalUnitFile: TLabel;
    LabelFinalDesignFile: TLabel;
    LabelClassName: TLabel;
    LabelDesignDir: TLabel;
    LabelBaseName: TLabel;
    LabelViewInitializeInfo: TLabel;
    LabelUnitName: TLabel;
    LabelCreateUnit: TLabel;
    LabelUnitDir: TLabel;
    PanelUnitClass: TPanel;
    PanelUnitView: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure ButtonUnitDirClick(Sender: TObject);
    procedure ButtonViewDirClick(Sender: TObject);
    procedure ComboUnitTypeChange(Sender: TObject);
    procedure EditBaseNameChange(Sender: TObject);
    procedure EditDesignDirChange(Sender: TObject);
    procedure EditUnitDirChange(Sender: TObject);
    procedure EditUnitNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    FCreatedUnitRelative, FCreatedDesignRelative: String;
    FCreatedUnitAbsolute, FCreatedDesignAbsolute: String;
    FUnitType: TNewUnitType;
    { Absolute directory (with final path delim) of the directory where
      unit should be created. }
    FUnitOutputPath: String;
    { Current project manifest. }
    FProjectManifest: TCastleManifest;
    UnitToInitializeView: String;
    procedure GetFinalFilenames(out FinalUnitRelative, FinalDesignRelative: String);
    procedure GetFinalFilenames(out FinalUnitRelative, FinalDesignRelative: String;
      out FinalUnitAbsolute, FinalDesignAbsolute: String);
    procedure SetUnitType(const AValue: TNewUnitType);
    procedure UpdateFinalFilenames;
    property UnitType: TNewUnitType read FUnitType write SetUnitType default utEmpty;
    procedure RefreshUiDependingOnUnitType;
  public
    { After ShowModel, the Created* contain filenames created (or empty if none). }
    property CreatedUnitRelative: String read FCreatedUnitRelative;
    property CreatedUnitAbsolute: String read FCreatedUnitAbsolute;
    property CreatedDesignRelative: String read FCreatedDesignRelative;
    property CreatedDesignAbsolute: String read FCreatedDesignAbsolute;

    procedure InitializeUi(const AUnitType: TNewUnitType;
      const AUnitOutputPath: String; const AProjectManifest: TCastleManifest);
  end;

var
  NewUnitForm: TNewUnitForm;

implementation

uses Generics.Collections,
  CastleFilesUtils, CastleURIUtils, CastleLog, CastleUtils, CastleStringUtils,
  EditorUtils, ProjectUtils, EditorCodeTools;

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
  RefreshUiDependingOnUnitType;
end;

procedure TNewUnitForm.RefreshUiDependingOnUnitType;
const
  ButtonsMargin = 16;
var
  RelativeUnitPath: String;
begin
  ComboUnitType.OnChange := nil; // avoid recursive ComboUnitType.OnChange calls
  ComboUnitType.ItemIndex := Ord(FUnitType);
  ComboUnitType.OnChange := @ComboUnitTypeChange;

  RelativeUnitPath := ExtractRelativePathDelim(FProjectManifest.Path, FUnitOutputPath);
  EditUnitDir.Text := RelativeUnitPath;

  SetEnabledVisible(PanelUnitClass, FUnitType in [utClass, utBehavior, utView]);
  SetEnabledVisible(PanelUnitView, FUnitType = utView);

  EditBaseName.Text := 'Something';

  case UnitType of
    utEmpty:
      begin
        { adjust form height }
        ClientHeight := LabelFinalUnitFile.Top + LabelFinalUnitFile.Height + ButtonsMargin + ButtonPanel1.Height;
      end;
    utClass, utBehavior:
      begin
        { adjust form height }
        ClientHeight := PanelUnitClass.Top + PanelUnitClass.Height + ButtonsMargin + ButtonPanel1.Height;
      end;
    utView:
      begin
        UnitToInitializeView := FindUnitToInitializeView(FProjectManifest);

        EditDesignDir.Text := 'data/';
        CheckViewInitialize.Checked := UnitToInitializeView <> '';
        CheckViewInitialize.Enabled := UnitToInitializeView <> '';

        if UnitToInitializeView <> '' then
          LabelViewInitializeInfo.Caption := Format(
            'Select above checkbox to modify %s to add view initialization.', [
            UnitToInitializeView
          ])
        else
          LabelViewInitializeInfo.Caption :=
            'WARNING: Cannot find unit with view initialization. We search units listed in game_units in CastleEngineManifest.xml, among the search paths, for special comments like {$region ''Castle...''} (see the new project templates for a precise example).' + NL + NL +
            'You will need to manually create the view somewhere, like "ViewMy := TViewMy.Create(Application);".';

        { adjust form height }
        PanelUnitView.ClientHeight := LabelViewInitializeInfo.Top + LabelViewInitializeInfo.Height;
        ClientHeight := PanelUnitView.Top + PanelUnitView.Height + ButtonsMargin + ButtonPanel1.Height;
      end;
  end;

  EditBaseNameChange(nil);
end;

procedure TNewUnitForm.ButtonViewDirClick(Sender: TObject);
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
        'The view design file will not be automatically referenced correctly from code (using "castle-data:/" protocol) and will not be automatically packaged in the project.' + NL +
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

procedure TNewUnitForm.EditBaseNameChange(Sender: TObject);
begin
  { automatically change lower edit boxes }
  case UnitType of
    utBehavior:
      begin
        EditClassName.Text := 'T' + EditBaseName.Text + 'Behavior';
        EditUnitName.Text := 'Game' + EditBaseName.Text + 'Behavior';
      end;
    utView:
      begin
        EditClassName.Text := 'TView' + EditBaseName.Text;
        EditUnitName.Text := 'GameView' + EditBaseName.Text;
      end;
    else
      begin
        EditClassName.Text := 'T' + EditBaseName.Text;
        EditUnitName.Text := 'Game' + EditBaseName.Text;
      end;
  end;

  UpdateFinalFilenames;
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
  UpdateFinalFilenames;
end;

procedure TNewUnitForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);

  function CheckValidPascalIdentifiers: Boolean;
  begin
    Result := true;

    if not IsValidIdent(EditUnitName.Text, true) then
    begin
      ErrorBox(Format('Unit name "%s" is not a valid Pascal identifier.', [
        EditUnitName.Text
      ]));
      Exit(false);
    end;

    if (UnitType in [utClass, utBehavior]) and (not IsValidIdent(EditClassName.Text, true)) then
    begin
      ErrorBox(Format('Class name "%s" is not a valid Pascal identifier.', [
        EditClassName.Text
      ]));
      Exit(false);
    end;

    if (UnitType = utView) and (not IsPrefix('t', EditClassName.Text, true)) then
    begin
      ErrorBox(Format('View name "%s" must start with letter "T" (following Pascal conventions for type names; this allows to declare singleton variable, with consistent name but without the "T" prefix).', [
        EditClassName.Text
      ]));
      Exit(false);
    end;

    if (UnitType = utView) and
       SameText(PrefixRemove('t', EditClassName.Text, true), EditUnitName.Text) then
    begin
      ErrorBox(Format('View unit name "%s" must be different than class name (with "T" prefix removed), otherwise we cannot declare view singleton variable.', [
        EditUnitName.Text
      ]));
      Exit(false);
    end;
  end;

  procedure CreateFiles(
    const FinalUnitRelative, FinalDesignRelative: String;
    const FinalUnitAbsolute, FinalDesignAbsolute: String);
  var
    Macros: TStringStringMap;
    TemplateSource, Contents, ViewVariableName: String;
  begin
    Macros := TStringStringMap.Create;
    try
      Macros.Add('${UNIT_NAME}', EditUnitName.Text);
      Macros.Add('${BASE_NAME}', EditBaseName.Text);
      case UnitType of
        utEmpty:
          begin
            TemplateSource := 'newunit.pas';
          end;
        utClass:
          begin
            TemplateSource := 'newunitclass.pas';
            Macros.Add('${CLASS_NAME}', EditClassName.Text);
          end;
        utBehavior:
          begin
            TemplateSource := 'newunitbehavior.pas';
            Macros.Add('${CLASS_NAME}', EditClassName.Text);
          end;
        utView:
          begin
            TemplateSource := 'newunitview.pas';
            ViewVariableName := PrefixRemove('t', EditClassName.Text, true);
            Macros.Add('${VIEW_CLASS_NAME}', EditClassName.Text);
            Macros.Add('${VIEW_VARIABLE_NAME}', ViewVariableName);
            Macros.Add('${DESIGN_FILE_URL}', MaybeUseDataProtocol(FilenameToURISafe(FinalDesignAbsolute)));

            StringToFile(FinalDesignAbsolute, FileToString(
              InternalCastleDesignData + 'templates/newunitview.castle-user-interface'));
            FCreatedDesignRelative := FinalDesignRelative;
            FCreatedDesignAbsolute := FinalDesignAbsolute;

            if CheckViewInitialize.Checked then
            begin
              Assert(UnitToInitializeView <> '');
              AddInitializeView(CombinePaths(FProjectManifest.Path, UnitToInitializeView),
                EditUnitName.Text,
                EditClassName.Text,
                ViewVariableName
              );
            end;
          end;
      end;

      Contents := FileToString(InternalCastleDesignData + 'templates/' + TemplateSource);
      Contents := SReplacePatterns(Contents, Macros, false);
      StringToFile(FinalUnitAbsolute, Contents);
      FCreatedUnitRelative := FinalUnitRelative;
      FCreatedUnitAbsolute := FinalUnitAbsolute;
    finally FreeAndNil(Macros) end;
  end;

  function CheckCanOverwriteFiles(
    const FinalUnitRelative, FinalDesignRelative: String;
    const FinalUnitAbsolute, FinalDesignAbsolute: String): Boolean;
  begin
    Result := true;

    Assert(FinalUnitAbsolute <> '');
    if FileExists(FinalUnitAbsolute) or DirectoryExists(FinalUnitAbsolute) then
    begin
      if not YesNoBox('Overwrite unit', Format('Unit file already exists: "%s".' + NL + NL + 'Overwrite file?', [
        FinalUnitRelative
      ])) then
        Exit(false);
    end;

    if (FinalDesignAbsolute <> '') and
       (FileExists(FinalDesignAbsolute) or DirectoryExists(FinalDesignAbsolute)) then
    begin
      if not YesNoBox('Overwrite design', Format('Design file already exists: "%s".' + NL + NL + 'Overwrite file?', [
        FinalDesignRelative
      ])) then
        Exit(false);
    end;
  end;

var
  FinalUnitRelative, FinalDesignRelative: String;
  FinalUnitAbsolute, FinalDesignAbsolute: String;
begin
  // reset output properties
  FCreatedUnitRelative := '';
  FCreatedUnitAbsolute := '';
  FCreatedDesignRelative := '';
  FCreatedDesignAbsolute := '';

  if ModalResult = mrOK then
  begin
    if not CheckValidPascalIdentifiers then
    begin
      CanClose := false;
      Exit;
    end;

    GetFinalFilenames(
      FinalUnitRelative, FinalDesignRelative,
      FinalUnitAbsolute, FinalDesignAbsolute);

    if not CheckCanOverwriteFiles(
      FinalUnitRelative, FinalDesignRelative,
      FinalUnitAbsolute, FinalDesignAbsolute) then
    begin
      CanClose := false;
      Exit;
    end;

    CreateFiles(
      FinalUnitRelative, FinalDesignRelative,
      FinalUnitAbsolute, FinalDesignAbsolute);
  end;
end;

procedure TNewUnitForm.FormShow(Sender: TObject);
begin
  ActiveControl := EditBaseName; // set focus on EditBaseName each time you open this form
  EditBaseName.SelectAll; // allow to easily type something from scratch
end;

procedure TNewUnitForm.GetFinalFilenames(
  out FinalUnitRelative, FinalDesignRelative: String);
begin
  FinalUnitRelative := EditUnitDir.Text + LowerCase(EditUnitName.Text) + '.pas';

  if UnitType = utView then
    FinalDesignRelative := EditDesignDir.Text + LowerCase(EditUnitName.Text) + '.castle-user-interface'
  else
    FinalDesignRelative := '';
end;

procedure TNewUnitForm.GetFinalFilenames(
  out FinalUnitRelative, FinalDesignRelative: String;
  out FinalUnitAbsolute, FinalDesignAbsolute: String);
begin
  GetFinalFilenames(FinalUnitRelative, FinalDesignRelative);

  FinalUnitAbsolute := CombinePaths(FProjectManifest.Path, FinalUnitRelative);

  if FinalDesignRelative <> '' then
    FinalDesignAbsolute := CombinePaths(FProjectManifest.Path, FinalDesignRelative)
  else
    FinalDesignAbsolute := '';
end;

procedure TNewUnitForm.UpdateFinalFilenames;
var
  FinalUnitRelative, FinalDesignRelative: String;
begin
  GetFinalFilenames(FinalUnitRelative, FinalDesignRelative);
  LabelFinalUnitFile.Caption := 'Final Unit File: ' + FinalUnitRelative;
  LabelFinalDesignFile.Caption := 'Final Design File: ' + FinalDesignRelative;
end;

end.

