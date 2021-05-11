{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Form to configure CGE editor (@link(TPreferencesForm)). }
unit FormPreferences;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  ExtCtrls, ButtonPanel;

type
  TPreferencesForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DirectoryEditFpc: TDirectoryEdit;
    DirectoryEditLazarus: TDirectoryEdit;
    EditCodeEditorCommand: TFileNameEdit;
    LabelCodeEditorCommandInstructions: TLabel;
    LabelCodeEditorHeader: TLabel;
    LabelFpc: TLabel;
    LabelFpcAutoDetectedCaption: TLabel;
    LabelInstructions0: TLabel;
    LabelLazarus: TLabel;
    LabelInstructions1: TLabel;
    LabelLazarusAutoDetectedCaption: TLabel;
    LabelTitle: TLabel;
    LabelInstructions2: TLabel;
    LabelFpcAutoDetected: TLabel;
    LabelLazarusAutoDetected: TLabel;
    LabelLazarusWebsite: TLabel;
    ListPages: TListBox;
    PanelCodeEditor: TPanel;
    PanelFpcLazarusConfig: TPanel;
    RadioCodeEditorLazarus: TRadioButton;
    RadioCodeEditorCommand: TRadioButton;
    procedure DirectoryEditFpcChange(Sender: TObject);
    procedure DirectoryEditLazarusChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure LabelLazarusWebsiteClick(Sender: TObject);
    procedure ListPagesClick(Sender: TObject);
  private
    OriginalFpcCustomPath, OriginalLazarusCustomPath: String;
    procedure UpdateAutoDetectedLabels;
    procedure UpdatePageVisible;
  public

  end;

var
  PreferencesForm: TPreferencesForm;

implementation

uses CastleOpenDocument, CastleUtils, CastleLog,
  ToolCompilerInfo, ToolFpcVersion,
  EditorUtils;

{$R *.lfm}

{ TPreferencesForm }

procedure TPreferencesForm.LabelLazarusWebsiteClick(Sender: TObject);
begin
  OpenURL('https://www.lazarus-ide.org/');
end;

procedure TPreferencesForm.ListPagesClick(Sender: TObject);
begin
  UpdatePageVisible;
end;

procedure TPreferencesForm.UpdateAutoDetectedLabels;
var
  FpcExe, FpcVer, LazarusExe: String;
begin
  try
    FpcExe := FindExeFpcCompiler;
    FpcVer := FpcVersion.ToString;
    LabelFpcAutoDetected.Caption :=
      'FPC executable: ' + FpcExe + NL +
      'FPC version: ' + FpcVer;
  except
    on E: Exception do
    begin
      WritelnLog('Cannot determine FPC executable or version, error: ' + ExceptMessage(E));
      LabelFpcAutoDetected.Caption :=
        'Cannot determine FPC executable.' + NL +
        'Make sure FPC is installed, and available on $PATH or configured above.';
    end;
  end;

  try
    LazarusExe := FindExeLazarusIDE;
    LabelLazarusAutoDetected.Caption :=
      'Lazarus executable: ' + LazarusExe;
  except
    on E: EExecutableNotFound do
    begin
      WritelnLog('Cannot determine Lazarus executable, error: ' + ExceptMessage(E));
      LabelLazarusAutoDetected.Caption :=
        'Cannot determine Lazarus executable.' + NL +
        'Make sure Lazarus is installed, and available on $PATH or configured above.';
    end;
  end;
end;

procedure TPreferencesForm.FormShow(Sender: TObject);
begin
  // ListPages.ItemIndex := ; // leave previous
  UpdatePageVisible;

  UpdateAutoDetectedLabels;

  DirectoryEditFpc.Directory := FpcCustomPath;
  DirectoryEditLazarus.Directory := LazarusCustomPath;
  { We will change the global Fpc/LazarusCustomPath during this dialog,
    so allow to revert them on "Cancel". }
  OriginalFpcCustomPath := FpcCustomPath;
  OriginalLazarusCustomPath := LazarusCustomPath;

  if Trim(CodeEditor) <> '' then
  begin
    RadioCodeEditorCommand.Checked := true; // will uncheck other Radio...
    EditCodeEditorCommand.Text := CodeEditor;
  end else
  begin
    RadioCodeEditorLazarus.Checked := true; // will uncheck other Radio...
    EditCodeEditorCommand.Text := '';
  end;
end;

procedure TPreferencesForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    { copy UI -> global variables }
    if RadioCodeEditorCommand.Checked then
      CodeEditor := EditCodeEditorCommand.Text
    else
      CodeEditor := '';
  end else
  begin
    { XxxCustomPath are special.
      They are updated all the time when you change them in UI,
      this makes functions like FindExeLazarusIDE work OK,
      and at the end we restore them to orignal values if user *did not*
      accept the changes by clicking "OK". }
    FpcCustomPath := OriginalFpcCustomPath;
    LazarusCustomPath := OriginalLazarusCustomPath;
  end;
end;

procedure TPreferencesForm.DirectoryEditFpcChange(Sender: TObject);
begin
  FpcCustomPath := DirectoryEditFpc.Directory;
  UpdateAutoDetectedLabels;
end;

procedure TPreferencesForm.DirectoryEditLazarusChange(Sender: TObject);
begin
  LazarusCustomPath := DirectoryEditLazarus.Directory;
  UpdateAutoDetectedLabels;
end;

procedure TPreferencesForm.UpdatePageVisible;
var
  SelectedPage: TPanel;
begin
  case ListPages.ItemIndex of
    0: SelectedPage := PanelFpcLazarusConfig;
    1: SelectedPage := PanelCodeEditor;
    else raise Exception.CreateFmt('Unexpected ListPages.ItemIndex %d', [ListPages.ItemIndex]);
  end;
  SetEnabledVisible(PanelFpcLazarusConfig, PanelFpcLazarusConfig = SelectedPage);
  SetEnabledVisible(PanelCodeEditor      , PanelCodeEditor       = SelectedPage);
end;

end.

