{
  Copyright 2019-2021 Michalis Kamburelis.

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
  ExtCtrls, ButtonPanel, ComCtrls;

type
  TPreferencesForm = class(TForm)
    ButtonRegisterLazarusPackages: TButton;
    ButtonPanel1: TButtonPanel;
    CheckBoxMuteOnRun: TCheckBox;
    DirectoryEditFpc: TDirectoryEdit;
    DirectoryEditLazarus: TDirectoryEdit;
    EditCodeEditorCommand: TFileNameEdit;
    EditCodeEditorCommandProject: TFileNameEdit;
    LabelInstructions0: TLabel;
    LabelInstructions1: TLabel;
    LabelInstructions2: TLabel;
    LabelLazarusWebsite: TLabel;
    LabelVolume: TLabel;
    LabelCodeEditorCommandInstructions: TLabel;
    LabelCodeEditorCommand: TLabel;
    LabelCodeEditorCommandProjectInstructions: TLabel;
    LabelCodeEditorHeader: TLabel;
    LabelSound: TLabel;
    LabelFpc: TLabel;
    LabelFpcAutoDetectedCaption: TLabel;
    LabelLazarus: TLabel;
    LabelLazarusAutoDetectedCaption: TLabel;
    LabelTitle: TLabel;
    LabelFpcAutoDetected: TLabel;
    LabelLazarusAutoDetected: TLabel;
    ListPages: TListBox;
    PanelInstructions: TPanel;
    PanelCodeEditor: TPanel;
    PanelSound: TPanel;
    PanelFpcLazarusConfig: TPanel;
    RadioCodeEditorLazarus: TRadioButton;
    RadioCodeEditorCustom: TRadioButton;
    TrackVolume: TTrackBar;
    procedure ButtonRegisterLazarusPackagesClick(Sender: TObject);
    procedure DirectoryEditFpcChange(Sender: TObject);
    procedure DirectoryEditLazarusChange(Sender: TObject);
    procedure EditCodeEditorCommandAcceptFileName(Sender: TObject;
      var Value: String);
    procedure EditCodeEditorCommandProjectAcceptFileName(Sender: TObject;
      var Value: String);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure LabelLazarusWebsiteClick(Sender: TObject);
    procedure ListPagesClick(Sender: TObject);
    procedure RadioCodeEditorAnyChange(Sender: TObject);
    procedure TrackVolumeChange(Sender: TObject);
  private
    OriginalFpcCustomPath, OriginalLazarusCustomPath: String;
    procedure UpdateAutoDetectedLabels;
    procedure UpdatePageVisible;
  public

  end;

var
  PreferencesForm: TPreferencesForm;

implementation

uses CastleOpenDocument, CastleUtils, CastleLog, CastleSoundEngine,
  ToolCompilerInfo, ToolFpcVersion, ToolCommonUtils,
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

procedure TPreferencesForm.RadioCodeEditorAnyChange(Sender: TObject);
begin
  EditCodeEditorCommand.Enabled := RadioCodeEditorCustom.Checked;
  EditCodeEditorCommandProject.Enabled := RadioCodeEditorCustom.Checked;
end;

procedure TPreferencesForm.TrackVolumeChange(Sender: TObject);
begin
  { While the volume will be adjusted in TPreferencesForm.FormClose too,
    along with EditorVolume, but it is natural to also modify the audible
    volume immediately. }
  SoundEngineSetVolume(TrackVolume.Position / TrackVolume.Max);
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

  { Update Enabled of edit fields }
  RadioCodeEditorAnyChange(nil);

  DirectoryEditFpc.Directory := FpcCustomPath;
  DirectoryEditLazarus.Directory := LazarusCustomPath;
  { We will change the global Fpc/LazarusCustomPath during this dialog,
    so allow to revert them on "Cancel". }
  OriginalFpcCustomPath := FpcCustomPath;
  OriginalLazarusCustomPath := LazarusCustomPath;

  // Note that making any RadioCodeEditorXxx checked will uncheck the others
  case CodeEditor of
    ceCustom: RadioCodeEditorCustom.Checked := true;
    ceLazarus: RadioCodeEditorLazarus.Checked := true;
    else raise EInternalError.Create('CodeEditor?');
  end;
  EditCodeEditorCommand.Text := CodeEditorCommand;
  EditCodeEditorCommandProject.Text := CodeEditorCommandProject;

  // sound tab
  TrackVolume.Position := Round(EditorVolume * TrackVolume.Max);
  CheckBoxMuteOnRun.Checked := MuteOnRun;
end;

procedure TPreferencesForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    { copy UI -> global variables }

    // code editor tab
    if RadioCodeEditorCustom.Checked then
      CodeEditor := ceCustom
    else
      CodeEditor := ceLazarus;
    CodeEditorCommand := EditCodeEditorCommand.Text;
    CodeEditorCommandProject := EditCodeEditorCommandProject.Text;

    // sound tab
    EditorVolume := TrackVolume.Position / TrackVolume.Max;
    MuteOnRun := CheckBoxMuteOnRun.Checked;
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

  { Set SoundEngine.Volume regardless if we accepted
    (so MuteOnRun, EditorVolume changed) or not (so they are unchanged)
    to revert any immediate changes to volume in TPreferencesForm.TrackVolumeChange. }
  SoundEngineSetVolume;
end;

procedure TPreferencesForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOK then
  begin
    if RadioCodeEditorCustom.Checked and
       (Trim(EditCodeEditorCommand.Text) = '') then
    begin
      ErrorBox('You must specify some custom editor command, or switch to use "Lazarus" as code editor');
      CanClose := false;
      Exit;
    end;
  end;
end;

procedure TPreferencesForm.DirectoryEditFpcChange(Sender: TObject);
begin
  FpcCustomPath := DirectoryEditFpc.Directory;
  UpdateAutoDetectedLabels;
end;

procedure TPreferencesForm.ButtonRegisterLazarusPackagesClick(Sender: TObject);
var
  ExecutionLog: String;

  procedure RegisterPackage(const Name: String);
  var
    LazbuildExe, LazbuildOutput, PackageFileName, CommandLog: String;
    LazbuildExitStatus: integer;
  begin
    LazbuildExe := FindExeLazarus('lazbuild');
    if LazbuildExe = '' then
      raise EExecutableNotFound.Create('Cannot find "lazbuild" program. Make sure it is installed, and set Lazarus location in CGE editor "Preferences".');

    PackageFileName := CastleEnginePath + 'packages' + PathDelim + Name + '.lpk';

    MyRunCommandIndir(
      GetCurrentDir { no better directory, but also should not matter },
      LazbuildExe, [
        '--add-package-link',
        PackageFileName
      ], LazbuildOutput, LazbuildExitStatus);

    CommandLog := 'lazbuild --add-package-link "' + PackageFileName + '"';
    ExecutionLog := ExecutionLog + NL + NL + CommandLog;
    WritelnLog('lazbuild status %d, output:' + NL + '%s', [LazbuildExitStatus, LazbuildOutput]);

    if (LazbuildExitStatus <> 0) or
       (Pos('Invalid option', LazbuildOutput) <> 0) { lazbuild has exit status 0 in this case } then
      raise Exception.Create('Executing lazbuild failed:' + NL + NL + LazbuildOutput);
  end;

begin
  ExecutionLog := 'Lazarus packages registed successfully.' + NL + NL +
    'Executed the following commands:';
  try
    RegisterPackage('castle_base');
    RegisterPackage('castle_window');
    RegisterPackage('castle_components');
    RegisterPackage('alternative_castle_window_based_on_lcl');
    RegisterPackage('castle_indy');
    ShowMessage(ExecutionLog);
  except
    on E: Exception do
      ErrorBox(E.Message);
  end;
end;

procedure TPreferencesForm.DirectoryEditLazarusChange(Sender: TObject);
begin
  LazarusCustomPath := DirectoryEditLazarus.Directory;
  UpdateAutoDetectedLabels;
end;

procedure TPreferencesForm.EditCodeEditorCommandAcceptFileName(Sender: TObject;
  var Value: String);
begin
  // auto-add ${PAS} macro and propose quoting
  Value := '"' + Value + '" ${PAS}';
end;

procedure TPreferencesForm.EditCodeEditorCommandProjectAcceptFileName(
  Sender: TObject; var Value: String);
begin
  // auto-add ${PROJECT_DIR} macro and propose quoting
  Value := '"' + Value + '" ${PROJECT_DIR}';
end;

procedure TPreferencesForm.UpdatePageVisible;
var
  SelectedPage: TPanel;
begin
  case ListPages.ItemIndex of
    0: SelectedPage := PanelFpcLazarusConfig;
    1: SelectedPage := PanelCodeEditor;
    2: SelectedPage := PanelSound;
    else raise Exception.CreateFmt('Unexpected ListPages.ItemIndex %d', [ListPages.ItemIndex]);
  end;
  SetEnabledVisible(PanelFpcLazarusConfig, PanelFpcLazarusConfig = SelectedPage);
  SetEnabledVisible(PanelCodeEditor      , PanelCodeEditor       = SelectedPage);
  SetEnabledVisible(PanelSound           , PanelSound            = SelectedPage);
end;

end.
