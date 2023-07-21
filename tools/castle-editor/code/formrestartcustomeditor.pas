unit FormRestartCustomEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  Buttons;

type
  TRestartCustomEditorForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    { Note: Keep this TButton, not TBitBtn.
      On GTK2, TBitBtn is not painted correctly, is underneath TButtonPanel. }
    ButtonRunLastEditor: TButton;
    LabelCaption: TLabel;
  private

  public
    procedure Initialize(const ProjectName, ProjectPath: String);
  end;

var
  RestartCustomEditorForm: TRestartCustomEditorForm;

implementation

uses CastleUtils,
  EditorUtils;

{$R *.lfm}

{ TRestartCustomEditorForm }

procedure TRestartCustomEditorForm.Initialize(
  const ProjectName, ProjectPath: String);
var
  CustomEditorFileName: String;
  CustomEditorExists: Boolean;
const
  ButtonsMargin = 16;
begin
  LabelCaption.Caption := Format('Project "%s" uses custom components.' + NL +
    NL +
    'Rebuild and restart editor with custom components?', [ProjectName]);

  CustomEditorFileName := ProjectPath + 'castle-engine-output' + PathDelim +
    'editor' + PathDelim + 'castle-editor' + ExeExtension;
  CustomEditorExists := FileExists(CustomEditorFileName);
  ButtonRunLastEditor.Enabled := CustomEditorExists;
  if CustomEditorExists then
    ButtonRunLastEditor.Caption := 'Run Last Build Editor (build: ' + FileDateTimeStr(CustomEditorFileName) + ')'
  else
    ButtonRunLastEditor.Caption := 'Run Last Build Editor (not found)';

  ClientHeight := Round(
    (LabelCaption.Top + LabelCaption.Height + ButtonsMargin + ButtonPanel1.Height) *
    PixelsPerInch / DesignTimePPI
  );

  // focus OK on start
  ActiveControl := ButtonPanel1.OKButton;
end;

end.

