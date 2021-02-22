unit FormImportAtlas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Spin, ExtCtrls, CastleInternalSpriteSheet;

type
  TImportAtlasForm = class(TForm)
    ButtonImport: TButton;
    chbxImportByColumns: TCheckBox;
    EditAnimationName: TEdit;
    EditAtlasURL: TEdit;
    LabelCols: TLabel;
    LabelAnimationName: TLabel;
    LabelAtlasNameTitle: TLabel;
    LabelRows: TLabel;
    PanelAnimationName: TPanel;
    PanelAtlasURL: TPanel;
    SpinEditCols: TSpinEdit;
    SpinEditRows: TSpinEdit;
    procedure ButtonImportClick(Sender: TObject);
  private
    FAtlasURL: String;
    FSpriteSheet: TCastleSpriteSheet;
  public
    procedure Initialize(const SpriteSheet: TCastleSpriteSheet; AtlasURL: String);
  end;

var
  ImportAtlasForm: TImportAtlasForm;

implementation

{$R *.lfm}

uses EditorUtils;

{ TImportAtlasForm }

procedure TImportAtlasForm.ButtonImportClick(Sender: TObject);
var
  Animation: TCastleSpriteSheetAnimation;
begin
  try
    if FSpriteSheet.HasAnimation(EditAnimationName.Text) then
      Animation := FSpriteSheet.AnimationByName(EditAnimationName.Text)
    else
      Animation := FSpriteSheet.AddAnimation(EditAnimationName.Text);

    Animation.ImportAtlas(EditAtlasURL.Text, SpinEditCols.Value,
      SpinEditRows.Value, chbxImportByColumns.Checked);
    ModalResult := mrOK;
  except
    on E:Exception do
    begin
      ErrorBox(E.Message);
    end;
  end;
end;

procedure TImportAtlasForm.Initialize(const SpriteSheet: TCastleSpriteSheet;
  AtlasURL: String);
begin
  FSpriteSheet := SpriteSheet;
  FAtlasURL := AtlasURL;
  EditAnimationName.Text := FSpriteSheet.ProposeAnimationName;
  EditAtlasURL.Text := AtlasURL;
end;

end.

