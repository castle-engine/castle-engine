unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, Menus, VRMLGLScene, MatrixNavigation, KambiVRMLBrowser,
  Buttons, ExtCtrls, StdCtrls;

type

  { TMain }

  TMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ButtonChangeCamera: TButton;
    EditGLControlFocus: TEdit;
    EditPositionX: TEdit;
    EditPositionY: TEdit;
    EditPositionZ: TEdit;
    EditDirectionZ: TEdit;
    EditDirectionY: TEdit;
    EditDirectionX: TEdit;
    EditUpZ: TEdit;
    EditUpY: TEdit;
    EditUpX: TEdit;
    GroupBox1: TGroupBox;
    Browser: TKamVRMLBrowser;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuFocusGLControl: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAboutOpenGL: TMenuItem;
    MenuItemView: TMenuItem;
    MenuQuit: TMenuItem;
    MenuSep1: TMenuItem;
    MenuOpen: TMenuItem;
    OpenDialog1: TOpenDialog;
    PanelBottom: TPanel;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonChangeCameraClick(Sender: TObject);
    procedure EditGLControlFocusExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure EditGLControlFocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditGLControlFocusKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuAboutOpenGLClick(Sender: TObject);
    procedure MenuFocusGLControlClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
  private
    SceneFileName: string;
    procedure OpenScene(const FileName: string);
    procedure UpdateCaption;
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

uses LCLType, VectorMath, Boxes3d, VRMLNodes, VRMLOpenGLRenderer,
  GL, GLU, GLExt, KambiClassUtils, KambiUtils, Object3dAsVRML,
  KambiGLUtils, VRMLScene, KambiFilesUtils,
  OpenGLInformation, KambiLCLUtils;

procedure TMain.OpenScene(const FileName: string);
begin
  Browser.Load(FileName);
  Browser.Scene.ProcessEvents := true;

  Browser.Resize;
  Browser.Invalidate;

  SceneFileName := FileName;
  UpdateCaption;
end;

procedure TMain.MenuOpenClick(Sender: TObject);
begin
  if SceneFileName <> '' then
    OpenDialog1.FileName := SceneFileName;
  if OpenDialog1.Execute then
    OpenScene(OpenDialog1.FileName);
end;

procedure TMain.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.MenuAboutOpenGLClick(Sender: TObject);
begin
  TOpenGLInformation.Execute;
end;

procedure TMain.MenuFocusGLControlClick(Sender: TObject);
begin
  EditGLControlFocus.SetFocus;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FileFiltersToOpenDialog(LoadAsVRML_FileFilters, OpenDialog1);

  UpdateCaption;

  MenuFocusGLControl.ShortCut := ShortCut(VK_Escape, []);

  if Parameters.High >= 1 then
    OpenScene(Parameters[1]);
end;

procedure TMain.FormDeactivate(Sender: TObject);
begin
  Browser.ReleaseAllKeysAndMouse;
end;

procedure TMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  Browser.Idle;
  Done := false;
end;

procedure TMain.ButtonChangeCameraClick(Sender: TObject);
var
  Pos, Dir, Up: TVector3Single;
begin
  Pos := Vector3Single(
    StrToFloat(EditPositionX.Text),
    StrToFloat(EditPositionY.Text),
    StrToFloat(EditPositionZ.Text));

  Dir := Vector3Single(
    StrToFloat(EditDirectionX.Text),
    StrToFloat(EditDirectionY.Text),
    StrToFloat(EditDirectionZ.Text));

  Up := Vector3Single(
    StrToFloat(EditUpX.Text),
    StrToFloat(EditUpY.Text),
    StrToFloat(EditUpZ.Text));

  { First convert all to float. Then set Navigator properties.
    This way in case of exception in StrToFloat all remains OK. }

  Browser.NavWalker.CameraPos := Pos;
  Browser.NavWalker.CameraDir := Dir;
  Browser.NavWalker.CameraUp := Up;
end;

procedure TMain.EditGLControlFocusExit(Sender: TObject);
begin
  Browser.ReleaseAllKeysAndMouse;
end;

procedure TMain.EditGLControlFocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { EditGLControlFocus is an ugly hack.

    The problem:
    TOpenGLControl cannot catch focus,so when I placed some
    focusable controls (edit boxes, buttons)
    on the form, it was not possible to pass key presses to GLControl.

    Poor workaround:
    Make form KeyPreview and pass OnKeyDown/Up from our Form
    to GLControl. This is poor workaround, because it makes funny
    effects when user tries to operate on edit boxes with arrows:
    both 3d view changes and the cursor inside edit box moves.

    Better workaround:
    Create EditGLControlFocus that can have focus, but is not visible
    --- so I set it's size to minimum (1, 1 in Lazarus)
    (I can't set Visible to false, then it would not be focusable).
    Then the only purpose of EditGLControlFocus is to call
    appropriate GLControl events. }

  { I must avoid catching tab, to allow user to switch between controls. }
  if Key <> VK_TAB then
  begin
    Browser.KeyDown(Key, Shift);
    Key := 0;
  end;
end;

procedure TMain.EditGLControlFocusKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> VK_TAB then
  begin
    Browser.KeyUp(Key, Shift);
    Key := 0;
  end;
end;

{TODO:
procedure TMain.NavigatorChanged(ANavigator: TMatrixNavigator);
begin
  GLControl.Invalidate;

  EditPositionX.Text := FloatToNiceStr(Navigator.CameraPos[0]);
  EditPositionY.Text := FloatToNiceStr(Navigator.CameraPos[1]);
  EditPositionZ.Text := FloatToNiceStr(Navigator.CameraPos[2]);

  EditDirectionX.Text := FloatToNiceStr(Navigator.CameraDir[0]);
  EditDirectionY.Text := FloatToNiceStr(Navigator.CameraDir[1]);
  EditDirectionZ.Text := FloatToNiceStr(Navigator.CameraDir[2]);

  EditUpX.Text := FloatToNiceStr(Navigator.CameraUp[0]);
  EditUpY.Text := FloatToNiceStr(Navigator.CameraUp[1]);
  EditUpZ.Text := FloatToNiceStr(Navigator.CameraUp[2]);
end;
}

procedure TMain.UpdateCaption;
var
  S: string;
begin
  if SceneFileName <> '' then
    S := ExtractFileName(SceneFileName) else
    S := 'No Scene';
  S += ' - view3dscene_mini_by_lazarus';
  Caption := S;
end;

initialization
  {$I mainf.lrs}
end.

