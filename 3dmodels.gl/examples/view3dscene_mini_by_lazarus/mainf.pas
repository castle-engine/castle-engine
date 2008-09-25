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
    ButtonChangeCamera: TButton;
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
    procedure BrowserNavigatorChanged(Navigator: TMatrixNavigator);
    procedure ButtonChangeCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
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

procedure TMain.UpdateCaption;
var
  S: string;
begin
  if SceneFileName <> '' then
    S := ExtractFileName(SceneFileName) else
    S := 'No Scene';
  S += ' - lazarus_vrml_browser';
  Caption := S;
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
  Browser.FocusableControl.SetFocus;
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

  { Length of direction vector affects speed.
    For simplicity, we don't allow user to change this here
    (although keys +/- do this in Walk mode), we keep previous CameraDir
    length. }
  VectorAdjustToLengthTo1st(Dir, VectorLen(Browser.NavWalker.CameraDir));

  { First convert all to float. Then set Navigator properties.
    This way in case of exception in StrToFloat, previous
    Navigator properties remain OK. }

  Browser.NavWalker.CameraPos := Pos;
  Browser.NavWalker.CameraDir := Dir;
  Browser.NavWalker.CameraUp := Up;
end;

procedure TMain.BrowserNavigatorChanged(Navigator: TMatrixNavigator);
var
  Dir: TVector3Single;
begin
  if Navigator is TMatrixWalker then
  begin
    { Browser.NavWalker is now the same thing as (Navigator as TMatrixWalker) }
    EditPositionX.Text := FloatToNiceStr(Browser.NavWalker.CameraPos[0]);
    EditPositionY.Text := FloatToNiceStr(Browser.NavWalker.CameraPos[1]);
    EditPositionZ.Text := FloatToNiceStr(Browser.NavWalker.CameraPos[2]);

    { Length of direction vector affects speed.
      For simplicity, we don't show it to user here (it could have small
      values, and would look like all "0.00" while in fact being non-zero).
      Instead. we show the normalized dir. }
    Dir := Normalized(Browser.NavWalker.CameraDir);

    EditDirectionX.Text := FloatToNiceStr(Dir[0]);
    EditDirectionY.Text := FloatToNiceStr(Dir[1]);
    EditDirectionZ.Text := FloatToNiceStr(Dir[2]);

    EditUpX.Text := FloatToNiceStr(Browser.NavWalker.CameraUp[0]);
    EditUpY.Text := FloatToNiceStr(Browser.NavWalker.CameraUp[1]);
    EditUpZ.Text := FloatToNiceStr(Browser.NavWalker.CameraUp[2]);
  end;
end;

initialization
  {$I mainf.lrs}
end.

