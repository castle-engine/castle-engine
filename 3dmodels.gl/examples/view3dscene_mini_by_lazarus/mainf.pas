unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, Menus, VRMLGLScene, MatrixNavigation, KambiGLControl,
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
    GLControl: TKamOpenGLControl;
    GroupBox1: TGroupBox;
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
    procedure FormDestroy(Sender: TObject);
    procedure EditGLControlFocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditGLControlFocusKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLControlGLContextClose(Sender: TObject);
    procedure GLControlGLContextInit(Sender: TObject);
    procedure MenuAboutOpenGLClick(Sender: TObject);
    procedure MenuFocusGLControlClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure GLControlPaint(Sender: TObject);
    procedure GLControlResize(Sender: TObject);
  private
    Scene: TVRMLGLScene;
    { This is always non-nil. }
    Navigator: TMatrixWalker;
    SceneFileName: string;
    procedure OpenScene(const FileName: string);
    procedure NavigatorChanged(ANavigator: TMatrixNavigator);
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
  OpenGLInformation;

procedure TMain.OpenScene(const FileName: string);
var
  CamPos, CamDir, CamUp, GravityUp: TVector3Single;
begin
  FreeAndNil(Scene);

  Scene := TVRMLGLScene.Create(
    LoadAsVRML(FileName, true),
    true, roSceneAsAWhole);

  { allow the scene to use it's own lights }
  Scene.Attributes.UseLights := true;
  Scene.Attributes.FirstGLFreeLight := 1;

  Scene.GetPerspectiveViewpoint(CamPos, CamDir, CamUp, GravityUp);

  Navigator.Init(
    CamPos,
    VectorAdjustToLength(CamDir,
      Box3dAvgSize(Scene.BoundingBox) * 0.01*0.4),
    CamUp, GravityUp,
    0.0, 0.0 { unused, we don't use Gravity here });

  GLControlResize(GLControl);
  GLControl.Invalidate;

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

procedure TMain.GLControlGLContextInit(Sender: TObject);
begin
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  GLControlResize(GLControl);
end;

procedure TMain.MenuAboutOpenGLClick(Sender: TObject);
begin
  TOpenGLInformation.Execute;
end;

procedure TMain.MenuFocusGLControlClick(Sender: TObject);
begin
  EditGLControlFocus.SetFocus;
end;

procedure TMain.GLControlGLContextClose(Sender: TObject);
begin
  if Scene <> nil then
  begin
    { Actually for now this would be done by FreeAndNil(Scene) too,
      but just to be clean I do it also here. }
    SCene.CloseGL;
  end;

  FreeAndNil(Scene);
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  Navigator := TMatrixWalker.Create(@NavigatorChanged);
  GLControl.Navigator := Navigator;

  UpdateCaption;

  MenuFocusGLControl.ShortCut := ShortCut(VK_Escape, []);

  if Parameters.High >= 1 then
    OpenScene(Parameters[1]);
end;

procedure TMain.FormDeactivate(Sender: TObject);
begin
  GLControl.ReleaseAllKeysAndMouse;
end;

procedure TMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  GLControl.NavigatorIdle;
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

  Navigator.CameraPos := Pos;
  Navigator.CameraDir := Dir;
  Navigator.CameraUp := Up;
end;

procedure TMain.EditGLControlFocusExit(Sender: TObject);
begin
  GLControl.ReleaseAllKeysAndMouse;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Navigator);
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
    GLControl.KeyDown(Key, Shift);
    Key := 0;
  end;
end;

procedure TMain.EditGLControlFocusKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> VK_TAB then
  begin
    GLControl.KeyUp(Key, Shift);
    Key := 0;
  end;
end;

procedure TMain.GLControlPaint(Sender: TObject);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(Navigator.Matrix);

  if Scene <> nil then
  begin
    Scene.Render(nil, tgAll);
  end;

  GLControl.SwapBuffers;
end;

procedure TMain.GLControlResize(Sender: TObject);

  procedure UpdateNavigatorProjectionMatrix;
  var
    ProjectionMatrix: TMatrix4f;
  begin
    glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
    Navigator.ProjectionMatrix := ProjectionMatrix;
  end;

begin
  if not GLControl.MakeCurrent then Exit;

  glViewport(0, 0, GLControl.Width, GLControl.Height);

  if Scene <> nil then
  begin
    ProjectionGLPerspective(45.0,
      GLControl.Width / GLControl.Height,
      Box3dMaxSize(Scene.BoundingBox) * 0.01,
      Box3dMaxSize(Scene.BoundingBox) * 20.0);
  end;

  UpdateNavigatorProjectionMatrix;
end;

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

