{ This is a demo implementation of something like "mini-view3dscene"
  using Lazarus.

  The intention is to make development of
  view3dscene much easier (by using all LCL visual things,
  and designing UI visually).

  The actual rendering code is adapted mostly from simpleViewModel
  and simpleViewModel_2 example programs (I didn't want to start
  with adapting a whole view3dscene code).
  But, unlike in simpleViewModel, user
  is able to open new scene file while the program is running
  (just like in view3dscene).

  Yes, eventually my intention is to really change view3dscene
  into a normal Lazarus program. But some code will have
  to be written for this to really happen,
  also GTK 2 support should be improved in Lazarus,
  also I'll also want to make sure that
  TOpenGLControl is as stable as my GLWindow unit.
  This program is a start of such effort.

  TODO: for now user has no way to actually change Navigator
  properties. This means that even though we use Navigator class,
  user gets to see only the initial camera view of the model. }

unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, Menus, VRMLFlatSceneGL, MatrixNavigation;

type

  { TMain }

  TMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuQuit: TMenuItem;
    MenuSep1: TMenuItem;
    MenuOpen: TMenuItem;
    GLControl: TOpenGLControl;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure GLControlPaint(Sender: TObject);
    procedure GLControlResize(Sender: TObject);
  private
    Scene: TVRMLFlatSceneGL;
    { This is always non-nil. }
    Navigator: TMatrixWalker;
    procedure PostRedisplay(ANavigator: TMatrixNavigator);
  public
    { public declarations }
  end; 

var
  Main: TMain;

implementation

uses VectorMath, Boxes3d, VRMLNodes, VRMLOpenGLRenderer, OpenGLh,
  KambiClassUtils, KambiUtils, Object3dAsVRML,
  KambiGLUtils, VRMLFlatScene,
  KambiFilesUtils;

procedure TMain.MenuOpenClick(Sender: TObject);
var
  CamPos, CamDir, CamUp: TVector3Single;
begin
  if OpenDialog1.Execute then
  begin
    FreeAndNil(Scene);
  
    Scene := TVRMLFlatSceneGL.Create(
      LoadAsVRML(OpenDialog1.FileName, true),
      true, roSceneAsAWhole);

    { allow the scene to use it's own lights }
    Scene.Attributes.UseLights := true;
    Scene.Attributes.FirstGLFreeLight := 1;

    Scene.GetPerspectiveCamera(CamPos, CamDir, CamUp);
  
    Navigator.Init(
      CamPos,
      VectorAdjustToLength(CamDir,
        Box3dAvgSize(Scene.BoundingBox) * 0.01*0.4),
      CamUp,
      0.0, 0.0 { unused, we don't use Gravity here });

    GLControl.Resize;
    PostRedisplay(Navigator);
  end;
end;

procedure TMain.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.FormShow(Sender: TObject);
begin
  { TODO: is this a proper place to initialize things tied to
    OpenGL context ? }

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
end;

procedure TMain.FormHide(Sender: TObject);
begin
  { TODO: is this a proper place to finalize things tied to
    OpenGL context ? }

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
  Navigator := TMatrixWalker.Create(@PostRedisplay);
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Navigator);
end;

procedure TMain.GLControlPaint(Sender: TObject);
begin
  { TODO: seems that FormShow is not good --- it's never called,
    at least during usual program startup ? }
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(Navigator.Matrix);
  
  if Scene <> nil then
  begin
    Scene.Render(nil);
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
  glViewport(0, 0, GLControl.Width, GLControl.Height);
  
  if Scene <> nil then
  begin
    ProjectionGLPerspective(45.0,
      GLControl.Width / GLControl.Height,
      Box3dMaxSize(Scene.BoundingBox) * 0.05,
      Box3dMaxSize(Scene.BoundingBox) * 3.0);
  end;
  
  UpdateNavigatorProjectionMatrix;
end;

procedure TMain.PostRedisplay(ANavigator: TMatrixNavigator);
begin
  GLControl.Invalidate;
end;
 
initialization
  {$I mainf.lrs}
end.

