unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  KambiGLControl, Cameras;

type
  TMainForm = class(TForm)
    GLControl: TKamOpenGLControl;
    Camera: TExamineCamera;
    procedure GLControlDraw(Sender: TObject);
    procedure GLControlResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLControlGLContextInit(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses VectorMath, Boxes3D, GL, GLU, KambiGLUtils;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Camera.Init(Box3d(
    Vector3Single(-1, -1, -1),
    Vector3Single( 1,  1,  1)));
end;

procedure TMainForm.GLControlGLContextInit(Sender: TObject);
begin
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_DEPTH_TEST);
end;

procedure TMainForm.GLControlResize(Sender: TObject);
begin
  glViewport(0, 0, GLControl.Width, GLControl.Height);
  ProjectionGLPerspective(45.0, GLControl.Width / GLControl.Height, 0.1, 100);
end;

procedure TMainForm.GLControlDraw(Sender: TObject);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(Camera.Matrix);

  DrawGLBox(-1, -1, -1, 1, 1, 1, 0, 0, 0, true, false);
end;

initialization
  {$I mainf.lrs}

end.

