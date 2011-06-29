unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  KambiGLControl, Cameras, KambiSceneManager;

type
  TMainForm = class(TForm)
    GLControl: TKamOpenGLControl;
    Camera: TExamineCamera;
    SceneManager: TKamSceneManager;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses VectorMath, Boxes3D, GL, KambiGLUtils, Base3D, Frustum;

type
  TCube = class(T3D)
  public
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    function BoundingBox: TBox3D; override;
  end;

procedure TCube.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  if Params.TransparentGroup in [tgAll, tgOpaque] then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      glEnable(GL_DEPTH_TEST);
      DrawGLBox(-1, -1, -1, 1, 1, 1, 0, 0, 0, true, false);
    glPopAttrib;
  end;
end;

function TCube.BoundingBox: TBox3D;
begin
  Result := Box3D(
    Vector3Single(-1, -1, -1),
    Vector3Single( 1,  1,  1));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  GLControl.Controls.Add(SceneManager);

  SceneManager.Items.Add(TCube.Create(Self));

  Camera.Init(Box3D(
    Vector3Single(-1, -1, -1),
    Vector3Single( 1,  1,  1)), 0.1);
end;

initialization
  {$I mainf.lrs}

end.

