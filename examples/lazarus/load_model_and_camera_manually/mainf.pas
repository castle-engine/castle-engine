unit mainf; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  CastleGLControl, Cameras, VRMLGLScene;

type
  TForm1 = class(TForm)
    KamVRMLBrowser1: TCastleControl;
    WalkCamera1: TWalkCamera;
    VRMLGLScene1: T3DScene;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses VectorMath, VRMLScene;

{ Demo how to load 3D scene, and camera, manually, adjusting all
  relevant properties. Some of the properties below could be also 
  set in the Object Inspector, showing them here just for clarity.
  
  Note that you could also use directly KamVRMLBrowser1.Load,
  this would recreate both MainScene and Camera (setting camera
  most suitable for the scene, following it's NavigationInfo and Viewpoint
  nodes). }

procedure TForm1.FormCreate(Sender: TObject);
begin
  VRMLGLScene1.Load('../../vrml/models/bridge_final.x3dv');
  VRMLGLScene1.Spatial := [ssRendering, ssDynamicCollisions]; // if you want collisions, and faster rendering
  VRMLGLScene1.ProcessEvents := true; // if you use VRML/X3D events

  KamVRMLBrowser1.SceneManager.MainScene := VRMLGLScene1;
  KamVRMLBrowser1.SceneManager.Items.Add(VRMLGLScene1);

  WalkCamera1.Init(Vector3Single(0, 0, 0), Vector3Single(1, 0, 0),
    Vector3Single(0, 1, 0), Vector3Single(0, 1, 0), 1, 0.1);
  WalkCamera1.MoveSpeed := 10.0; // default is 1
  WalkCamera1.Gravity := true; // if you want gravity, of course
  KamVRMLBrowser1.SceneManager.Camera := WalkCamera1;
end;

end.

