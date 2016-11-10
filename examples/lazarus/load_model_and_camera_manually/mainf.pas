unit mainf; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  CastleControl, CastleCameras, CastleScene;

type
  TForm1 = class(TForm)
    Control1: TCastleControl;
    WalkCamera1: TWalkCamera;
    Scene1: TCastleScene;
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

uses CastleVectors, CastleSceneCore;

{ Demo how to load 3D scene, and camera, manually, adjusting all
  relevant properties. Some of the properties below could be also 
  set in the Object Inspector, showing them here just for clarity.
  
  Note that you could also use directly Control1.Load,
  this would recreate both MainScene and Camera (setting camera
  most suitable for the scene, following it's NavigationInfo and Viewpoint
  nodes). }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Scene1.Load('../../3d_rendering_processing/data/bridge_final.x3dv');
  Scene1.Spatial := [ssRendering, ssDynamicCollisions]; // if you want collisions, and faster rendering
  Scene1.ProcessEvents := true; // if you use VRML/X3D events

  Control1.SceneManager.MainScene := Scene1;
  Control1.SceneManager.Items.Add(Scene1);

  WalkCamera1.Init(Vector3Single(0, 0, 0), Vector3Single(1, 0, 0),
    Vector3Single(0, 1, 0), Vector3Single(0, 1, 0), 1, 0.1);
  WalkCamera1.MoveSpeed := 10.0; // default is 1
  WalkCamera1.Gravity := true; // if you want gravity, of course
  Control1.SceneManager.Camera := WalkCamera1;
end;

end.

