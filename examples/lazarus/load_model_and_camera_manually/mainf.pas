{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main form. }
unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  CastleControl, CastleCameras, CastleScene;

type
  TForm1 = class(TForm)
    Control1: TCastleControl;
    procedure FormCreate(Sender: TObject);
  private
    WalkCamera: TWalkCamera;
    Scene: TCastleScene;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses CastleVectors, CastleSceneCore;

{ Demo how to load 3D scene, and set camera.

  Note that you could also use directly Control1.Load,
  this would recreate both MainScene and Camera (setting camera
  most suitable for the scene, following it's NavigationInfo and Viewpoint
  nodes). }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Scene := TCastleScene.Create(Self);
  Scene.Load('../../3d_rendering_processing/data/bridge_final.x3dv');
  Scene.Spatial := [ssRendering, ssDynamicCollisions]; // if you want collisions, and faster rendering
  Scene.ProcessEvents := true; // if you use VRML/X3D events

  Control1.SceneManager.MainScene := Scene;
  Control1.SceneManager.Items.Add(Scene);

  WalkCamera := Control1.SceneManager.WalkCamera;
  WalkCamera.Init(Vector3(0, 0, 0), Vector3(1, 0, 0),
    Vector3(0, 1, 0), Vector3(0, 1, 0), 1, 0.1);
  WalkCamera.MoveSpeed := 10.0; // default is 1
  WalkCamera.Gravity := true; // if you want gravity, of course
  Control1.SceneManager.Camera := WalkCamera;
end;

end.
