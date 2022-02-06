{
  Copyright 2014-2022 Michalis Kamburelis.

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
  CastleControl, CastleCameras, CastleScene, CastleViewport;

type
  TForm1 = class(TForm)
    Control1: TCastleControl;
    procedure FormCreate(Sender: TObject);
  private
    Viewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;
    Scene: TCastleScene;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses CastleVectors, CastleSceneCore;

{ Simple demo how to load 3D scene, and set camera and navigation explicitly.

  Note: we do this by code, just for demo.
  The setup here could be also just designed in CGE editor,
  and loaded by TCastleControl.DesignUrl -- this is generally better than doing it by code,
  as it means you can visually see what you're doing at development.
}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Viewport := TCastleViewport.Create(Self);
  Viewport.FullSize := true;
  Control1.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Self);
  Scene.Load('castle-data:/bridge_level/bridge_final.x3dv');
  Scene.Spatial := [ssRendering, ssDynamicCollisions]; // if you want collisions, and faster rendering
  Scene.ProcessEvents := true; // if you use VRML/X3D events

  Viewport.Items.MainScene := Scene;
  Viewport.Items.Add(Scene);

  WalkNavigation := TCastleWalkNavigation.Create(Self);
  WalkNavigation.PreferredHeight := 2;
  WalkNavigation.Radius := 0.1;
  WalkNavigation.MoveSpeed := 10.0; // default is 1
  WalkNavigation.Gravity := true;
  Viewport.Navigation := WalkNavigation;

  Viewport.Camera.Init(
    Vector3(0, 0, 0), // position
    Vector3(1, 0, 0), // direction
    Vector3(0, 1, 0), // up
    Vector3(0, 1, 0)); // gravity up
  Viewport.Camera.ProjectionNear := WalkNavigation.Radius / 2;
end;

end.
