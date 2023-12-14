{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo how to load and render 3D model, with some additional tricks.
  This is an extended version of view_3d_model_simple.lpr,
  so look there first. }
program view_3d_model_advanced;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils, Classes,
  CastleUtils, CastleWindow, CastleUIControls,
  CastleSceneCore, CastleLog, CastleParameters, CastleScene, X3DLoad,
  CastleControls, CastleUriUtils, CastleApplicationProperties, CastleViewport,
  CastleCameras;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
  OpenButton: TCastleButton;
  Url: String =
    //'castle-data:/bridge_final.x3dv';
    'castle-data:/car.gltf';

type
  TEventHandler = class(TComponent)
    procedure OpenButtonClick(Sender: TObject);
  end;

procedure TEventHandler.OpenButtonClick(Sender: TObject);
begin
  if Window.FileDialog('Open Scene', Url, true, LoadScene_FileFilters) then
  begin
    Scene.Load(Url);
    Scene.PlayAnimation('animation', true); // play animation named "animation", if exists.

    { Move camera to most suitable place for the *new* scene (ignoring previous camera
      position). }
    Viewport.AssignDefaultCamera;
  end;
end;

var
  EventHandler: TEventHandler;
begin
  { You can specify initial 3D model URL by command-line parameter. }
  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    Url := Parameters[1];

  InitializeLog;

  Window := TCastleWindow.Create(Application);

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
  Window.Controls.InsertFront(Viewport);

  Window.Open;

  { load a Scene and add it to Viewport, just like view_3d_model_simple }
  Scene := TCastleScene.Create(Application);
  Scene.Load(Url);
  // play animation named "wheels_turning", if exists.
  Scene.PlayAnimation('wheels_turning', true);

  Scene.PreciseCollisions := true;
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  { Output some information about the loaded scene }
  WritelnLog('Scene vertexes: %d, triangles: %d, bounding box: %s', [
    Scene.VerticesCount,
    Scene.TrianglesCount,
    Scene.BoundingBox.ToString
  ]);

  EventHandler := TEventHandler.Create(Application);

  { add an "Open" button to the window controls }
  OpenButton := TCastleButton.Create(Application);
  OpenButton.Caption := 'Open Scene';
  OpenButton.OnClick := {$ifdef FPC}@{$endif} EventHandler.OpenButtonClick;
  OpenButton.Anchor(hpLeft, 10);
  OpenButton.Anchor(vpBottom, 10);
  OpenButton.AutoSize := false;
  OpenButton.Width := 250;
  OpenButton.Height := 75;
  Window.Controls.InsertFront(OpenButton);

  Application.Run;
end.
