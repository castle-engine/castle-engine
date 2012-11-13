{
  Copyright 2009-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Play the animations of resources (creatures/items). }

uses SysUtils, CastleFilesUtils, CastleWindow, CastleResources, CastleScene,
  CastleProgress, ProgressUnit;
var
  BaseScene: TCastleScene;
  Window: TCastleWindow;
  Resource: T3DResource;
  Animation: T3DResourceAnimation;
  Time: Single;
begin
  Window := TCastleWindow.Create(Application);
  WindowProgressInterface.Window := Window;
  Progress.UserInterface := WindowProgressInterface;

  Resources.LoadFromFiles;
  Resource := Resources.FindName('KnightKAnim');
  { Directly get the animation. In a normal game, you would never get an animation
    like this --- instead resource logic (like TWalkAttackCreature)
    would always choose appropriate animation, based on creature/item state,
    and would choose if it's looping. }
  Animation := Resource.Animations.FindName('walk');
  Time := 0.0;

  { load basic 3D scene where creature is shown. This isn't necessary,
    but it's an easy way to add a camera with headlight,
    and some grid to help with orientation. }
  BaseScene := TCastleScene.Create(Application);
  BaseScene.Load(ProgramDataPath + 'data' + PathDelim + 'base.x3d');
  { turn on headlight, as base.x3d exported from Blender has always headlight=false }
  BaseScene.NavigationInfoStack.Top.FdHeadlight.Send(true);
  Window.SceneManager.MainScene := BaseScene;
  Window.SceneManager.Items.Add(BaseScene);

  { Open the window, as OpenGL context must be ready before Resources.Prepare. }
  Window.Open;

  { Prepare (load animations) for all resources.
    In a normal game, you would not call this directly, instead you would
    depend on TGameSceneManager.LoadLevel doing this for you. }
  Resources.Prepare(
    Window.SceneManager.Items.BaseLights,
    Window.SceneManager.Items.GravityUp, 'resources');

  Window.SceneManager.Items.Add(Animation.Scene(Time, true));
  Application.Run;
end.
