{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This opens VRML/X3D file, renders it, allows to move in the scene
  (with navigation adjusted to NavigationInfo.type, see view3dscene docs
  for keys/mouse to control Walk/Examine navigation
  [http://castle-engine.sourceforge.net/view3dscene.php]) and makes
  events working.

  Run with a command-line option to give 3D model filename. For example,
    ./simplest_vrml_browser models/bridge_final.x3dv

  This program is a minimal VRML browser, passing all user interactions
  to the Scene (so that they can be processed by VRML events).

  To extend this into your own game:

  1. Look at all the window methods and callbacks of TCastleWindowBase,
     which is an ancestor of TCastleWindowCustom, which is an ancestor of
     TCastleWindow (that you have inside Window variable).
     For example: assigning your own handlers for Window.OnKeyDown
     or Window.OnIdle is often useful.

  2. Look at TCastleSceneManager methods and properties (you have TCastleSceneManager
     instance inside Window.SceneManager variable of this program).

  3. Look at TCastleScene (and it's parent, TCastleSceneCore) methods and properties
     (you have TCastleScene instance inside Window.Scene
     variable of this program; the same thing is also available in
     Window.SceneManager.MainScene).

  4. Finally, often it's more comfortable to just create your own
     TCastleSceneManager and TCastleScene instances explicitly, using TCastleWindowCustom
     (instead of TCastleWindow). This way you get a little more control
     and understanding of our scene manager, which is really the core
     of our engine since version 2.0. It's quite easy, see
     scene_manager_demos.lpr example.
}

program simplest_vrml_browser;

{$apptype CONSOLE}

uses CastleUtils, CastleWindow, ProgressUnit, ProgressConsole,
  CastleSceneCore, SysUtils, CastleWarnings, CastleParameters;

var
  Window: TCastleWindow;
  FileName: string = 'models' + PathDelim + 'teapot.x3dv';

begin
  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    FileName := Parameters[1];

  OnWarning := @OnWarningWrite;
  Progress.UserInterface := ProgressConsoleInterface;

  Window := TCastleWindow.Create(Application);

  Window.Load(FileName);
  Writeln(Window.MainScene.Info(true, true, false));
  Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Window.MainScene.ProcessEvents := true;

  Window.OpenAndRun;
end.
