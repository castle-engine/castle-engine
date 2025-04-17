// -*- compile-command: "./test_single_testcase.sh TTestCastleSceneManager" -*-
{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleSceneManager unit. }
unit TestCastleSceneManager;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleSceneManager = class(TCastleTestCase)
  published
    procedure TestNavigationCreating;
    procedure TestLoadLevel;
  end;

implementation

uses CastleCameras, CastleSceneManager, CastleScene, CastleSceneCore,
  CastleFilesUtils, CastleVectors, CastleViewport;

procedure TTestCastleSceneManager.TestNavigationCreating;
var
  SceneManager: TCastleSceneManager;
  C: TCastleNavigation;
begin
  SceneManager := TCastleSceneManager.Create(nil);
  try
    AssertTrue(SceneManager.Navigation = nil);
    AssertTrue(SceneManager.WalkNavigation(false) = nil);
    AssertTrue(SceneManager.ExamineNavigation(false) = nil);
    AssertTrue(SceneManager.Navigation = nil);

    SceneManager.NavigationType := ntWalk;
    C := SceneManager.Navigation;
    AssertTrue(C <> nil);
    AssertTrue(SceneManager.NavigationType = ntWalk);

    SceneManager.NavigationType := ntFly;
    AssertTrue(C = SceneManager.Navigation); // ntWalk -> ntFly didn't change camera instance
    AssertTrue(C = SceneManager.WalkNavigation);
    AssertTrue(C = SceneManager.WalkNavigation(false));
    AssertTrue(SceneManager.NavigationType = ntFly);

    C.Radius := 100;
    AssertTrue(SceneManager.Navigation.Radius = 100);

    SceneManager.NavigationType := ntExamine;
    AssertTrue(C <> SceneManager.Navigation); // ntFly -> ntExamine did change camera instance
    AssertTrue(SceneManager.NavigationType = ntExamine);
    // but the Radius was preserved when copying
    AssertTrue(SceneManager.Navigation.Radius = 100);
  finally FreeAndNil(SceneManager) end;

  SceneManager := TCastleSceneManager.Create(nil);
  try
    AssertTrue(SceneManager.Navigation = nil);

    SceneManager.WalkNavigation.Radius := 100;
    AssertTrue(SceneManager.Navigation <> nil);
    AssertTrue(SceneManager.Navigation = SceneManager.WalkNavigation);
    AssertTrue(SceneManager.NavigationType = ntWalk);
    AssertTrue(SceneManager.WalkNavigation.PreferGravityUpForRotations);
    AssertTrue(SceneManager.WalkNavigation.PreferGravityUpForMoving);
    AssertTrue(SceneManager.WalkNavigation.Gravity);

    C := SceneManager.WalkNavigation;
    SceneManager.NavigationType := ntFly;
    AssertTrue(SceneManager.Navigation = SceneManager.WalkNavigation);
    AssertTrue(C = SceneManager.WalkNavigation);
    AssertTrue(SceneManager.WalkNavigation.PreferGravityUpForRotations);
    AssertFalse(SceneManager.WalkNavigation.PreferGravityUpForMoving);
    AssertFalse(SceneManager.WalkNavigation.Gravity);

    // changing to ExamineNavigation and back to WalkNavigation sets ntWalk again
    SceneManager.ExamineNavigation;
    SceneManager.WalkNavigation;
    AssertTrue(SceneManager.NavigationType = ntWalk);
    AssertTrue(SceneManager.WalkNavigation.PreferGravityUpForRotations);
    AssertTrue(SceneManager.WalkNavigation.PreferGravityUpForMoving);
    AssertTrue(SceneManager.WalkNavigation.Gravity);
  finally FreeAndNil(SceneManager) end;
end;

procedure TTestCastleSceneManager.TestLoadLevel;
var
  Scene: TCastleScene;
  SceneManager: TCastleSceneManager;
  Pos, Dir, Up, Pos2, Dir2, Up2: TVector3;
begin
  SceneManager := TCastleSceneManager.Create(nil);
  try
    Scene := TCastleScene.Create(SceneManager);
    Scene.Load('castle-data:/level1.x3d');
    Scene.PreciseCollisions := true;
    Scene.ProcessEvents := true;

    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;

    { Note that this depends on hack within RequiredNavigation:
      We depend that RequiredNavigation calls EnsureCameraDetected,
      thus Camera position/dir/up are correct after RequiredNavigation
      (even though it's not guaranteed in docs). }

    AssertTrue(SceneManager.NavigationType = ntNone);
    SceneManager.RequiredNavigation;
    AssertTrue(SceneManager.NavigationType = ntExamine);
    SceneManager.Navigation.Camera.GetWorldView(Pos, Dir, Up);
    AssertVectorEquals(Vector3(3.249692, 2.000000, -5.416155), Pos);
  finally FreeAndNil(SceneManager) end;

  SceneManager := TCastleSceneManager.Create(nil);
  try
    Scene := TCastleScene.Create(SceneManager);
    Scene.Load('castle-data:/level1.x3d');
    Scene.PreciseCollisions := true;
    Scene.ProcessEvents := true;

    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;

    { Note that this depends on hack within RequiredNavigation:
      We depend that RequiredNavigation calls EnsureCameraDetected,
      thus Camera position/dir/up are correct after RequiredNavigation
      (even though it's not guaranteed in docs). }

    // changes Examine into Walk, preserving the camera view
    SceneManager.NavigationType := ntWalk;
    SceneManager.WalkNavigation.MoveSpeed := 10;
    SceneManager.Navigation.Camera.GetWorldView(Pos2, Dir2, Up2);
    AssertVectorEquals(Pos, Pos2);
    AssertVectorEquals(Vector3(3.249692, 2.000000, -5.416155), Pos);
    AssertVectorEquals(Dir, Dir2);
    AssertVectorEquals(Up, Up2);
    AssertTrue(SceneManager.NavigationType = ntWalk);

    AssertTrue(
      SceneManager.WalkNavigation.PreferredHeight >
      SceneManager.WalkNavigation.Radius);

    AssertSameValue(1.75, SceneManager.WalkNavigation.PreferredHeight);
    AssertSameValue(0.25, SceneManager.WalkNavigation.Radius);
    AssertSameValue(0.75, SceneManager.WalkNavigation.ClimbHeight);
  finally FreeAndNil(SceneManager) end;
end;

initialization
  RegisterTest(TTestCastleSceneManager);
end.
