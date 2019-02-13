unit TestCastleSceneManager;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleTestCase;

type
  TTestCastleSceneManager = class(TCastleTestCase)
  published
    procedure TestCameraCreating;
    procedure TestLoadLevel;
  end;

implementation

uses CastleCameras, CastleSceneManager, CastleScene, CastleSceneCore,
  CastleFilesUtils, CastleVectors;

procedure TTestCastleSceneManager.TestCameraCreating;
var
  SceneManager: TCastleSceneManager;
  C: TCamera;
begin
  SceneManager := TCastleSceneManager.Create(nil);
  try
    AssertTrue(SceneManager.Camera = nil);
    AssertTrue(SceneManager.WalkCamera(false) = nil);
    AssertTrue(SceneManager.ExamineCamera(false) = nil);
    AssertTrue(SceneManager.Camera = nil);

    SceneManager.NavigationType := ntWalk;
    C := SceneManager.Camera;
    AssertTrue(C <> nil);
    AssertTrue(SceneManager.NavigationType = ntWalk);

    SceneManager.NavigationType := ntFly;
    AssertTrue(C = SceneManager.Camera); // ntWalk -> ntFly didn't change camera instance
    AssertTrue(C = SceneManager.WalkCamera);
    AssertTrue(C = SceneManager.WalkCamera(false));
    AssertTrue(SceneManager.NavigationType = ntFly);

    C.Radius := 100;
    AssertTrue(SceneManager.Camera.Radius = 100);

    SceneManager.NavigationType := ntExamine;
    AssertTrue(C <> SceneManager.Camera); // ntFly -> ntExamine did change camera instance
    AssertTrue(SceneManager.NavigationType = ntExamine);
    // but the Radius was preserved when copying
    AssertTrue(SceneManager.Camera.Radius = 100);
  finally FreeAndNil(SceneManager) end;

  SceneManager := TCastleSceneManager.Create(nil);
  try
    AssertTrue(SceneManager.Camera = nil);

    SceneManager.WalkCamera.Radius := 100;
    AssertTrue(SceneManager.Camera <> nil);
    AssertTrue(SceneManager.Camera = SceneManager.WalkCamera);
    AssertTrue(SceneManager.NavigationType = ntWalk);
    AssertTrue(SceneManager.WalkCamera.PreferGravityUpForRotations);
    AssertTrue(SceneManager.WalkCamera.PreferGravityUpForMoving);
    AssertTrue(SceneManager.WalkCamera.Gravity);

    C := SceneManager.WalkCamera;
    SceneManager.NavigationType := ntFly;
    AssertTrue(SceneManager.Camera = SceneManager.WalkCamera);
    AssertTrue(C = SceneManager.WalkCamera);
    AssertTrue(SceneManager.WalkCamera.PreferGravityUpForRotations);
    AssertFalse(SceneManager.WalkCamera.PreferGravityUpForMoving);
    AssertFalse(SceneManager.WalkCamera.Gravity);

    // changing to ExamineCamera and back to WalkCamera sets ntWalk again
    SceneManager.ExamineCamera;
    SceneManager.WalkCamera;
    AssertTrue(SceneManager.NavigationType = ntWalk);
    AssertTrue(SceneManager.WalkCamera.PreferGravityUpForRotations);
    AssertTrue(SceneManager.WalkCamera.PreferGravityUpForMoving);
    AssertTrue(SceneManager.WalkCamera.Gravity);
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
    Scene.Load(ApplicationData('level1.x3d'));
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;

    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;

    AssertTrue(SceneManager.NavigationType = ntNone);
    SceneManager.RequiredCamera;
    AssertTrue(SceneManager.NavigationType = ntExamine);
    SceneManager.Camera.GetView(Pos, Dir, Up);
  finally FreeAndNil(SceneManager) end;

  SceneManager := TCastleSceneManager.Create(nil);
  try
    Scene := TCastleScene.Create(SceneManager);
    Scene.Load(ApplicationData('level1.x3d'));
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;

    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;
    // changes Examine into Walk, preserving the camera view
    SceneManager.NavigationType := ntWalk;
    SceneManager.WalkCamera.MoveSpeed := 10;
    SceneManager.Camera.GetView(Pos2, Dir2, Up2);
    AssertVectorEquals(Pos, Pos2);
    AssertVectorEquals(Pos, Vector3(3.249692, 2.000000, -5.416155));
    AssertVectorEquals(Dir, Dir2);
    AssertVectorEquals(Up, Up2);
    AssertTrue(SceneManager.NavigationType = ntWalk);

    AssertTrue(
      SceneManager.WalkCamera.PreferredHeight >
      SceneManager.WalkCamera.Radius);

    AssertSameValue(1.75, SceneManager.WalkCamera.PreferredHeight);
    AssertSameValue(0.25, SceneManager.WalkCamera.Radius);
    AssertSameValue(0.75, SceneManager.WalkCamera.ClimbHeight);
  finally FreeAndNil(SceneManager) end;
end;

initialization
  RegisterTest(TTestCastleSceneManager);
end.
