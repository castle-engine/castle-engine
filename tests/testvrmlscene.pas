unit TestVRMLScene;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVRMLScene = class(TTestCase)
  published
    procedure TestBorderManifoldEdges;
  end;

implementation

uses VRMLNodes, VRMLScene, Object3dAsVRML, VectorMath;

procedure TTestVRMLScene.TestBorderManifoldEdges;
var
  Scene: TVRMLScene;
begin
  Scene := TVRMLScene.Create(LoadAsVRML('model_manifold.wrl'), true);
  try
    Assert(Scene.BorderEdges.Count = 0);
  finally FreeAndNil(Scene) end;
end;

initialization
  RegisterTest(TTestVRMLScene);
end.
