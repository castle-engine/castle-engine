unit TestVRMLScene;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVRMLScene = class(TTestCase)
  published
    procedure TestBorderManifoldEdges;
    procedure TestIterator;
  end;

implementation

uses VRMLNodes, VRMLScene, Object3dAsVRML, VectorMath, VRMLShape;

procedure TTestVRMLScene.TestBorderManifoldEdges;
var
  Scene: TVRMLScene;
begin
  Scene := TVRMLScene.Create(LoadAsVRML('model_manifold.wrl'), true);
  try
    Assert(Scene.BorderEdges.Count = 0);
  finally FreeAndNil(Scene) end;
end;

procedure TTestVRMLScene.TestIterator;

  procedure CheckIterator(const FileName: string);
  var
    Scene: TVRMLScene;
    List: TVRMLShapesList;
    SI: TVRMLShapeTreeIterator;
    OnlyActive: boolean;
    I: Integer;
  begin
    Scene := TVRMLScene.Create(LoadAsVRML(FileName), true);
    try
      for OnlyActive := false to true do
      begin
        { Compare the simple iterator implementation (that just calls
          Traverse and gathers results to the list) with actual sophisticated
          implementation in TVRMLShapeTreeIterator. }
        List := TVRMLShapesList.Create(Scene.Shapes, OnlyActive);
        SI := TVRMLShapeTreeIterator.Create(Scene.Shapes, OnlyActive);
        for I := 0 to List.Count - 1 do
        begin
          Check(SI.GetNext, 'SI.GetNext');
          Check(SI.Current = List[I], 'SI.Current');
        end;
        Check(not SI.GetNext, 'not SI.GetNext');

//        writeln('done for ', FileName, ' active: ', OnlyActive, ' count is ', List.Count);

        FreeAndNil(List);
        FreeAndNil(SI);
      end;
    finally FreeAndNil(Scene) end;
  end;

begin
  CheckIterator('../../kambi_vrml_test_suite/x3d/deranged_house_final.x3dv');
  CheckIterator('../../kambi_vrml_test_suite/x3d/anchor_test.x3dv');
  CheckIterator('../../kambi_vrml_test_suite/x3d/switches_and_transforms.x3dv');
  CheckIterator('../../kambi_vrml_test_suite/x3d/key_sensor.x3dv');

  CheckIterator('switches_and_transforms_2.x3dv');
  CheckIterator('key_sensor_2.x3dv');
end;

initialization
  RegisterTest(TTestVRMLScene);
end.
