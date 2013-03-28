unit TestSceneCore;

{$I tests.inc}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestSceneCore = class(TTestCase)
  published
    procedure TestBorderManifoldEdges;
    procedure TestIterator;
    { $define ITERATOR_SPEED_TEST}
    {$ifdef ITERATOR_SPEED_TEST}
    procedure TestIteratorSpeed;
    {$endif ITERATOR_SPEED_TEST}
    procedure TestFind;
  end;

implementation

uses X3DNodes, CastleSceneCore, X3DLoad, CastleVectors, CastleShapes,
  CastleTimeUtils, CastleStringUtils, X3DFields;

procedure TTestSceneCore.TestBorderManifoldEdges;
var
  Scene: TCastleSceneCore;
begin
  Scene := TCastleSceneCore.Create(nil);
  try
    Scene.Load('data' + PathDelim + 'model_manifold.wrl');
    Assert(Scene.BorderEdges.Count = 0);
  finally FreeAndNil(Scene) end;
end;

{$ifdef ITERATOR_SPEED_TEST}
procedure TTestSceneCore.TestIteratorSpeed;

  procedure CheckIteratorSpeed(const FileName: string;
    const TestCount: Integer = 1000);
  var
    Scene: TCastleSceneCore;
    List: TShapesList;
    SI: TShapeTreeIterator;
    OnlyActive: boolean;
    I: Integer;
    Test: Integer;
  begin
    Writeln('CheckIteratorSpeed on ', FileName);

    Scene := TCastleSceneCore.Create(nil);
    try
      Scene.Load(FileName);
      for OnlyActive := false to true do
      begin
        ProcessTimerBegin;
        for Test := 0 to TestCount - 1 do
        begin
          List := TShapesList.Create(Scene.Shapes, OnlyActive);
          for I := 0 to List.Count - 1 do
            { Just do anything that requires access to List[I] }
            PointerToStr(List[I].Geometry);
          FreeAndNil(List);
        end;
        Writeln('TShapesList traverse: ', ProcessTimerEnd:1:2);

        ProcessTimerBegin;
        for Test := 0 to TestCount - 1 do
        begin
          SI := TShapeTreeIterator.Create(Scene.Shapes, OnlyActive);
          while SI.GetNext do
            PointerToStr(SI.Current.Geometry);
          FreeAndNil(SI);
        end;
        Writeln('TShapeTreeIterator: ', ProcessTimerEnd:1:2);

      end;
    finally FreeAndNil(Scene) end;
  end;

begin
  CheckIteratorSpeed('../../demo_models/x3d/deranged_house_final.x3dv');
  CheckIteratorSpeed('../../demo_models/x3d/anchor_test.x3dv');
  CheckIteratorSpeed('../../demo_models/x3d/switches_and_transforms.x3dv');
  CheckIteratorSpeed('../../demo_models/x3d/key_sensor.x3dv');

  CheckIteratorSpeed('data' + PathDelim + 'switches_and_transforms_2.x3dv');
  CheckIteratorSpeed('data' + PathDelim + 'key_sensor_2.x3dv');

  CheckIteratorSpeed('/home/michalis/sources/rrtankticks2/rrtankticks3/rrtt.wrl',
    10); { smaller TestCount, as it's quite slow }
end;
{$endif ITERATOR_SPEED_TEST}

procedure TTestSceneCore.TestIterator;

  procedure CheckIterator(const FileName: string);
  var
    Scene: TCastleSceneCore;
    List: TShapeList;
    SI: TShapeTreeIterator;
    OnlyActive: boolean;
    I: Integer;
  begin
    Scene := TCastleSceneCore.Create(nil);
    try
      Scene.Load(FileName);
      for OnlyActive := false to true do
      begin
        { Compare the simple iterator implementation (that just calls
          Traverse and gathers results to the list) with actual sophisticated
          implementation in TShapeTreeIterator. }
        List := TShapeList.Create(Scene.Shapes, OnlyActive);
        SI := TShapeTreeIterator.Create(Scene.Shapes, OnlyActive);
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
  {$ifdef CASTLE_ENGINE_TRUNK_AVAILABLE}
  CheckIterator('../../demo_models/x3d/deranged_house_final.x3dv');
  CheckIterator('../../demo_models/x3d/anchor_test.x3dv');
  CheckIterator('../../demo_models/x3d/switches_and_transforms.x3dv');
  CheckIterator('../../demo_models/x3d/key_sensor.x3dv');
  {$endif CASTLE_ENGINE_TRUNK_AVAILABLE}

  CheckIterator('data' + PathDelim + 'switches_and_transforms_2.x3dv');
  CheckIterator('data' + PathDelim + 'key_sensor_2.x3dv');
  CheckIterator('data' + PathDelim + 'extrusion_empty_spine.x3dv');
  CheckIterator('data' + PathDelim + 'extrusion_empty_spine_concave.x3dv');
  CheckIterator('data' + PathDelim + 'extrusion_empty_spine_smooth.x3dv');
end;

procedure TTestSceneCore.TestFind;
var
  Scene: TCastleSceneCore;
begin
  Scene := TCastleSceneCore.Create(nil);
  try
    try
      Scene.Node('Left');
      Assert(false, 'Should fail with EX3DNotFound');
    except on EX3DNotFound do ; end;

    try
      Scene.Field('Left', 'translation');
      Assert(false, 'Should fail with EX3DNotFound');
    except on EX3DNotFound do ; end;

    try
      Scene.Event('Left', 'addChildren');
      Assert(false, 'Should fail with EX3DNotFound');
    except on EX3DNotFound do ; end;

    Scene.URL := 'data/switches_and_transforms_2.x3dv';
    Scene.Node('Left');
    Scene.Field('Left', 'translation');
    Scene.Event('Left', 'addChildren');

    try
      Scene.Node('Blah');
      Assert(false, 'Should fail with EX3DNotFound');
    except on EX3DNotFound do ; end;

    try
      Scene.Field('Blah', 'translation');
      Assert(false, 'Should fail with EX3DNotFound');
    except on EX3DNotFound do ; end;

    try
      Scene.Event('Blah', 'addChildren');
      Assert(false, 'Should fail with EX3DNotFound');
    except on EX3DNotFound do ; end;

    try
      Scene.Field('Left', 'blah');
      Assert(false, 'Should fail with EX3DNotFound');
    except on EX3DNotFound do ; end;

    try
      Scene.Event('Left', 'blah');
      Assert(false, 'Should fail with EX3DNotFound');
    except on EX3DNotFound do ; end;
  finally FreeAndNil(Scene) end;
end;

initialization
  RegisterTest(TTestSceneCore);
end.
