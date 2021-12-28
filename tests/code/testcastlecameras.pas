// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleCameras" -*-
{
  Copyright 2004-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleCameras unit. }
unit TestCastleCameras;

interface

uses
  Classes, SysUtils{$ifndef CASTLE_TESTER}, FpcUnit, TestUtils, TestRegistry{$else}
  , CastleTester{$endif};

type
  TTestCameras = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
    procedure TestToOrientationAndBack;
    procedure TestOrientationFromBasicAxes;
    procedure TestInput;
  end;

implementation

uses CastleUtils, CastleVectors, X3DCameraUtils, Math, CastleCameras, CastleQuaternions;

procedure TTestCameras.TestToOrientationAndBack;

  procedure CamOrientToDirUp(const Orient: TVector4; var Dir, Up: TVector3);
  var OrientAxis: TVector3 absolute Orient;
  begin
   Dir := RotatePointAroundAxisRad(Orient[3], Vector3(0, 0, -1), OrientAxis);
   Up := RotatePointAroundAxisRad(Orient[3], Vector3(0, 1, 0 ), OrientAxis);
  end;

const
  TestOrients: array[0..4]of TVector4 =
  (
    { orientacje z lbview }
    (Data: (-1, 0, 0, 1.5708)),
    (Data: (-0.590284, 0.769274, 0.244504, 0.987861)),
    (Data: (0, 0, 1, 0)),
    (Data: (0, 1, 0, 1.5708)),
    { Here original orient.c failed, as far as I remember.
      Looks like my OrientationFromDirectionUp fixes this. }
    (Data: (0.9133538007736206, -0.23269428312778472, -0.33412325382232666, 1.807408332824707))
  );

  EqEpsilon = 0.01; // larger epsilon for ppc64

var Dir, Up: TVector3;
    i: Integer;
    NewOrient: TVector4;
    oohFailed: boolean;
begin
 { Uzywam zmiennej oohFailed bo chce wykonac zawsze test dla wszystkich
   wymienionych orientacji, nawet jesli niektore po drodze zawioda.
   I tak bede chcial zobaczyc ktorym sie udalo.
   Dopiero po wykonaniu wszystkich testow rzucam wyjatek jezeli jakis
   test zakonczyl sie porazka. }
 oohFailed := false;

 for i := 0 to High(TestOrients) do
 begin
  CamOrientToDirUp(TestOrients[i], Dir, Up);
  NewOrient := OrientationFromDirectionUp(Dir, Up);
  if not ( (IsZero(NewOrient[3]) and IsZero(TestOrients[i][3])) or
	   TVector4.Equals(NewOrient, TestOrients[i], EqEpsilon) ) then
  begin
   Writeln(Format(
     'failed z TestOrients[%d] = %s' +nl+
     'Dir = %s' +nl+
     'Up = %s' +nl+
     'NewOrient = %s',
     [ i,
       TestOrients[i].ToString,
       Dir.ToString,
       Up.ToString,
       NewOrient.ToString ]));
   oohFailed := true;
  end;
 end;

 AssertTrue(not oohFailed);
end;

procedure TTestCameras.TestOrientationFromBasicAxes;
const
  Tests: array [1..18] of record Dir, Up: TVector3; end =
  (
    { +X up }
    (Dir: (Data: (0, -1, 0)); Up: (Data: (1, 0, 0))),
    (Dir: (Data: (0, +1, 0)); Up: (Data: (1, 0, 0))),
    (Dir: (Data: (0, 0, -1)); Up: (Data: (1, 0, 0))),
    (Dir: (Data: (0, 0, +1)); Up: (Data: (1, 0, 0))),

    { +Y up }
    (Dir: (Data: (-1, 0, 0)); Up: (Data: (0, 1, 0))),
    (Dir: (Data: (+1, 0, 0)); Up: (Data: (0, 1, 0))),
    (Dir: (Data: (0, 0, -1)); Up: (Data: (0, 1, 0))),
    (Dir: (Data: (0, 0, +1)); Up: (Data: (0, 1, 0))),

    { +Z up }
    (Dir: (Data: (-1, 0, 0)); Up: (Data: (0, 0, 1))),
    (Dir: (Data: (+1, 0, 0)); Up: (Data: (0, 0, 1))),
    (Dir: (Data: (0, -1, 0)); Up: (Data: (0, 0, 1))),
    (Dir: (Data: (0, +1, 0)); Up: (Data: (0, 0, 1))),

    { From https://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&t=35
      The 3rd one fails. }
    (Dir: (Data: (0, -1, 1)); Up: (Data: (0, 1, 1))),
    (Dir: (Data: (0, -0.71, 0.71)); Up: (Data: (0, 0.71, 0.71))),
    (Dir: (Data: (0, -0.7099999785423278, 0.7099999785423278)); Up: (Data: (0, 0.7100000381469726, 0.7099999189376831))),

    (Dir: (Data: (7.9712307865520415E-008,  0.410611480474472 ,  0.9117947816848754)); Up: (Data: (-3.5896810857138917E-008, 0.9118096828460693, -0.410611480474472))),
    (Dir: (Data: (0                      ,  0.410611480474472 ,  0.9117947816848754)); Up: (Data: (-                      0, 0.9118096828460693, -0.410611480474472))),
    (Dir: (Data: (-0.0031079668551683    , -0.0010071427095681, -0.9999946355819702)); Up: (Data: (-0.0020249725785106     , 0.9999974370002747, -0.0010008082026616)))
  );

  procedure DoTest(const Dir, Up: TVector3; const TestName: string);
  var
    Orientation: TQuaternion;
  begin
    if not IsZero(TVector3.DotProduct(Dir, Up), 0.001) then
      Fail(TestName + ': Given test dir/up not orthogonal, OrientationQuaternionFromDirectionUp assumes it');

    Orientation := OrientationQuaternionFromDirectionUp(Dir, Up).Conjugate;
    try
      AssertTrue(TVector3.Equals(Orientation.Rotate(Dir.Normalize), DefaultX3DCameraDirection, 0.01));
      AssertTrue(TVector3.Equals(Orientation.Rotate(Up .Normalize), DefaultX3DCameraUp       , 0.01));
    except
      Writeln('Failed on ', TestName, '. Resulting dir is ',
        Orientation.Rotate(Dir.Normalize).ToString,
        ', resulting up is ',
        Orientation.Rotate(Up.Normalize).ToString);
      raise;
    end;
  end;

var
  I: Integer;
begin
  for I := Low(Tests) to High(Tests) do
    DoTest(Tests[I].Dir, Tests[I].Up, Format('%d', [I]));

  { Now negate the Up vectors, for additional tests }
  for I := Low(Tests) to High(Tests) do
    DoTest(Tests[I].Dir, -Tests[I].Up, Format('%d (negated up)', [I]));
end;

procedure TTestCameras.TestInput;

  procedure AssertCamera(const C: TCamera; const Input: TCameraInputs;
    const IgnoreAllInputs, MouseNavigation: boolean);
  begin
    AssertTrue(C.Input = Input);
    {$ifdef FPC}
    {$warnings off}
    { Consciously using here deprecated IgnoreAllInputs
      and MouseNavigation (to test it's still Ok) }
    AssertTrue(C.IgnoreAllInputs = IgnoreAllInputs);
    if C is TExamineCamera then
      AssertTrue(TExamineCamera(C).MouseNavigation = MouseNavigation);
    {$warnings on}
    {$endif}
  end;

var
  E: TExamineCamera;
  W: TWalkCamera;
begin
  E := TExamineCamera.Create(nil);
  try
    AssertCamera(E, TCamera.DefaultInput, false, true);
    E.Input := [];
    AssertCamera(E, [], true, false);
    {$ifdef FPC}
    {$warnings off}
    { Consciously using here deprecated IgnoreAllInputs and MouseNavigation (to test it's still Ok) }
    E.MouseNavigation := true;
    AssertCamera(E, [ciMouseDragging], false, true);
    E.IgnoreAllInputs := true;
    {$warnings on}
    {$endif}
    AssertCamera(E, [], true, false);
    E.Input := [ciNormal];
    AssertCamera(E, [ciNormal], false, false);
  finally FreeAndNil(E) end;

  W := TWalkCamera.Create(nil);
  try
    AssertCamera(W, TCamera.DefaultInput, false, true);
    W.Input := [];
    AssertCamera(W, [], true, false);
    W.Input := W.Input + [ciMouseDragging];
    AssertCamera(W, [ciMouseDragging], false, true);
    {$ifdef FPC}
    {$warnings off}
    { Consciously using here deprecated IgnoreAllInputs (to test it's still Ok) }
    W.IgnoreAllInputs := true;
    {$warnings on}
    AssertCamera(W, [], true, false);
    {$endif}
    W.Input := [ciNormal];
    AssertCamera(W, [ciNormal], false, false);
  finally FreeAndNil(W) end;
end;

{$ifndef CASTLE_TESTER}
initialization
 RegisterTest(TTestCameras);
{$endif}
end.
