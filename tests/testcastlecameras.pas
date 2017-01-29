{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleCameras;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestCameras = class(TTestCase)
    procedure TestToOrientationAndBack;
    procedure TestOrientationFromBasicAxes;
    procedure TestInput;
  end;

implementation

uses CastleUtils, CastleVectors, X3DCameraUtils, Math, CastleCameras, CastleQuaternions;

procedure TTestCameras.TestToOrientationAndBack;

  procedure CamOrientToDirUp(const Orient: TVector4Single; var Dir, Up: TVector3Single);
  var OrientAxis: TVector3Single absolute Orient;
  begin
   Dir := RotatePointAroundAxisRad(Orient[3], Vector3Single(0, 0, -1), OrientAxis);
   Up := RotatePointAroundAxisRad(Orient[3], Vector3Single(0, 1, 0 ), OrientAxis);
  end;

const
  TestOrients: array[0..4]of TVector4Single =
  (
    { orientacje z lbview }
    (-1, 0, 0, 1.5708),
    (-0.590284, 0.769274, 0.244504, 0.987861),
    (0, 0, 1, 0),
    (0, 1, 0, 1.5708),

    { Here original orient.c failed, as far as I remember.
      Looks like my CamDirUp2Orient fixes this. }
    (0.9133538007736206, -0.23269428312778472, -0.33412325382232666, 1.807408332824707)
  );

  EqEpsilon = 0.0001;

var Dir, Up: TVector3Single;
    i: Integer;
    NewOrient: TVector4Single;
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
  NewOrient := CamDirUp2Orient(Dir, Up);
  if not ( (Zero(NewOrient[3]) and Zero(TestOrients[i, 3])) or
	   VectorsEqual(NewOrient, TestOrients[i], EqEpsilon) ) then
  begin
   Writeln(Format(
     'failed z TestOrients[%d] = %s' +nl+
     'Dir = %s' +nl+
     'Up = %s' +nl+
     'NewOrient = %s',
     [ i,
       VectorToNiceStr(TestOrients[i]),
       VectorToNiceStr(Dir),
       VectorToNiceStr(Up),
       VectorToNiceStr(NewOrient) ]));
   oohFailed := true;
  end;
 end;

 AssertTrue(not oohFailed);
end;

procedure TTestCameras.TestOrientationFromBasicAxes;
const
  Tests: array [1..18] of record Dir, Up: TVector3Single; end =
  (
    { +X up }
    (Dir: (0, -1, 0); Up: (1, 0, 0)),
    (Dir: (0, +1, 0); Up: (1, 0, 0)),
    (Dir: (0, 0, -1); Up: (1, 0, 0)),
    (Dir: (0, 0, +1); Up: (1, 0, 0)),

    { +Y up }
    (Dir: (-1, 0, 0); Up: (0, 1, 0)),
    (Dir: (+1, 0, 0); Up: (0, 1, 0)),
    (Dir: (0, 0, -1); Up: (0, 1, 0)),
    (Dir: (0, 0, +1); Up: (0, 1, 0)),

    { +Z up }
    (Dir: (-1, 0, 0); Up: (0, 0, 1)),
    (Dir: (+1, 0, 0); Up: (0, 0, 1)),
    (Dir: (0, -1, 0); Up: (0, 0, 1)),
    (Dir: (0, +1, 0); Up: (0, 0, 1)),

    { From https://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&t=35
      The 3rd one fails. }
    (Dir: (0, -1, 1); Up: (0, 1, 1)),
    (Dir: (0, -0.71, 0.71); Up: (0, 0.71, 0.71)),
    (Dir: (0, -0.7099999785423278, 0.7099999785423278); Up: (0, 0.7100000381469726, 0.7099999189376831)),

    (Dir: (7.9712307865520415E-008, 0.410611480474472, 0.9117947816848754); Up: (-3.5896810857138917E-008, 0.9118096828460693, -0.410611480474472)),
    (Dir: (0                      , 0.410611480474472, 0.9117947816848754); Up: (-                      0, 0.9118096828460693, -0.410611480474472)),
    (Dir: (-0.0031079668551683, -0.0010071427095681, -0.9999946355819702); Up : (-0.0020249725785106, 0.9999974370002747, -0.0010008082026616))
  );

  procedure DoTest(const Dir, Up: TVector3Single; const TestName: string);
  var
    Orientation: TQuaternion;
  begin
    if not Zero(VectorDotProduct(Dir, Up), 0.001) then
      Fail(TestName + ': Given test dir/up not orthogonal, CamDirUp2OrientQuat assumes it');

    Orientation := CamDirUp2OrientQuat(Dir, Up).Conjugate;
    try
      AssertTrue(VectorsEqual(Orientation.Rotate(Normalized(Dir)), DefaultX3DCameraDirection, 0.01));
      AssertTrue(VectorsEqual(Orientation.Rotate(Normalized(Up )), DefaultX3DCameraUp       , 0.01));
    except
      Writeln('Failed on ', TestName, '. Resulting dir is ',
        VectorToNiceStr(Orientation.Rotate(Normalized(Dir))),
        ', resulting up is ',
        VectorToNiceStr(Orientation.Rotate(Normalized(Up))));
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
    {$warnings off}
    { Consciously using here deprecated IgnoreAllInputs
      and MouseNavigation (to test it's still Ok) }
    AssertTrue(C.IgnoreAllInputs = IgnoreAllInputs);
    if C is TExamineCamera then
      AssertTrue(TExamineCamera(C).MouseNavigation = MouseNavigation);
    {$warnings on}
    { for TUniversalCamera, child examine/walk must always have synchronized
      properties }
    if C is TUniversalCamera then
    begin
      AssertCamera(TUniversalCamera(C).Walk   , Input, IgnoreAllInputs, MouseNavigation);
      AssertCamera(TUniversalCamera(C).Examine, Input, IgnoreAllInputs, MouseNavigation);
    end;
  end;

var
  E: TExamineCamera;
  U: TUniversalCamera;
  W: TWalkCamera;
begin
  E := TExamineCamera.Create(nil);
  try
    AssertCamera(E, TCamera.DefaultInput, false, true);
    E.Input := [];
    AssertCamera(E, [], true, false);
    {$warnings off}
    { Consciously using here deprecated IgnoreAllInputs and MouseNavigation (to test it's still Ok) }
    E.MouseNavigation := true;
    AssertCamera(E, [ciMouseDragging], false, true);
    E.IgnoreAllInputs := true;
    {$warnings on}
    AssertCamera(E, [], true, false);
    E.Input := [ciNormal];
    AssertCamera(E, [ciNormal], false, false);
  finally FreeAndNil(E) end;

  U := TUniversalCamera.Create(nil);
  try
    AssertCamera(U, TCamera.DefaultInput, false, true);
    U.Input := [];
    AssertCamera(U, [], true, false);
    U.Input := U.Input + [ciMouseDragging];
    AssertCamera(U, [ciMouseDragging], false, true);
    {$warnings off}
    { Consciously using here deprecated IgnoreAllInputs (to test it's still Ok) }
    U.IgnoreAllInputs := true;
    {$warnings on}
    AssertCamera(U, [], true, false);
    U.Input := [ciNormal];
    AssertCamera(U, [ciNormal], false, false);
  finally FreeAndNil(U) end;

  W := TWalkCamera.Create(nil);
  try
    AssertCamera(W, TCamera.DefaultInput, false, true);
    W.Input := [];
    AssertCamera(W, [], true, false);
    W.Input := W.Input + [ciMouseDragging];
    AssertCamera(W, [ciMouseDragging], false, true);
    {$warnings off}
    { Consciously using here deprecated IgnoreAllInputs (to test it's still Ok) }
    W.IgnoreAllInputs := true;
    {$warnings on}
    AssertCamera(W, [], true, false);
    W.Input := [ciNormal];
    AssertCamera(W, [ciNormal], false, false);
  finally FreeAndNil(W) end;
end;

initialization
 RegisterTest(TTestCameras);
end.
