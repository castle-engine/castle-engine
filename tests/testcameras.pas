{
  Copyright 2004-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCameras;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestCameras = class(TTestCase)
    procedure TestToOrientationAndBack;
    procedure TestOrientationFromBasicAxes;
  end;

implementation

uses CastleUtils, VectorMath, X3DCameraUtils, Math, Cameras, Quaternions;

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

 Assert(not oohFailed);
end;

procedure TTestCameras.TestOrientationFromBasicAxes;
const
  Tests: array [1..15] of record Dir, Up: TVector3Single; end =
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
    (Dir: (0, -0.7099999785423278, 0.7099999785423278); Up: (0, 0.7100000381469726, 0.7099999189376831))
  );

  procedure DoTest(const Dir, Up: TVector3Single; const TestName: string);
  var
    Orientation: TQuaternion;
  begin
    Orientation := CamDirUp2OrientQuat(Dir, Up).Conjugate;
    try
      Assert(VectorsEqual(Orientation.Rotate(Normalized(Dir)), DefaultVRMLCameraDirection, 0.01));
      Assert(VectorsEqual(Orientation.Rotate(Normalized(Up )), DefaultVRMLCameraUp       , 0.01));
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

initialization
 RegisterTest(TTestCameras);
end.
