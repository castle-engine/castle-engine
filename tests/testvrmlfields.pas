{
  Copyright 2004-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestVRMLFields;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVRMLFields = class(TTestCase)
    procedure TestVRMLFields;
  end;

implementation

uses KambiUtils, VectorMath, VRMLFields, VRMLCameraUtils, Math, Cameras;

procedure TTestVRMLFields.TestVRMLFields;

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

initialization
 RegisterTest(TTestVRMLFields);
end.
