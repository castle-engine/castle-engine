{
  Copyright 2004-2005 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

uses KambiUtils, VectorMath, VRMLFields, VRMLCameraUtils;

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
  if not ( (IsZero(NewOrient[3]) and IsZero(TestOrients[i, 3])) or
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
