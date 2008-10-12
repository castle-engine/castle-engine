{
  Copyright 2004-2008 Michalis Kamburelis.

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

unit TestDynArrays;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestDynArrays = class(TTestCase)
    procedure TestSetCountItems;
    procedure TestDynArrays;
    procedure TestVectorMathDynArrays;
    procedure TestDynArraysAssign;
    procedure TestDynArraysAssignLerp;
  end;

implementation

uses KambiUtils, VectorMath;

{ DynArray.inc KambiUtils_StandardDynArrays.inc }

function IsSmallerString(const a, b: string): boolean;
begin result := a < b end;

procedure TTestDynArrays.TestSetCountItems;
var
  iarr: TDynIntegerArray;
begin
  iarr := TDynIntegerArray.Create;
  try
    Assert(iarr.Count = 0);

    { growing count works, and sets Items to non-nil }
    iarr.Count := 3;
    Assert(iarr.Count = 3);
    Assert(iarr.Items <> nil);

    { growing count doesn't change previous values }
    iarr[2] := 123;
    iarr.Count := 1000000;
    Assert(iarr[2] = 123);
  finally FreeAndNil(iarr) end;
end;

procedure TTestDynArrays.TestDynArrays;
var sarr, sarr2: TDynStringArray;
    iarr, iarr2: TDynIntegerArray; { tablica int nie jest init_fini }
    i, j: integer;
const twoStrings: array[0..1]of string = ('raz','dwa');
begin
 for i := 1 to 100 do
 begin
  sarr := TDynStringArray.Create;
  try
   sarr.AllowedCapacityOverflow := Random(8);
   sarr.SetLength(4);
   sarr[0] := 'FOO';
   sarr[1] := 'foo bar xyz';
   sarr.Delete(0, 1);
   sarr.AppendArray(twoStrings);
   sarr.AppendItem('trzy?');

   Assert(not sarr.Equal(['foo bar xyz', '', '']));
   Assert(sarr.Equal(['foo bar xyz', '', '', 'raz', 'dwa', 'trzy?']));

   sarr.Reverse;
   Assert(sarr.Equal(['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));

   sarr2 := TDynStringArray.Create;
   try
    sarr2.AppendItem('blah');
    Assert(sarr2.Equal(['blah']));
    sarr2.Assign(sarr);
    Assert(sarr2.Equal(['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));

    {sortuj ustalone 6 stringow}
    sarr.Sort(@IsSmallerString);
    Assert(sarr.Equal(['', '', 'dwa', 'foo bar xyz', 'raz', 'trzy?']));

    { sprawdz ze kolejnosc na sarr2 pozostala niezmieniona }
    Assert(sarr2.Equal(['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));
   finally sarr2.Free end;

   {dodaj losowe stringi, sortuj, sprawdz}
   for j := 0 to 20 do
    sarr.AppendItem( Chr(Random(256)) + Chr(Random(256)) + Chr(Random(256)) );
   sarr.Sort(@IsSmallerString);
   for j := 0 to sarr.Count-2 do Assert(sarr[j] <= sarr[j+1]);

  finally sarr.Free end;
 end;

 sarr := TDynStringArray.Create;
 try
  { na tablicy o 0 liczbie elementow tez wszystko powinno isc ok }
  Assert(sarr.Length = 0);
  sarr.Reverse;
  Assert(sarr.Length = 0);
 finally sarr.Free end;

 iarr := nil;
 iarr2 := nil;
 try
  iarr := TDynIntegerArray.Create;
  iarr2 := TDynIntegerArray.Create;
  Assert(iarr.Equal(iarr2));
  Assert(iarr.Sum = 0);
  Assert(iarr2.Sum = 0);

  iarr.AppendArray([1, 3, 6, 8]);
  Assert(not iarr.Equal(iarr2));
  Assert(iarr.Sum = 1 + 3 + 6 + 8);
  iarr2.AppendArray([1, 3, 6, 8]);
  Assert(iarr.Equal(iarr2));
  iarr2.Insert(0, 99);
  Assert(not iarr.Equal(iarr2));
  Assert(iarr2.Sum = iarr.Sum + 99);
  Assert(iarr2.Equal([99, 1, 3, 6, 8]));

  iarr.Assign(iarr2);
  Assert(iarr.Equal(iarr2));
  Assert(iarr.Equal([99, 1, 3, 6, 8]));

  { simple DeleteDuplicates tests }
  IArr.DeleteDuplicates;
  Assert(iarr.Equal([99, 1, 3, 6, 8]));
  IArr.Insert(0, 3);
  IArr.DeleteDuplicates;
  Assert(iarr.Equal([3, 99, 1, 6, 8]));
  IArr.Count := 0;
  IArr.DeleteDuplicates;
  Assert(iarr.Equal([]));
 finally
  iarr.Free;
  iarr2.Free;
 end;
end;

procedure TTestDynArrays.TestVectorMathDynArrays;
var
  vecs: TDynVector3SingleArray;
begin
  vecs := TDynVector3SingleArray.Create;
  try
    vecs.AppendItem(Vector3Single(1.0, 2.0, 3.0));
    vecs.AppendItem(Vector3Single(4.0, 5.0, 6.0));
    vecs.AppendItem(Vector3Single(1.0, 2.0, 3.0));
    Assert(    VectorsPerfectlyEqual(vecs.Items[0], vecs.Items[2]));
    Assert(not VectorsPerfectlyEqual(vecs.Items[0], vecs.Items[1]));
    Assert(not VectorsPerfectlyEqual(vecs.Items[2], vecs.Items[1]));
  finally FreeAndNil(vecs) end;
end;

procedure TTestDynArrays.TestDynArraysAssign;
var
  V1, V2: TDynVector3SingleArray;
begin
  V1 := TDynVector3SingleArray.Create;
  V2 := TDynVector3SingleArray.Create;
  try
    V1.AppendItem(Vector3Single(1.0, 2.0, 3.0));
    V1.AppendItem(Vector3Single(4.0, 5.0, 6.0));
    V1.AppendItem(Vector3Single(7.0, 8.0, 9.0));

    V2.AppendItem(Vector3Single(6.0, 6.0, 6.0));
    V2.AppendDynArray(V1);
    V2.AppendItem(Vector3Single(6.0, 6.0, 6.0));

    Assert(VectorsPerfectlyEqual(V1.Items[0], V2.Items[1]));
    Assert(VectorsPerfectlyEqual(V1.Items[1], V2.Items[2]));
    Assert(VectorsPerfectlyEqual(V1.Items[2], V2.Items[3]));

    V2.AppendDynArray(V1, 1, 1);
    Assert(V2.Count = 6);
    Assert(VectorsPerfectlyEqual(V1.Items[1], V2.Items[5]));
  finally
    FreeAndNil(V1);
    FreeAndNil(V2);
  end;
end;

procedure TTestDynArrays.TestDynArraysAssignLerp;
var
  V1, V2, V3: TDynVector3SingleArray;
begin
  V1 := TDynVector3SingleArray.Create;
  V2 := TDynVector3SingleArray.Create;
  V3 := TDynVector3SingleArray.Create;
  try
    V1.AppendItem(Vector3Single(1.0, 2.0, 3.0));
    V1.AppendItem(Vector3Single(4.0, 5.0, 6.0));

    V2.AppendItem(Vector3Single(7.0, 8.0, 9.0));
    V2.AppendItem(Vector3Single(11.0, 12.0, 13.0));
    V2.AppendItem(Vector3Single(17.0, 18.0, 19.0));

    V3.AssignLerp(0.2, V1, V2, 0, 1, 2);
    Assert(V3.Count = 2);

    Assert(VectorsPerfectlyEqual(V3.Items[0], Lerp(0.2,
      Vector3Single(1.0, 2.0, 3.0),
      Vector3Single(11.0, 12.0, 13.0))));
    Assert(VectorsPerfectlyEqual(V3.Items[1], Lerp(0.2,
      Vector3Single(4.0, 5.0, 6.0),
      Vector3Single(17.0, 18.0, 19.0))));
  finally
    FreeAndNil(V1);
    FreeAndNil(V2);
    FreeAndNil(V3);
  end;
end;

initialization
 RegisterTest(TTestDynArrays);
end.
