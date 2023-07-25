// -*- compile-command: "./test_single_testcase.sh TTestBasicLists" -*-
{
  Copyright 2004-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test lists in CastleUtils unit. }
unit TestCastleUtilsLists;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry
  {$else}CastleTester{$endif};

type
  TTestBasicLists = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
    procedure TestSetCountItems;
    procedure TestPrimitiveLists;
    procedure TestCastleVectorsLists;
    procedure TestListsAssign;
    procedure TestListsAssignLerp;
    procedure TestListsAssignLerpShifted;
    procedure TestZero;
    procedure TestSortAndRemoveDuplicates;
  end;

implementation

uses CastleUtils, CastleVectors;

function IsSmallerString(const a, b: string): Integer;
begin
  if A < B then
    Result := -1 else
  if A > B then
    Result :=  1 else
    Result :=  0;
end;

procedure TTestBasicLists.TestSetCountItems;
var
  iarr: TIntegerList;
begin
  iarr := TIntegerList.Create;
  try
    AssertTrue(iarr.Count = 0);

    { growing count works, and sets Items to non-nil }
    iarr.Count := 3;
    AssertTrue(iarr.Count = 3);
    AssertTrue(iarr.L <> nil);

    { growing count doesn't change previous values }
    iarr[2] := 123;
    iarr.Count := 1000000;
    AssertTrue(iarr[2] = 123);
  finally FreeAndNil(iarr) end;
end;

(*
procedure TTestBasicLists.TestGenericStringList;

  function Equal(const S1: TGenericStringList; const S2: array of string): boolean;
  var
    I: Integer;
  begin
    Result := S1.Count = High(S2) + 1;
    if Result then
    begin
      for I := 0 to S1.Count - 1 do
        if S1[I] <> S2[I] then
          Exit(false);
      Result := true;
    end;
  end;

  procedure Reverse(S: TGenericStringList);
  var
    I: Integer;
  begin
    { Need to specially check for Count = 0 case, since (0-1) div 2 = -1 div 2 = 0
      which means that loop would try invalid Exchange(0, -1). }
    if S.Count = 0 then Exit;
    for I := 0 to (S.Count - 1) div 2 do
      S.Exchange(I, S.Count - 1 - I);
  end;

var
  sarr, sarr2: TGenericStringList;
  i, j: integer;
const
  twoStrings: array[0..1]of string = ('raz','dwa');
begin
 for i := 1 to 100 do
 begin
  sarr := TGenericStringList.Create;
  try
   sarr.Capacity := Random(8);
   sarr.Count := 4;
   sarr[0] := 'FOO';
   sarr[1] := 'foo bar xyz';
   sarr.Delete(0);
   sarr.AddRange(twoStrings);
   sarr.Add('trzy?');

   AssertTrue(not Equal(sarr, ['foo bar xyz', '', '']));
   AssertTrue(Equal(sarr, ['foo bar xyz', '', '', 'raz', 'dwa', 'trzy?']));

   Reverse(sarr);
   AssertTrue(Equal(sarr, ['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));

   sarr2 := TGenericStringList.Create;
   try
    sarr2.Add('blah');
    AssertTrue(Equal(sarr2, ['blah']));
    sarr2.Assign(sarr);
    AssertTrue(Equal(sarr2, ['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));

    {sortuj ustalone 6 stringow}
    sarr.Sort(@IsSmallerString);
    AssertTrue(Equal(sarr, ['', '', 'dwa', 'foo bar xyz', 'raz', 'trzy?']));

    { sprawdz ze kolejnosc na sarr2 pozostala niezmieniona }
    AssertTrue(Equal(sarr2, ['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));
   finally sarr2.Free end;

   {dodaj losowe stringi, sortuj, sprawdz}
   for j := 0 to 20 do
    sarr.Add( Chr(Random(256)) + Chr(Random(256)) + Chr(Random(256)) );
   sarr.Sort(@IsSmallerString);
   for j := 0 to sarr.Count-2 do AssertTrue(sarr[j] <= sarr[j+1]);

  finally sarr.Free end;
 end;

 sarr := TGenericStringList.Create;
 try
  { na tablicy o 0 liczbie elementow tez wszystko powinno isc ok }
  AssertTrue(sarr.Count = 0);
  Reverse(sarr);
  AssertTrue(sarr.Count = 0);
 finally sarr.Free end;
end;
*)

procedure TTestBasicLists.TestPrimitiveLists;

  function Equal(const S1: TIntegerList; const S2: array of Integer): boolean; overload;
  var
    I: Integer;
  begin
    Result := S1.Count = High(S2) + 1;
    if Result then
    begin
      for I := 0 to S1.Count - 1 do
        if S1[I] <> S2[I] then
          Exit(false);
      Result := true;
    end;
  end;

  function Equal(const S1, S2: TIntegerList): boolean; overload;
  var
    I: Integer;
  begin
    Result := S1.Count = S2.Count;
    if Result then
    begin
      for I := 0 to S1.Count - 1 do
        if S1[I] <> S2[I] then
          Exit(false);
      Result := true;
    end;
  end;

var
  iarr, iarr2: TIntegerList;
begin
 iarr := nil;
 iarr2 := nil;
 try
  iarr := TIntegerList.Create;
  iarr2 := TIntegerList.Create;
  AssertTrue(Equal(iarr, iarr2));
  AssertTrue(iarr.Sum = 0);
  AssertTrue(iarr2.Sum = 0);

  iarr.AddRange([1, 3, 6, 8]);
  AssertTrue(not Equal(iarr, iarr2));
  AssertTrue(iarr.Sum = 1 + 3 + 6 + 8);
  iarr2.AddRange([1, 3, 6, 8]);
  AssertTrue(Equal(iarr, iarr2));
  iarr2.Insert(0, 99);
  AssertTrue(not Equal(iarr, iarr2));
  AssertTrue(iarr2.Sum = iarr.Sum + 99);
  AssertTrue(Equal(iarr2, [99, 1, 3, 6, 8]));

  iarr.Assign(iarr2);
  AssertTrue(Equal(iarr, iarr2));
  AssertTrue(Equal(iarr, [99, 1, 3, 6, 8]));

  { simple DeleteDuplicates tests }

  { DeleteDuplicates removed, since not used, and not avail in FGL.

procedure DeleteDuplicates;
var
  I, Index: integer;
begin
  I := 0;
  while I < Count do
  begin
    Index := I + 1;
    repeat
      Index := IndexOf(Items[I], Index);
      if Index = -1 then Break;
      Delete(Index);
    until false;

    Inc(I);
  end;
end;
}

{  IArr.DeleteDuplicates;
  AssertTrue(iarr.Equal([99, 1, 3, 6, 8]));
  IArr.Insert(0, 3);
  IArr.DeleteDuplicates;
  AssertTrue(iarr.Equal([3, 99, 1, 6, 8]));
  IArr.Count := 0;
  IArr.DeleteDuplicates;
  AssertTrue(iarr.Equal([]));}
 finally
  iarr.Free;
  iarr2.Free;
 end;
end;

procedure TTestBasicLists.TestCastleVectorsLists;
var
  vecs: TVector3List;
begin
  vecs := TVector3List.Create;
  try
    vecs.Add(Vector3(1.0, 2.0, 3.0));
    vecs.Add(Vector3(4.0, 5.0, 6.0));
    vecs.Add(Vector3(1.0, 2.0, 3.0));
    AssertTrue(    TVector3.PerfectlyEquals(vecs.L[0], vecs.L[2]));
    AssertTrue(not TVector3.PerfectlyEquals(vecs.L[0], vecs.L[1]));
    AssertTrue(not TVector3.PerfectlyEquals(vecs.L[2], vecs.L[1]));
  finally FreeAndNil(vecs) end;
end;

procedure TTestBasicLists.TestListsAssign;
var
  V1, V2: TVector3List;
begin
  V1 := TVector3List.Create;
  V2 := TVector3List.Create;
  try
    V1.Add(Vector3(1.0, 2.0, 3.0));
    V1.Add(Vector3(4.0, 5.0, 6.0));
    V1.Add(Vector3(7.0, 8.0, 9.0));

    V2.Add(Vector3(6.0, 6.0, 6.0));
    V2.AddRange(V1);
    V2.Add(Vector3(6.0, 6.0, 6.0));

    AssertTrue(TVector3.PerfectlyEquals(V1.L[0], V2.L[1]));
    AssertTrue(TVector3.PerfectlyEquals(V1.L[1], V2.L[2]));
    AssertTrue(TVector3.PerfectlyEquals(V1.L[2], V2.L[3]));

    V2.AddSubRange(V1, 1, 1);
    AssertTrue(V2.Count = 6);
    AssertTrue(TVector3.PerfectlyEquals(V1.L[1], V2.L[5]));
  finally
    FreeAndNil(V1);
    FreeAndNil(V2);
  end;
end;

procedure TTestBasicLists.TestListsAssignLerpShifted;
var
  V1, V2, V3: TVector3List;
begin
  V1 := TVector3List.Create;
  V2 := TVector3List.Create;
  V3 := TVector3List.Create;
  try
    V1.Add(Vector3(1.0, 2.0, 3.0));
    V1.Add(Vector3(4.0, 5.0, 6.0));

    V2.Add(Vector3(7.0, 8.0, 9.0));
    V2.Add(Vector3(11.0, 12.0, 13.0));
    V2.Add(Vector3(17.0, 18.0, 19.0));

    V3.AssignLerpRange(0.2, V1, V2, 0, 1, 2);
    AssertTrue(V3.Count = 2);

    AssertTrue(TVector3.PerfectlyEquals(V3.L[0], Lerp(0.2,
      Vector3(1.0, 2.0, 3.0),
      Vector3(11.0, 12.0, 13.0))));
    AssertTrue(TVector3.PerfectlyEquals(V3.L[1], Lerp(0.2,
      Vector3(4.0, 5.0, 6.0),
      Vector3(17.0, 18.0, 19.0))));
  finally
    FreeAndNil(V1);
    FreeAndNil(V2);
    FreeAndNil(V3);
  end;
end;

procedure TTestBasicLists.TestListsAssignLerp;
var
  V1, V2, V3: TVector3List;
begin
  V1 := TVector3List.Create;
  V2 := TVector3List.Create;
  V3 := TVector3List.Create;
  try
    V1.Add(Vector3(1.0, 2.0, 3.0));
    V1.Add(Vector3(4.0, 5.0, 6.0));

    V2.Add(Vector3(7.0, 8.0, 9.0));
    V2.Add(Vector3(11.0, 12.0, 13.0));

    V3.AssignLerp(0.2, V1, V2);
    AssertTrue(V3.Count = 2);

    AssertTrue(TVector3.PerfectlyEquals(V3.L[0], Lerp(0.2,
      Vector3(1.0, 2.0, 3.0),
      Vector3(7.0, 8.0, 9.0))));
    AssertTrue(TVector3.PerfectlyEquals(V3.L[1], Lerp(0.2,
      Vector3(4.0, 5.0, 6.0),
      Vector3(11.0, 12.0, 13.0))));
  finally
    FreeAndNil(V1);
    FreeAndNil(V2);
    FreeAndNil(V3);
  end;
end;

procedure TTestBasicLists.TestZero;
var
  B: TBooleanList;
  O: TIntegerList;
  F: TSingleList;
  I: Integer;
begin
  { For lists based on TFPGList, increasing count always initializes new memory
    to zero. }

  B := TBooleanList.Create;
  B.Count := 10;
  for I := 0 to B.Count - 1 do
    AssertTrue(not B[I]);
  FreeAndNil(B);

  O := TIntegerList.Create;
  O.Count := 10;
  for I := 0 to O.Count - 1 do
    AssertTrue(O[I] = 0);
  FreeAndNil(O);

  F := TSingleList.Create;
  F.Count := 10;
  for I := 0 to F.Count - 1 do
    AssertTrue(F[I] = 0);
  FreeAndNil(F);
end;

procedure TTestBasicLists.TestSortAndRemoveDuplicates;
var
  F: TSingleList;
begin
  F := TSingleList.Create;
  F.Count := 3;
  F[0] := 10;
  F[1] := 5;
  F[2] := 7.5;
  F.SortAndRemoveDuplicates;
  AssertEquals(3, F.Count);
  AssertEquals(5, F[0]);
  AssertEquals(7.5, F[1]);
  AssertEquals(10, F[2]);
  FreeAndNil(F);

  F := TSingleList.Create;
  F.Count := 5;
  F[0] := 10;
  F[1] := 5;
  F[2] := 7.5;
  F[3] := 1;
  F[4] := 7.5;
  F.SortAndRemoveDuplicates;
  AssertEquals(4, F.Count);
  AssertEquals(1, F[0]);
  AssertEquals(5, F[1]);
  AssertEquals(7.5, F[2]);
  AssertEquals(10, F[3]);
  FreeAndNil(F);

  F := TSingleList.Create;
  F.Count := 5;
  F[0] := 10;
  F[1] := 5;
  F[2] := 7.5;
  F[3] := 1;
  F[4] := 10;
  F.SortAndRemoveDuplicates;
  AssertEquals(4, F.Count);
  AssertEquals(1, F[0]);
  AssertEquals(5, F[1]);
  AssertEquals(7.5, F[2]);
  AssertEquals(10, F[3]);
  FreeAndNil(F);

  F := TSingleList.Create;
  F.Count := 7;
  F[0] := 10;
  F[1] := 5;
  F[2] := 7.5;
  F[3] := 1;
  F[4] := 5;
  F[5] := 5;
  F[6] := 5;
  F.SortAndRemoveDuplicates;
  AssertEquals(4, F.Count);
  AssertEquals(1, F[0]);
  AssertEquals(5, F[1]);
  AssertEquals(7.5, F[2]);
  AssertEquals(10, F[3]);
  FreeAndNil(F);
end;

initialization
 RegisterTest(TTestBasicLists);
end.
