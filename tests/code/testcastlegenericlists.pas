{
  Copyright 2011-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleGenericLists;

interface

uses {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry{$else}CastleTester{$endif};

type
  TTestGenericLists = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
  published
    procedure TestList;
    procedure TestMap;
  end;

implementation

uses SysUtils, Classes,
  CastleGenericLists;

{ Do not put these as local types inside TestList, it makes FPC 2.6.4
  fail with Access Violation. (Works OK with FPC 3.0.2.) }
type
  TMyRecord = record S: string; Int: Integer; end;
  TMyRecordList = specialize TGenericStructList<TMyRecord>;

procedure TTestGenericLists.TestList;
var
  L: TMyRecordList;
  R: TMyRecord;
begin
  L := TMyRecordList.Create;
  try
    R.S := 'blah';
    R.Int := 234;
    L.Add(R);

    R.S := 'foo';
    R.Int := 666;
    L.Add(R);

    AssertEquals(2, L.Count);
    AssertEquals('foo', L[1].S);
    AssertEquals(666, L[1].Int);
    AssertEquals('blah', L[0].S);
    AssertEquals(234, L[0].Int);

    // This should not even compile
    // L[1].Int := 44;

    L.List^[1].Int := 44;
    AssertEquals(44, L[1].Int);
    AssertEquals('foo', L[1].S); // L[1].S unchaned

    L.L[1].Int := 789;
    AssertEquals(789, L[1].Int);
    AssertEquals('foo', L[1].S); // L[1].S unchaned
  finally FreeAndNil(L) end;
end;

type
  TObjectToStringMap = specialize TGenericStructMap<TObject, string>;

procedure TTestGenericLists.TestMap;
var
  Map: TObjectToStringMap;
  O1, O2: TObject;
begin
  O1 := nil;
  O2 := nil;
  Map := nil;
  try
    Map := TObjectToStringMap.Create;
    O1 := TObject.Create;
    O2 := TObject.Create;

    AssertTrue(Map.IndexOf(O1) = -1);
    AssertTrue(Map.IndexOf(O2) = -1);

    Map.Add(O1, 'blah 1');
    AssertTrue(Map.IndexOf(O1) = 0);
    AssertTrue(Map.IndexOf(O2) = -1);
    AssertTrue(Map[O1] = 'blah 1');

    Map[O2] := 'blah 2';
    Map[O1] := 'new blah 1';
    AssertTrue(Map.IndexOf(O1) <> -1);
    AssertTrue(Map.IndexOf(O2) <> -1);
    AssertTrue(Map[O1] = 'new blah 1');
    AssertTrue(Map[O2] = 'blah 2');
  finally
    FreeAndNil(Map);
    FreeAndNil(O1);
    FreeAndNil(O2);
  end;
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestGenericLists);
{$endif}
end.
