{
  Copyright 2011-2017 Michalis Kamburelis.

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

uses fpcunit, testutils, testregistry;

type
  TTestGenericLists = class(TTestCase)
  published
    procedure TestMap;
  end;

implementation

uses SysUtils, Classes, CastleGenericLists;

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

initialization
  RegisterTest(TTestGenericLists);
end.
