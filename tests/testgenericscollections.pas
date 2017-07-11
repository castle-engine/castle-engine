{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test Generics.Collections unit. }
unit TestGenericsCollections;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Generics.Collections;

type
  TTestGenericsCollections = class(TTestCase)
    procedure Test1;
    procedure TestFreeingManually;
    procedure TestAddingLists;
    procedure TestSort;
    procedure TestPack;
    procedure TestMapTryGetValue;
  end;

implementation

uses Generics.Defaults;

type
  TApple = class
    Name: string;
    procedure Eat;
  end;

procedure TApple.Eat;
begin
  // Writeln('TApple.Eat');
end;

type
  TAppleList = class(specialize TObjectList<TApple>)
    procedure Pack;
  end;

procedure TAppleList.Pack;
begin
  while Remove(nil) <> -1 do ;
end;

procedure TTestGenericsCollections.Test1;
var
  A: TApple;
  Apples: TAppleList;
begin
  Apples := TAppleList.Create(true);
  try
    A := TApple.Create;
    Apples.Add(A);
    Apples.Add(TApple.Create);
    A := TApple.Create;
    Apples.Add(A);

    AssertEquals(3, Apples.Count);
    AssertEquals(2, Apples.IndexOf(A));

    Apples.Delete(0);

    AssertEquals(2, Apples.Count);
    AssertEquals(1, Apples.IndexOf(A));

    Apples.Remove(A);

    AssertEquals(1, Apples.Count);

    Apples.Delete(0);

    AssertEquals(0, Apples.Count);
  finally FreeAndNil(Apples) end;
end;

procedure TTestGenericsCollections.TestFreeingManually;
var
  A: TApple;
  Apples: TAppleList;
begin
  Apples := TAppleList.Create(false);
  try
    A := TApple.Create;
    Apples.Add(A);
    Apples.Add(A);
    Apples.Add(TApple.Create);

    { This freeing would be invalid on a list that owns children,
      as we free something twice, and we leave some invalid references
      (to already freed items) in the list at various stages.
      But it should be OK with list that has OwnsChildren = false. }

    Apples[0].Free;
    Apples[0] := nil;
    Apples[1] := nil;
    Apples[2].Free;
  finally FreeAndNil(Apples) end;
end;

procedure TTestGenericsCollections.TestAddingLists;
var
  A: TApple;
  Apples, Apples2: TAppleList;
begin
  Apples := TAppleList.Create(true);
  try
    A := TApple.Create;
    A.Name := 'One';
    Apples.Add(A);

    A := TApple.Create;
    A.Name := 'Two';
    Apples.Add(A);

    Apples2 := TAppleList.Create(false);
    try
      Apples2.AddRange(Apples);
      Apples2.AddRange(Apples);
      Apples2.AddRange(Apples);
      AssertEquals(6, Apples2.Count);
      AssertEquals('One', Apples2[0].Name);
      AssertEquals('Two', Apples2[1].Name);
      AssertEquals('One', Apples2[2].Name);
      AssertEquals('Two', Apples2[3].Name);
      AssertEquals('One', Apples2[4].Name);
      AssertEquals('Two', Apples2[5].Name);
    finally FreeAndNil(Apples2) end;
  finally FreeAndNil(Apples) end;
end;

function CompareApples(constref Left, Right: TApple): Integer;
begin
  Result := AnsiCompareStr(Left.Name, Right.Name);
end;

procedure TTestGenericsCollections.TestSort;
type
  TAppleComparer = specialize TComparer<TApple>;
var
  A: TApple;
  L: TAppleList;
begin
  L := TAppleList.Create(true);
  try
    A := TApple.Create;
    A.Name := '11';
    L.Add(A);

    A := TApple.Create;
    A.Name := '33';
    L.Add(A);

    A := TApple.Create;
    A.Name := '22';
    L.Add(A);

    L.Sort(TAppleComparer.Construct(@CompareApples));

    AssertEquals(3, L.Count);
    AssertEquals('11', L[0].Name);
    AssertEquals('22', L[1].Name);
    AssertEquals('33', L[2].Name);
  finally FreeAndNil(L) end;
end;

procedure TTestGenericsCollections.TestPack;
var
  A: TApple;
  L: TAppleList;
begin
  L := TAppleList.Create(true);
  try
    L.Add(nil);

    A := TApple.Create;
    A.Name := '11';
    L.Add(A);

    L.Add(nil);

    A := TApple.Create;
    A.Name := '33';
    L.Add(A);

    A := TApple.Create;
    A.Name := '22';
    L.Add(A);

    L.Add(nil);
    L.Add(nil);

    L.Pack;

    AssertEquals(3, L.Count);
    AssertEquals('11', L[0].Name);
    AssertEquals('33', L[1].Name);
    AssertEquals('22', L[2].Name);
  finally FreeAndNil(L) end;
end;

type
  TAppleDictionary = specialize TDictionary<string, TApple>;

procedure TTestGenericsCollections.TestMapTryGetValue;
var
  Apples: TAppleDictionary;
  A, FoundA: TApple;
begin
  Apples := TAppleDictionary.Create;
  try
    Apples.TryGetValue('blah', FoundA);
    AssertTrue(nil = FoundA);

    A := TApple.Create;
    Apples.AddOrSetValue('nope', A);

    Apples.TryGetValue('blah', FoundA);
    AssertTrue(nil = FoundA);

    A := TApple.Create;
    Apples.AddOrSetValue('blah', A);

    Apples.TryGetValue('blah', FoundA);
    AssertTrue(A = FoundA);

    Apples.Remove('blah');

    A.Free;

    Apples.TryGetValue('blah', FoundA);
    AssertTrue(nil = FoundA);

    Apples['nope'].Free;
  finally FreeAndNil(Apples) end;
end;

initialization
  RegisterTest(TTestGenericsCollections);
end.
