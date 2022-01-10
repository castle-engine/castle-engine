// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestGenericsCollections" -*-
{
  Copyright 2017-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test Generics.Collections unit. These tests are independent from CGE. }
unit TestGenericsCollections;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  {$else}CastleTester,{$endif} Generics.Collections;

type
  TTestGenericsCollections = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
    procedure Test1;
    procedure TestFreeingManually;
    procedure TestAddingLists;
    procedure TestSort;
    procedure TestPack;
    procedure TestMapTryGetValue;
    procedure TestRecordsList;
    procedure TestVectorsList;
    procedure TestMethodsList;
    procedure TestTryGetValueNil;
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
  TAppleList = class({$ifdef FPC}specialize{$endif} TObjectList<TApple>)
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

function CompareApples({$ifdef FPC}constref{$else}const{$endif} Left, Right: TApple): Integer;
begin
  Result := AnsiCompareStr(Left.Name, Right.Name);
end;

procedure TTestGenericsCollections.TestSort;
type
  TAppleComparer = {$ifdef FPC}specialize{$endif} TComparer<TApple>;
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

    L.Sort(TAppleComparer.Construct({$ifdef FPC}@{$endif}CompareApples));

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
  TAppleDictionary = {$ifdef FPC}specialize{$endif} TDictionary<string, TApple>;

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

procedure TTestGenericsCollections.TestRecordsList;
type
  TMyRecord = packed record
    A, B: Integer;
  end;
  TMyRecordList = {$ifdef FPC}specialize{$endif} TList<TMyRecord>;
var
  List: TMyRecordList;
  R1, R2, R: TMyRecord;
begin
  List := TMyRecordList.Create;
  try
    R1.A := 11;
    R1.B := 22;
    List.Add(R1);

    R2.A := 33;
    R2.B := 44;
    List.Add(R2);

    R2.A := 33;
    R2.B := 44;
    List.Add(R2);

    AssertEquals(3, List.Count);
    AssertEquals(11, List[0].A);
    AssertEquals(22, List[0].B);
    AssertEquals(33, List[1].A);
    AssertEquals(44, List[1].B);
    AssertEquals(33, List[2].A);
    AssertEquals(44, List[2].B);

    List.Delete(2);

    AssertEquals(2, List.Count);
    AssertEquals(11, List[0].A);
    AssertEquals(22, List[0].B);
    AssertEquals(33, List[1].A);
    AssertEquals(44, List[1].B);

    AssertEquals(0, List.IndexOf(R1));
    AssertEquals(1, List.IndexOf(R2));

    // change R1 and R2, to make sure it doesn't matter for tests
    R1.A := 111111;
    R1.B := 222222;
    R2.A := 333333;
    R2.B := 444444;
    AssertEquals(-1, List.IndexOf(R1));
    AssertEquals(-1, List.IndexOf(R2));

    R.A := 11;
    R.B := 22;
    AssertEquals(0, List.IndexOf(R));

    R.A := 33;
    R.B := 44;
    AssertEquals(1, List.IndexOf(R));

    R.A := 11;
    R.B := 22;
    List.Remove(R);
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0].A);
    AssertEquals(44, List[0].B);

    R.A := 666;
    R.B := 22;
    List.Remove(R); // does nothing, no such record
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0].A);
    AssertEquals(44, List[0].B);
  finally FreeAndNil(List) end;
end;

procedure TTestGenericsCollections.TestVectorsList;
type
  TMyVector = packed array [0..1] of Single;
  TMyVectorList = {$ifdef FPC}specialize{$endif} TList<TMyVector>;
var
  List: TMyVectorList;
  R1, R2, R: TMyVector;
begin
  List := TMyVectorList.Create;
  try
    R1[0] := 11;
    R1[1] := 22;
    List.Add(R1);

    R2[0] := 33;
    R2[1] := 44;
    List.Add(R2);

    R2[0] := 33;
    R2[1] := 44;
    List.Add(R2);

    AssertEquals(3, List.Count);
    AssertEquals(11, List[0][0]);
    AssertEquals(22, List[0][1]);
    AssertEquals(33, List[1][0]);
    AssertEquals(44, List[1][1]);
    AssertEquals(33, List[2][0]);
    AssertEquals(44, List[2][1]);

    List.Delete(2);

    AssertEquals(2, List.Count);
    AssertEquals(11, List[0][0]);
    AssertEquals(22, List[0][1]);
    AssertEquals(33, List[1][0]);
    AssertEquals(44, List[1][1]);

    AssertEquals(0, List.IndexOf(R1));
    AssertEquals(1, List.IndexOf(R2));

    // change R1 and R2, to make sure it doesn't matter for tests
    R1[0] := 111111;
    R1[1] := 222222;
    R2[0] := 333333;
    R2[1] := 444444;
    AssertEquals(-1, List.IndexOf(R1));
    AssertEquals(-1, List.IndexOf(R2));

    R[0] := 11;
    R[1] := 22;
    AssertEquals(0, List.IndexOf(R));

    R[0] := 33;
    R[1] := 44;
    AssertEquals(1, List.IndexOf(R));

    R[0] := 11;
    R[1] := 22;
    List.Remove(R);
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0][0]);
    AssertEquals(44, List[0][1]);

    R[0] := 666;
    R[1] := 22;
    List.Remove(R); // does nothing, no such item
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0][0]);
    AssertEquals(44, List[0][1]);
  finally FreeAndNil(List) end;
end;

type
  TSomeClass = class
    procedure Foo(A: Integer);
  end;

procedure TSomeClass.Foo(A: Integer);
begin
end;

procedure TTestGenericsCollections.TestMethodsList;
type
  TMyMethod = procedure (A: Integer) of object;
  TMyMethodList = {$ifdef FPC}specialize{$endif} TList<TMyMethod>;

  procedure AssertMethodsEqual(const M1, M2: TMyMethod);
  begin
    AssertTrue(TMethod(M1).Code = TMethod(M2).Code);
    AssertTrue(TMethod(M1).Data = TMethod(M2).Data);
  end;

var
  List: TMyMethodList;
  C1, C2, C3: TSomeClass;
  M: TMyMethod;
begin
  C1 := TSomeClass.Create;
  C2 := TSomeClass.Create;
  C3 := TSomeClass.Create;

  List := TMyMethodList.Create;
  try
    List.Add({$ifdef FPC}@{$endif}C1.Foo);
    List.Add({$ifdef FPC}@{$endif}C2.Foo);
    List.Add({$ifdef FPC}@{$endif}C2.Foo);

    AssertEquals(3, List.Count);
    M := {$ifdef FPC}@{$endif}C1.Foo;
    AssertMethodsEqual(List[0], M);
    M := {$ifdef FPC}@{$endif}C2.Foo;
    AssertMethodsEqual(List[1], M);
    AssertMethodsEqual(List[2], M);

    List.Delete(2);

    AssertEquals(2, List.Count);
    M := {$ifdef FPC}@{$endif}C1.Foo;
    AssertMethodsEqual(List[0], M);
    M := {$ifdef FPC}@{$endif}C2.Foo;
    AssertMethodsEqual(List[1], M);

    AssertEquals(0, List.IndexOf({$ifdef FPC}@{$endif}C1.Foo));
    AssertEquals(1, List.IndexOf({$ifdef FPC}@{$endif}C2.Foo));

    AssertEquals(-1, List.IndexOf({$ifdef FPC}@{$endif}C3.Foo));

    List.Remove({$ifdef FPC}@{$endif}C1.Foo);
    AssertEquals(1, List.Count);
    M := {$ifdef FPC}@{$endif}C2.Foo;
    AssertMethodsEqual(List[0], M);

    List.Remove({$ifdef FPC}@{$endif}C3.Foo); // does nothing, no such item
    AssertEquals(1, List.Count);
    M := {$ifdef FPC}@{$endif}C2.Foo;
    AssertMethodsEqual(List[0], M);
  finally FreeAndNil(List) end;

  C1.Free;
  C2.Free;
  C3.Free;
end;

type
  TStringStringMap = class({$ifdef FPC}specialize{$endif} TDictionary<string, string>)
  end;

procedure TTestGenericsCollections.TestTryGetValueNil;
var
  Map: TStringStringMap;
  V: String;
  B: Boolean;
begin
  Map := TStringStringMap.Create;
  try
    Map.AddOrSetValue('some key', 'some value');

    B := Map.TryGetValue('some key', V);
    AssertTrue(B);
    AssertEquals('some value', V);

    B := Map.TryGetValue('some other key', V);
    AssertFalse(B);

    B := Map.TryGetValue('', V);
    AssertFalse(B);
  finally FreeAndNil(Map) end;
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestGenericsCollections);
{$endif}
end.
