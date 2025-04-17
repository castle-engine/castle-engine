// -*- compile-command: "./test_single_testcase.sh TTestCompiler" -*-
{
  Copyright 2017-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test some Pascal compiler (FPC, Delphi) stuff. These tests are independent from CGE. }
unit TestCompiler;

{ Needed to define EXTENDED_EQUALS_DOUBLE on some platforms/compilers. }
{$I ../../../src/common_includes/castleconf.inc}

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCompiler = class(TCastleTestCase)
    procedure TestIs;
    procedure TestSinglePrecision;
    procedure TestCTypesSizes;
    procedure TestSizes;
    procedure TestPackedOpenArray;
  end;

implementation

uses CTypes, CastleUtils, CastleVectors;

type
  TFruit = class
  end;

  TApple = class(TFruit)
  end;

  TWerewolf = class
  end;

procedure TTestCompiler.TestIs;
var
  O: TObject;
begin
  O := TWerewolf.Create;
  try
    AssertTrue(O is TWerewolf);
    AssertFalse(O is TFruit);
    AssertFalse(O is TApple);
  finally FreeAndNil(O) end;

  O := TApple.Create;
  try
    AssertFalse(O is TWerewolf);
    AssertTrue(O is TFruit);
    AssertTrue(O is TApple);
  finally FreeAndNil(O) end;

  O := nil;
  try
    AssertFalse(O is TWerewolf);
    AssertFalse(O is TFruit);
    AssertFalse(O is TApple);
  finally FreeAndNil(O) end;
end;

procedure TTestCompiler.TestSinglePrecision;
var
  I: Integer;
begin
  for I := -32000 to 32000 do
    AssertEquals(I, Round(Single(I)));
end;

procedure TTestCompiler.TestCTypesSizes;
begin
  AssertEquals(1, SizeOf(CInt8));
  AssertEquals(1, SizeOf(CUInt8));

  AssertEquals(2, SizeOf(CInt16));
  AssertEquals(2, SizeOf(CUInt16));

  AssertEquals(4, SizeOf(CInt32));
  AssertEquals(4, SizeOf(CUInt32));

  AssertEquals(8, SizeOf(CInt64));
  AssertEquals(8, SizeOf(CUInt64));
end;

procedure TTestCompiler.TestSizes;
begin
  AssertEquals(1, SizeOf(Byte));
  AssertEquals(1, SizeOf(ShortInt));

  AssertEquals(2, SizeOf(Word));
  AssertEquals(2, SizeOf(SmallInt));

  AssertEquals(4, SizeOf(Int32));
  AssertEquals(4, SizeOf(UInt32));

  AssertEquals(8, SizeOf(Int64));
  AssertEquals(8, SizeOf(UInt64));
  AssertEquals(8, SizeOf(QWord));

  { Both in FPC and Delphi, Integer/Cardinal remained 4-byte (even though
    in old days the Integer/Cardinal were documented as potentially
    platform-dependent size).
    See (Delphi): https://docwiki.embarcadero.com/RADStudio/Sydney/en/Simple_Types_(Delphi) }
  AssertEquals(4, SizeOf(Integer));
  AssertEquals(4, SizeOf(Cardinal));

  AssertEquals(4, SizeOf(Single));
  AssertEquals(8, SizeOf(Double));

  AssertEquals(
    {$if defined(EXTENDED_EQUALS_DOUBLE)} 8
    {$elseif defined(EXTENDED_EQUALS_LONG_DOUBLE)} 16
    {$else} 10
    {$endif}, SizeOf(Extended));
end;

type
  { TCastleRenderUnlitMesh implementation assumes that "array of UInt16",
    "array of TVector4" parameters are packed arrays.
    But in Delphi we cannot declare them as such.
    So let's check ar runtime they are packed. }
  TCastleRenderUnlitMeshTest = class
  public
    procedure SetIndexes(const Indexes: array of UInt16);
    procedure SetVertexes(const Vertexes: array of TVector4;
      const UsageDynamic: Boolean);
  end;

procedure TCastleRenderUnlitMeshTest.SetIndexes(const Indexes: array of UInt16);
var
  I: Integer;
begin
  {$I norqcheckbegin.inc}
  for I := 0 to 100 do
    Check(PtrUInt(@Indexes[I + 1]) - PtrUInt(@Indexes[I]) = SizeOf(UInt16));
  {$I norqcheckend.inc}
end;

procedure TCastleRenderUnlitMeshTest.SetVertexes(const Vertexes: array of TVector4;
  const UsageDynamic: Boolean);
var
  I: Integer;
begin
  Check(High(Vertexes) = 3);
  Check(TVector4.PerfectlyEquals(Vertexes[0], Vector4(1, 1.2, 1.3, 1.4)));
  {$I norqcheckbegin.inc}
  for I := 0 to 100 do
    Check(PtrUInt(@Vertexes[I + 1]) - PtrUInt(@Vertexes[I]) = SizeOf(TVector4));
  {$I norqcheckend.inc}
end;

procedure TTestCompiler.TestPackedOpenArray;
var
  Mesh: TCastleRenderUnlitMeshTest;
begin
  Mesh := TCastleRenderUnlitMeshTest.Create;
  try
    Mesh.SetIndexes([1, 2, 3, 4]);
    Mesh.SetVertexes([
      Vector4(1, 1.2, 1.3, 1.4),
      Vector4(2, 2.2, 2.3, 2.4),
      Vector4(3, 3.2, 3.3, 3.4),
      Vector4(4, 4.2, 4.3, 4.4)
    ], true);
  finally FreeAndNil(Mesh) end;
end;

initialization
  RegisterTest(TTestCompiler);
end.
