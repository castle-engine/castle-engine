// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCompiler" -*-
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

{ Test some Pascal compiler (FPC) stuff. These tests are independent from CGE. }
unit TestCompiler;

interface

{$I castleconf.inc}

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry
  {$else}CastleTester{$endif};

type
  TTestCompiler = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
    procedure TestIs;
    procedure TestSinglePrecision;
    procedure TestSizes;
  end;

implementation

uses CastleUtils;

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

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestCompiler);
{$endif}
end.
