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

uses
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry;

type
  TTestCompiler = class(TTestCase)
    procedure TestIs;
    procedure TestSinglePrecision;
  end;

implementation

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

initialization
  RegisterTest(TTestCompiler);
end.
