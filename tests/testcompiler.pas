{
  Copyright 2017-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCompiler;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestCompiler = class(TTestCase)
    procedure TestIs;
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

initialization
  RegisterTest(TTestCompiler);
end.
