// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleFindFiles" -*-
{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleFindFiles unit. }
unit TestCastleFindFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry, CastleTestCase;

type
  TTestCastleFindFiles = class(TCastleTestCase)
  published
    procedure TestNilHandler;
  end;

implementation

uses CastleFindFiles;

procedure TTestCastleFindFiles.TestNilHandler;
begin
  AssertEquals(2, FindFiles('castle-data:/designs/', '*.castle-user-interface', false, nil, [ffRecursive]));
end;

initialization
  RegisterTest(TTestCastleFindFiles);
end.
