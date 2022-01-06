// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestSysUtils" -*-
{
  Copyright 2015-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestToolFpcVersion;

interface

uses
  ToolFpcVersion,
  CastleTestCase, TestRegistry;

type
  TTestToolFpcVersion = class(TCastleTestCase)
    procedure TestVersionParsing;
  end;

implementation

uses CastleFilesUtils, CastleURIUtils;

type
  TTestVersionParsing = class(TToolVersion)
  public
    function TestParsing(const VersionString: String): Boolean;
  end;

function TTestVersionParsing.TestParsing(const VersionString: String): Boolean;
begin
  ParseVersion(VersionString);
  Result := ToString = VersionString;
end;

procedure TTestToolFpcVersion.TestVersionParsing;
var
  VersionParsing: TTestVersionParsing;
begin
  VersionParsing := TTestVersionParsing.Create;
  AssertTrue(VersionParsing.TestParsing('2.2.0RC1'));
  AssertTrue(VersionParsing.TestParsing('2.2.0RC2'));
  VersionParsing.Free;
end;

initialization
  RegisterTest(TTestToolFpcVersion);
end.
