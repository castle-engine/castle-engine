// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestToolFpcVersion" -*-
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

uses SysUtils,
  CastleFilesUtils, CastleURIUtils;

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
  try
    AssertTrue(VersionParsing.TestParsing('2.2.0RC1'));
    AssertEquals(2, VersionParsing.Major);
    AssertEquals(2, VersionParsing.Minor);
    AssertEquals(0, VersionParsing.Release);
    AssertEquals('RC1', VersionParsing.ReleaseRemark);

    AssertTrue(VersionParsing.TestParsing('2.2.0RC2'));
    AssertEquals(2, VersionParsing.Major);
    AssertEquals(2, VersionParsing.Minor);
    AssertEquals(0, VersionParsing.Release);
    AssertEquals('RC2', VersionParsing.ReleaseRemark);

    AssertTrue(VersionParsing.TestParsing('123.456.789'));
    AssertEquals(123, VersionParsing.Major);
    AssertEquals(456, VersionParsing.Minor);
    AssertEquals(789, VersionParsing.Release);
    AssertEquals('', VersionParsing.ReleaseRemark);
  finally FreeAndNil(VersionParsing) end;
end;

initialization
  RegisterTest(TTestToolFpcVersion);
end.
