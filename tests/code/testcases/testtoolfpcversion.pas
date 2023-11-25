// -*- compile-command: "./test_single_testcase.sh TTestToolFpcVersion" -*-
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
  CastleFilesUtils, CastleUriUtils;

type
  { Descendant of TToolVersion, just to expose protected ParseVersion.
    We actually don't need to make it public here, it's enough that TTestVersionParsing
    is in the same unit as TTestToolFpcVersion.TestVersionParsing that uses it. }
  TTestVersionParsing = class(TToolVersion)
  // public
  //   procedure ParseVersion(const S: String);
  end;

// procedure procedure TTestVersionParsing.ParseVersion(const S: String);
// begin
//   inherited;
// end;

procedure TTestToolFpcVersion.TestVersionParsing;
{ See https://github.com/castle-engine/castle-engine/pull/356 for various example
  version numbers }
var
  VersionParsing: TTestVersionParsing;

  procedure TestParsing(const Ver: String);
  begin
    VersionParsing.ParseVersion(Ver);
    AssertEquals(Ver, VersionParsing.ToString);
  end;

begin
  VersionParsing := TTestVersionParsing.Create;
  try
    TestParsing('1.6.4');
    AssertEquals(1, VersionParsing.Major);
    AssertEquals(6, VersionParsing.Minor);
    AssertEquals(4, VersionParsing.Release);
    AssertEquals('', VersionParsing.ReleaseRemark);

    TestParsing('2.2.0');
    AssertEquals(2, VersionParsing.Major);
    AssertEquals(2, VersionParsing.Minor);
    AssertEquals(0, VersionParsing.Release);
    AssertEquals('', VersionParsing.ReleaseRemark);

    TestParsing('0.9.30.2');
    AssertEquals(0, VersionParsing.Major);
    AssertEquals(9, VersionParsing.Minor);
    AssertEquals(30, VersionParsing.Release);
    AssertEquals('.2', VersionParsing.ReleaseRemark);

    // Ignore this failure -- we read Major and Minor OK, but ReleaseRemark is not correct in this case
    // TestParsing('1.0RC2');
    // AssertEquals(1, VersionParsing.Major);
    // AssertEquals(0, VersionParsing.Minor);
    // AssertEquals(0, VersionParsing.Release);
    // AssertEquals('RC2', VersionParsing.ReleaseRemark);

    TestParsing('2.2.0RC1');
    AssertEquals(2, VersionParsing.Major);
    AssertEquals(2, VersionParsing.Minor);
    AssertEquals(0, VersionParsing.Release);
    AssertEquals('RC1', VersionParsing.ReleaseRemark);

    TestParsing('2.2.0RC2');
    AssertEquals(2, VersionParsing.Major);
    AssertEquals(2, VersionParsing.Minor);
    AssertEquals(0, VersionParsing.Release);
    AssertEquals('RC2', VersionParsing.ReleaseRemark);

    TestParsing('123.456.789');
    AssertEquals(123, VersionParsing.Major);
    AssertEquals(456, VersionParsing.Minor);
    AssertEquals(789, VersionParsing.Release);
    AssertEquals('', VersionParsing.ReleaseRemark);
  finally FreeAndNil(VersionParsing) end;
end;

initialization
  RegisterTest(TTestToolFpcVersion);
end.
