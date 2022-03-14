// -*- compile-command: "./test_single_testcase.sh TTestDownload" -*-
{
  Copyright 2020-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleDownload unit. }
unit TestCastleDownload;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestDownload = class(TCastleTestCase)
  published
    procedure TestLocalChars;
  end;

implementation

uses CastleDownload, CastleClassUtils;

procedure TTestDownload.TestLocalChars;

  procedure TestReading(const URL: String);
  var
    Stream: TStream;
    S: String;
  begin
    Stream := Download(URL);
    try
      S := StreamToString(Stream);
      AssertEquals('Testing.', Trim(S));
    finally FreeAndNil(Stream) end;
  end;

begin
  TestReading('castle-data:/local_chars/ascii_name.txt');
  TestReading('castle-data:/local_chars/name with Polish chars ćma źrebak żmija wąż królik.txt');
  TestReading('castle-data:/local_chars/name with Chinese chars 样例中文文本.txt');
  TestReading('castle-data:/local_chars/样例中文文本/name with Chinese chars 样例中文文本.txt');
end;

initialization
  RegisterTest(TTestDownload);
end.
