// -*- compile-command: "./test_single_testcase.sh TTestUnicode" -*-
{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleUnicode unit. }
unit TestCastleUnicode;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestUnicode = class(TCastleTestCase)
  published
    {$ifdef FPC} // UTF8ToHtmlEntities is now only for FPC
    procedure TestUTF8ToHtmlEntities;
    {$endif}
  end;

implementation

uses CastleUnicode, CastleClassUtils;

{$ifdef FPC}
procedure TTestUnicode.TestUTF8ToHtmlEntities;
begin
  AssertEquals('ascii_name.txt', UTF8ToHtmlEntities('ascii_name.txt'));
  AssertEquals('name with Polish chars &#x107;ma &#x17A;rebak &#x17C;mija w&#x105;&#x17C; kr&#xF3;lik.txt', UTF8ToHtmlEntities('name with Polish chars ćma źrebak żmija wąż królik.txt'));
  AssertEquals('name with Chinese chars &#x6837;&#x4F8B;&#x4E2D;&#x6587;&#x6587;&#x672C;.txt', UTF8ToHtmlEntities('name with Chinese chars 样例中文文本.txt'));
end;
{$endif}

initialization
  RegisterTest(TTestUnicode);
end.
