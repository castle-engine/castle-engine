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
  Classes, SysUtils, CastleTester;

type
  TTestUnicode = class(TCastleTestCase)
  published
    {$ifdef FPC} // UTF8ToHtmlEntities is now only for FPC
    procedure TestUTF8ToHtmlEntities;
    {$endif}
    procedure TestStringOperations;
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

procedure TTestUnicode.TestStringOperations;
begin
  AssertEquals(18, StringLength('Custom large fonta'));
  // Polish chars below, in UTF-8 some of these chars are multibyte
  AssertEquals(27, StringLength('Ćma źrebak żmija wąż królik'));
  AssertEquals('Custom large font', StringCopy('Custom large fonta', 1, 17));
  AssertEquals('Custom large fonta', StringCopy('Custom large fonta', 1, 100));
end;

initialization
  RegisterTest(TTestUnicode);
end.
