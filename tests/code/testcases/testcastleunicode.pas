// -*- compile-command: "./test_single_testcase.sh TTestUnicode" -*-
{
  Copyright 2020-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleUnicode unit.

  Leave UTF-8 BOM in this file for Delphi!
  To let it interpret the string literals (like using Chinese) in this file correctly. }
unit TestCastleUnicode;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestUnicode = class(TCastleTestCase)
  published
    procedure TestHtmlEntities;
    procedure TestStringOperations;
    procedure TestIterator;
    procedure TestUnicodeCharList;
    procedure TestUnicodeCharToReadableString;
    procedure TestStringEnding;
  end;

implementation

uses CastleUnicode, CastleClassUtils;

procedure TTestUnicode.TestHtmlEntities;
begin
  AssertEquals('ascii_name.txt', StringWithHtmlEntities('ascii_name.txt'));
  AssertEquals('name with Polish chars &#x107;ma &#x17A;rebak &#x17C;mija w&#x105;&#x17C; kr&#xF3;lik.txt', StringWithHtmlEntities('name with Polish chars ćma źrebak żmija wąż królik.txt'));
  AssertEquals('name with Chinese chars &#x6837;&#x4F8B;&#x4E2D;&#x6587;&#x6587;&#x672C;.txt', StringWithHtmlEntities('name with Chinese chars 样例中文文本.txt'));
end;

procedure TTestUnicode.TestStringOperations;
begin
  AssertEquals(18, StringLength('Custom large fonta'));
  // Polish chars below, in UTF-8 some of these chars are multibyte
  AssertEquals(27, StringLength('Ćma źrebak żmija wąż królik'));
  AssertEquals('Custom large font', StringCopy('Custom large fonta', 1, 17));
  AssertEquals('Custom large fonta', StringCopy('Custom large fonta', 1, 100));
end;

procedure TTestUnicode.TestIterator;
var
  I: TCastleStringIterator;
  S: String;
begin
  // empty string
  S := '';
  I.Start(S);
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);

  // string with Polish chars
  S := 'Ćma źrebak';
  I.Start(S);
  AssertTrue(I.GetNext); AssertEquals('Ć', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('m', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals(' ', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('ź', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('r', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('e', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('b', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('k', UnicodeCharToString(I.Current));
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);

  // string with Chinese chars
  S := '样例中文文本';
  I.Start(S);
  AssertTrue(I.GetNext); AssertEquals('样', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('例', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('中', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('文', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('文', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('本', UnicodeCharToString(I.Current));
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);

  // string with ASCII + Chinese chars
  S := '1MyName样';
  I.Start(S);
  AssertTrue(I.GetNext); AssertEquals('1', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('M', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('y', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('N', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('m', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('e', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('样', UnicodeCharToString(I.Current));
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);

  // interrupting the iteration with same string
  S := 'Ćma źrebak';
  I.Start(S);
  AssertTrue(I.GetNext); AssertEquals('Ć', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('m', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals(' ', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('ź', UnicodeCharToString(I.Current));
  I.Start(S);
  AssertTrue(I.GetNext); AssertEquals('Ć', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('m', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals(' ', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('ź', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('r', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('e', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('b', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  I.Start(S); // interrupt iteration again
  I.Start(S);
  I.Start(S);
  AssertTrue(I.GetNext); AssertEquals('Ć', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('m', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals(' ', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('ź', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('r', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('e', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('b', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('k', UnicodeCharToString(I.Current));
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);

  // interrupting the iteration with different string
  S := '样例中文文本';
  I.Start(S);
  AssertTrue(I.GetNext); AssertEquals('样', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('例', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('中', UnicodeCharToString(I.Current));
  S := 'Ćma źrebak';
  I.Start(S);
  AssertTrue(I.GetNext); AssertEquals('Ć', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('m', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals(' ', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('ź', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('r', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('e', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('b', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('a', UnicodeCharToString(I.Current));
  AssertTrue(I.GetNext); AssertEquals('k', UnicodeCharToString(I.Current));
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);
  AssertFalse(I.GetNext);

  // abandon the iteration in the middle, make sure no memory leaks
  S := '样例中文文本';
  I.Start(S);
  AssertTrue(I.GetNext); AssertEquals('样', UnicodeCharToString(I.Current));
end;

procedure TTestUnicode.TestUnicodeCharList;
var
  L: TUnicodeCharList;
begin
  L := TUnicodeCharList.Create;
  try
    L.Add('Ć'); // decimal: 262, https://www.compart.com/en/unicode/U+0106
    L.Add('m');
    L.Add('xyz');
    L.Add('样'); // decimal: 26679, https://www.compart.com/en/unicode/U+6837
    L.Add('例中'); // decimal: 20363, 20013, https://www.compart.com/en/unicode/U+4F8B, https://www.compart.com/en/unicode/U+4E2D
    AssertEquals(8, L.Count);

    AssertFalse(L.Contains(0));
    AssertFalse(L.Contains(Ord('C')));
    AssertFalse(L.Contains(Ord('a')));
    AssertFalse(L.Contains(25991));

    AssertTrue(L.Contains(262)); AssertTrue(L.Contains($106));
    AssertTrue(L.Contains(Ord('m')));
    AssertTrue(L.Contains(Ord('x')));
    AssertTrue(L.Contains(Ord('y')));
    AssertTrue(L.Contains(Ord('z')));
    AssertTrue(L.Contains(26679)); AssertTrue(L.Contains($6837));
    AssertTrue(L.Contains(20363)); AssertTrue(L.Contains($4F8B));
    AssertTrue(L.Contains(20013)); AssertTrue(L.Contains($4E2D));
  finally FreeAndNil(L) end;
end;

procedure TTestUnicode.TestUnicodeCharToReadableString;
begin
  AssertEquals('#13', UnicodeCharToReadableString(13));
  AssertEquals('#0', UnicodeCharToReadableString(0));
  AssertEquals('#27', UnicodeCharToReadableString(27));
  AssertEquals(' ', UnicodeCharToReadableString(Ord(' ')));
  AssertEquals('Ć', UnicodeCharToReadableString(262));
  AssertEquals('m', UnicodeCharToReadableString(Ord('m')));
  AssertEquals('样', UnicodeCharToReadableString(26679));
  AssertEquals('样', UnicodeCharToReadableString($6837));
  AssertEquals('例', UnicodeCharToReadableString(20363));
  AssertEquals('中', UnicodeCharToReadableString(20013));
end;

procedure TTestUnicode.TestStringEnding;
begin
  AssertEquals('a źrebię', StringEnding('Ćma źrebię', 3));
  AssertEquals('', StringEnding('Ćma źrebię', 100));
end;

initialization
  RegisterTest(TTestUnicode);
end.
