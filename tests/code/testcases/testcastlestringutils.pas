// -*- compile-command: "./test_single_testcase.sh TTestCastleStringUtils" -*-
{
  Copyright 2004-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleStringUtils;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils,
  TestRegistry{$else}CastleTester{$endif};

type
  TTestCastleStringUtils = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
  published
    procedure TestIntToStrBase;
    procedure TestDeFormat;
    procedure TestSReplacePercent;
    procedure TestIntToStr2;
    procedure TestCompressWhiteSpace;
    procedure TestFormatNameCounter;
    procedure TestIntToStr64;
    procedure TestCastleStringList;
    procedure TestCastleStringListNewlinesInside;
    procedure TestSReplacePatterns;
    {$ifdef FPC}procedure TestGetFileFilter;{$endif}
    procedure TestSplitString;
    procedure TestTrimEndingNewline;
    procedure TestAddMultiLine;
    procedure TestRegexpMatches;
  end;

implementation

uses CastleUtils, CastleStringUtils,
  CastleTestUtils;

procedure TTestCastleStringUtils.TestIntToStrBase;
var i: Integer;
    l: Integer;
    s1, s2: string;
begin
 for i := 1 to 100 do
 begin
  { Format('%x', [l]) from FPC fails to work correctly for negative numbers.
    That's an advantage of my IntToStr16. }
  l := Random(High(Integer)){ - High(Integer) div 2};
  s1 := IntToStr16(l);
  s2 := Format('%x', [l]);
  AssertTrue(s1 = s2);
 end;
 AssertTrue(IntToStr16(-17) = '-11');
end;

procedure TTestCastleStringUtils.TestDeFormat;
var
  s, S2: string;
  i: integer;
  f: float;
begin
  DeFormat('123FOO98.2e1 '#9'123ioioio-x    /'+nl, '%dfoo%f %s /',
    [@i, @f, @s], true);
  AssertTrue(i = 123);
  AssertTrue(f = 98.2e1);
  AssertTrue(s = '123ioioio-x');

  { %% test }
  DeFormat('%d%%456foobar %', '%%d%%%%%d%s %%',
    [@i, @s], true);
  AssertTrue(i = 456);
  AssertTrue(s = 'foobar');

  { Test RelatedWhitespaceChecking }
  try
    DeFormat('123  foo', '%d %s %s', [@i, @S, @S2], true, true);
    raise Exception.Create('"DeFormat(123  foo)" with relaxed should fail');
  except
    on EDeformatError do ;
  end;
  DeFormat('123  foo', '%d %s %s', [@i, @S, @S2], true, false);
  AssertTrue(I = 123);
  AssertTrue(S = '');
  AssertTrue(S2 = 'foo');

  { Test %s at the end of data can be '' }
  DeFormat('123 ', '%d %s', [@i, @s], true, true);
  AssertTrue(I = 123);
  AssertTrue(S = '');

  { Similar as above, but last 2 args different.
    Result should be the same. }
  DeFormat('123 ', '%d %s', [@i, @s], false, false);
  AssertTrue(I = 123);
  AssertTrue(S = '');
end;

procedure TTestCastleStringUtils.TestSReplacePercent;
const
  Replaces: array[0..1]of TPercentReplace =
  ((c:'k'; s:'kot'), (c:'p'; s:'pies'));
begin
 {$warnings off} // testing deprecated routine
 AssertTrue( SPercentReplace('bla%kkk%jk%pies', Replaces, false, '%', false)
   = 'blakotkk%jkpiesies');
 try
  SPercentReplace('bla%kkk%jk%pies', Replaces, true, '%', false);
  raise Exception.Create('Last SPercentReplace SHOULD raise exception');
 except on E: EUnknownPercentFormat do AssertTrue(e.Message = 'Unknown format pattern in format "bla%kkk%jk%pies", wrong sequence is : "%j"'); end;

 AssertEquals('blakotkkkotkpiesies', SPercentReplace('bla%kkk%Kk%pies', Replaces, true, '%', true));
 try
  SPercentReplace('bla%kkk%Kk%pies', Replaces, true, '%', false);
  raise Exception.Create('Last SPercentReplace SHOULD raise exception');
 except on E: EUnknownPercentFormat do AssertTrue(e.Message = 'Unknown format pattern in format "bla%kkk%Kk%pies", wrong sequence is : "%K"'); end;

 AssertTrue( SPercentReplace('bla%k%%', Replaces, false, '%', false) = 'blakot%');
 AssertTrue( SPercentReplace('bla%k%%', Replaces, true, '%', false) = 'blakot%');

 AssertTrue( SPercentReplace('foo%', Replaces, false, '%', false) = 'foo%');
 try
  AssertTrue( SPercentReplace('foo%', Replaces, true, '%', false) = 'foo%');
  raise Exception.Create('Last SPercentReplace SHOULD raise exception');
 except on E: EUnknownPercentFormat do AssertTrue(e.Message = 'Unknown format pattern in format "foo%", wrong sequence is : % at the end of the format string'); end;
 {$warnings on}
end;

procedure TTestCastleStringUtils.TestIntToStr2;
var i, Value, MinLength: Integer;
begin
 AssertTrue(IntToStr2(2) = '10');
 AssertTrue(IntToStr2(0) = '0');
 AssertTrue(IntToStr2(2, 4) = '0010');
 AssertTrue(IntToStr2(-2, 4) = '-0010');
 AssertTrue(IntToStr2(0, 4) = '0000');

 AssertTrue(IntToStr2(2, 4, '_', 'M', '+') = '__M_');
 AssertTrue(IntToStr2(-2, 4, '_', 'M', '+') = '+__M_');
 AssertTrue(IntToStr2(0, 4, '_', 'M', '+') = '____');

 for i := 1 to 100 do
 begin
  Value := Integer(Random(10000)) - 10000 div 2;
  MinLength := Random(5);
  AssertTrue(IntToStrBase(Value, 2, MinLength) = IntToStr2(Value, MinLength));
 end;
end;

procedure TTestCastleStringUtils.TestCompressWhiteSpace;
begin
  AssertTrue(SCompressWhiteSpace('') = '');
  AssertTrue(SCompressWhiteSpace('a') = 'a');
  AssertTrue(SCompressWhiteSpace(' ') = ' ');
  AssertTrue(SCompressWhiteSpace('     ') = ' ');
  AssertTrue(SCompressWhiteSpace(' blah blah ') = ' blah blah ');
  AssertTrue(SCompressWhiteSpace('   blah  ' + CharTab + 'blah ' + NL) = ' blah blah ');
end;

procedure TTestCastleStringUtils.TestFormatNameCounter;
var
  ReplacementsDone: Cardinal;
  AllowOldPercentSyntax: boolean;
begin
  AssertTrue(FormatNameCounter('', 0, true, ReplacementsDone) = '');
  AssertTrue(FormatNameCounter('a', 0, true, ReplacementsDone) = 'a');
  AssertTrue(FormatNameCounter('a%', 0, true, ReplacementsDone) = 'a%');
  AssertTrue(FormatNameCounter('%a%', 66, true, ReplacementsDone) = '%a%');
  AssertTrue(FormatNameCounter('%d%', 66, true, ReplacementsDone) = '66%');
  AssertTrue(FormatNameCounter('%%%', 66, true, ReplacementsDone) = '%%');
  AssertTrue(FormatNameCounter('%%number%d%d.again%d', 66, true, ReplacementsDone) = '%number6666.again66');
  AssertTrue(FormatNameCounter('%%number%0d%2d.again%4d', 66, true, ReplacementsDone) = '%number6666.again0066');

  AssertTrue(FormatNameCounter('', 0, false, ReplacementsDone) = '');
  AssertTrue(FormatNameCounter('a', 0, false, ReplacementsDone) = 'a');
  AssertTrue(FormatNameCounter('a%', 0, false, ReplacementsDone) = 'a%');
  AssertTrue(FormatNameCounter('%a%', 66, false, ReplacementsDone) = '%a%');
  AssertTrue(FormatNameCounter('%d%', 66, false, ReplacementsDone) = '%d%');
  AssertTrue(FormatNameCounter('%%%', 66, false, ReplacementsDone) = '%%%');
  AssertTrue(FormatNameCounter('%%number%d%d.again%d', 66, false, ReplacementsDone) = '%%number%d%d.again%d');
  AssertTrue(FormatNameCounter('%%number%0d%2d.again%4d', 66, false, ReplacementsDone) = '%%number%0d%2d.again%4d');

  { assertions below should work for both AllowOldPercentSyntax values }
  for AllowOldPercentSyntax := false to true do
  begin
    AssertTrue(FormatNameCounter('', 0, AllowOldPercentSyntax, ReplacementsDone) = '');
    AssertTrue(FormatNameCounter('a', 0, AllowOldPercentSyntax, ReplacementsDone) = 'a');
    AssertTrue(FormatNameCounter('%again@counter(1)', 66, AllowOldPercentSyntax, ReplacementsDone) = '%again66');
    AssertTrue(FormatNameCounter('%%again@counter(1)', 66, AllowOldPercentSyntax, ReplacementsDone) = '%%again66');
    AssertTrue(FormatNameCounter('%%again@counter(4)', 66, AllowOldPercentSyntax, ReplacementsDone) = '%%again0066');
  end;
end;

procedure TTestCastleStringUtils.TestIntToStr64;
const
  A1: QWord = $ABCDEF123;
  A2: Int64 = $ABCDEF123;
  A3: Int64 = -$ABCDEF123;
  A4: QWord = $0123456789ABCDEF;
var
  A5, A6: QWord;
begin
  AssertTrue(IntToStr16(A1) = 'ABCDEF123');
  AssertTrue(IntToStr16(A2) = 'ABCDEF123');
  AssertTrue(IntToStr16(A3) = '-ABCDEF123');

  AssertTrue(IntToStr16(A4) = '123456789ABCDEF');

  A5 := QWord($EFCDAB8967452301);
  AssertTrue(IntToStr16(A5) = 'EFCDAB8967452301');

  A6 := QWord($FFEE000000000000);
  AssertTrue(IntToStr16(A6) = 'FFEE000000000000');
end;

procedure TTestCastleStringUtils.TestCastleStringList;

{ Useful to debug state in the middle:
  procedure WritelnList(const S: TCastleStringList);
  var
    I: Integer;
  begin
    Writeln(S.Count);
    for I := 0 to S.Count - 1 do
      Writeln(Format('%4d: %s', [I, S[I]]));
  end;
}

var sarr, sarr2: TCastleStringList;
    i, j: integer;
const twoStrings: array[0..1]of string = ('raz','dwa');
begin
 for i := 1 to 100 do
 begin
  sarr := TCastleStringList.Create;
  try
   sarr.Count := 4;
   AssertTrue(sarr.Count = 4);
   sarr[0] := 'FOO';
   sarr[1] := 'foo bar xyz';
   sarr.Delete(0);
   sarr.AddRange(twoStrings);
   sarr.Add('trzy?');

   AssertTrue(not sarr.Equals(['foo bar xyz', '', '']));
   AssertTrue(sarr.Equals(['foo bar xyz', '', '', 'raz', 'dwa', 'trzy?']));

   sarr.Reverse;
   AssertTrue(sarr.Equals(['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));

   sarr2 := TCastleStringList.Create;
   try
    sarr2.Add('blah');
    AssertTrue(sarr2.Equals(['blah']));
    sarr2.Assign(sarr);
    AssertTrue(sarr2.Equals(['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));

    {sortuj ustalone 6 stringow}
    sarr.Sort;
    AssertTrue(sarr.Equals(['', '', 'dwa', 'foo bar xyz', 'raz', 'trzy?']));

    { sprawdz ze kolejnosc na sarr2 pozostala niezmieniona }
    AssertTrue(sarr2.Equals(['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));
   finally sarr2.Free end;

   {dodaj losowe stringi, sortuj, sprawdz}
   for j := 0 to 20 do
    sarr.Add( Chr(Random(256)) + Chr(Random(256)) + Chr(Random(256)) );
   sarr.Sort;
   for j := 0 to sarr.Count-2 do AssertTrue(
     { sarr[j] <= sarr[j+1] }
     { Sort may use AnsiCompareStr that takes into account locale }
     AnsiCompareStr(sarr[j], sarr[j+1]) <= 0);

  finally sarr.Free end;
 end;

 sarr := TCastleStringList.Create;
 try
  { na tablicy o 0 liczbie elementow tez wszystko powinno isc ok }
  AssertTrue(sarr.Count = 0);
  sarr.Reverse;
  AssertTrue(sarr.Count = 0);
 finally sarr.Free end;
end;

procedure TTestCastleStringUtils.TestCastleStringListNewlinesInside;
var
  SL: TCastleStringList;
begin
  SL := TCastleStringList.Create;
  try
    SL.Add('');
    SL.Add(NL + 'foo' + NL + 'bar' + NL);
    SL.Add('');
    SL.Add('');
    AssertTrue(SL.Count = 4);
    AssertTrue(SL.IndexOf('') = 0);

    SL.Delete(0);
    AssertTrue(SL.Count = 3);
    AssertTrue(SL[0] = NL + 'foo' + NL + 'bar' + NL);
    AssertTrue(SL[1] = '');
    AssertTrue(SL[2] = '');
    AssertTrue(SL.IndexOf('') = 1);

    SL.Delete(1);
    SL.Delete(1);
    AssertTrue(SL.Count = 1);
    AssertTrue(SL[0] = NL + 'foo' + NL + 'bar' + NL);
    AssertTrue(SL.IndexOf(NL + 'foo' + NL + 'bar' + NL) = 0);
  finally FreeAndNil(SL) end;
end;

procedure TTestCastleStringUtils.TestSReplacePatterns;
var
  S1, S2: TCastleStringList;
  SMap: TStringStringMap;
begin
  AssertEquals('bladogbla dog dog', SReplacePatterns('blacatbla dog cat', ['cat'], ['dog'], false));
  { test case matching works }
  AssertEquals('bladogbla dog cAt', SReplacePatterns('blacatbla dog cAt', ['cat'], ['dog'], false));
  AssertEquals('blacatbla dog dog', SReplacePatterns('blacatbla dog cAt', ['cAt'], ['dog'], false));

  AssertEquals('bladogbla dog dog', SReplacePatterns('blacatbla dog cat', ['cat'], ['dog'], true));
  { test case ignoring works }
  AssertEquals('bladogbla dog dog', SReplacePatterns('blacatbla dog cAt', ['cat'], ['dog'], true));
  AssertEquals('bladogbla dog dog', SReplacePatterns('blacatbla dog cAt', ['cAt'], ['dog'], true));
  AssertEquals('blaDogbla dog Dog', SReplacePatterns('blacatbla dog cAt', ['cat'], ['Dog'], true));
  AssertEquals('blaDogbla dog Dog', SReplacePatterns('blacatbla dog cAt', ['cAt'], ['Dog'], true));

  { test pattern inside pattern }
  AssertEquals('12 is 1', SReplacePatterns('foobar is foo', ['foo', 'bar', 'foobar'], ['1', '2', '3'], false));
  AssertEquals('foobar is foo', SReplacePatterns('foobar is foo', ['foo', 'bar', 'foobar'], ['foo', 'bar', '3'], false));
  AssertEquals('3 is 1', SReplacePatterns('foobar is foo', ['foobar', 'foo', 'bar'], ['3', '1', '2'], false));
  AssertEquals('3 is foo', SReplacePatterns('foobar is foo', ['foobar', 'foo', 'bar'], ['3', 'foo', 'bar'], false));
  AssertEquals('3 is f1', SReplacePatterns('foobar is foo', ['foobar', 'oo'], ['3', '1'], false));
  AssertEquals('3oo is f1oo', SReplacePatterns('foobar is foo', ['foobar', 'oo'], ['3oo', '1oo'], false));

  { test overloaded version on TCastleStringList }
  S1 := TCastleStringList.Create;
  S2 := TCastleStringList.Create;
  try
    S1.Append('cat');
    S2.Append('dog');
    AssertEquals('bladogbla dog dog', SReplacePatterns('blacatbla dog cat', S1, S2, false));
  finally
    FreeAndNil(S1);
    FreeAndNil(S2);
  end;

  { test overloaded version on TStringStringMap }
  SMap := TStringStringMap.Create;
  try
    SMap['cat'] := 'dog';
    AssertEquals('bladogbla dog dog', SReplacePatterns('blacatbla dog cat', SMap, false));
  finally FreeAndNil(SMap) end;
end;

{$ifdef FPC}
procedure TTestCastleStringUtils.TestGetFileFilter;
var
  Exts: TStringList;
begin
  Exts := TStringList.Create;
  try
    GetFileFilterExts('xxxx|name1.ext1;name2.ext2', Exts);
    AssertEquals(2, Exts.Count);
    AssertEquals('.ext1', Exts[0]);
    AssertEquals('.ext2', Exts[1]);

    GetFileFilterExts('name1.ext1;name2.ext2', Exts);
    AssertEquals(0, Exts.Count);

    GetFileFilterExts('some name|*.ext1;*.ext2', Exts);
    AssertEquals(2, Exts.Count);
    AssertEquals('.ext1', Exts[0]);
    AssertEquals('.ext2', Exts[1]);

    GetFileFilterExts('some name|ext1;*.ext2', Exts);
    AssertEquals(2, Exts.Count);
    AssertEquals('.ext1', Exts[0]);
    AssertEquals('.ext2', Exts[1]);
  finally FreeAndNil(Exts) end;

  AssertEquals('Pascal files', GetFileFilterName('Pascal files (*.pas)|*.pas'));
  AssertEquals('Pascal files', GetFileFilterName('Pascal files (*.pas;*.inc)|*.pas;*.inc'));
  AssertEquals('Pascal files', GetFileFilterName('Pascal files (*.pas,*.inc)|*.pas;*.inc'));
  AssertEquals('Pascal files', GetFileFilterName('Pascal files|*.pas;*.inc'));
  AssertEquals('Pascal files', GetFileFilterName('Pascal files'));
  AssertEquals('Pascal files', GetFileFilterName('Pascal files ()|'));
end;
{$endif}

procedure TTestCastleStringUtils.TestSplitString;

  procedure AssertStringListEquals(const A: array of string; const List: TCastleStringList);
  var
    I: Integer;
  begin
    AssertEquals(High(A) + 1, List.Count);
    for I := 0 to List.Count - 1 do
      AssertEquals(A[I], List[I]);
  end;

  procedure TestSplitAndGlue(const CorrectParts: array of string;
    const S: string; const Delimiter: char);
  var
    List: TCastleStringList;
  begin
    List := SplitString(S, Delimiter);
    try
      AssertStringListEquals(CorrectParts, List); // check SplitString correctness
      AssertEquals(S, GlueStrings(List, Delimiter)); // check GlueStrings is reverse
    finally FreeAndNil(List) end;
  end;

begin
  TestSplitAndGlue(['foo', 'bar'], 'foo|bar', '|');
  TestSplitAndGlue(['foo'], 'foo', '|');
  TestSplitAndGlue([''], '', '|');
  TestSplitAndGlue(['foo', '', 'bar'], 'foo||bar', '|');
  TestSplitAndGlue(['foo', '', '', 'bar'], 'foo|||bar', '|');
  TestSplitAndGlue(['foo', '', 'bar', ''], 'foo||bar|', '|');
end;

procedure TTestCastleStringUtils.TestTrimEndingNewline;
begin
  AssertEquals('aa  ', TrimEndingNewline('aa  '));
  AssertEquals('aa  ', TrimEndingNewline('aa  '#10));
  AssertEquals('aa  ', TrimEndingNewline('aa  '#13#10));
  AssertEquals(#13'a'#10'a'#10, TrimEndingNewline(#13'a'#10'a'#10#10));
  AssertEquals(#13'a'#10'a'#10, TrimEndingNewline(#13'a'#10'a'#10#13#10));
end;

procedure TTestCastleStringUtils.TestAddMultiLine;
var
  SList: TStringList;
begin
  SList := TStringList.Create;
  try
    AssertEquals(0, SList.Count);
    SList.Add('');
    AssertEquals(1, SList.Count);
    SList.Add('');
    AssertEquals(2, SList.Count);
    SList.Add('');
    AssertEquals(3, SList.Count);
    SList.Add('blahblah'#13'another line'#13#10'yet another line');
    AssertEquals(4, SList.Count);
    SList.AddMultiLine('blahblah'#13'another line'#13#10'yet another line');
    AssertEquals(7, SList.Count);
    SList.AddMultiLine('simple');
    AssertEquals(8, SList.Count);
    SList.AddMultiLine('');
    AssertEquals(9, SList.Count);

    AssertEquals('', SList[0]);
    AssertEquals('', SList[1]);
    AssertEquals('', SList[2]);
    AssertEquals('blahblah'#13'another line'#13#10'yet another line', SList[3]);
    AssertEquals('blahblah', SList[4]);
    AssertEquals('another line', SList[5]);
    AssertEquals('yet another line', SList[6]);
    AssertEquals('simple', SList[7]);
    AssertEquals('', SList[8]);
  finally FreeAndNil(SList) end;
end;

procedure TTestCastleStringUtils.TestRegexpMatches;
begin
  AssertTrue(StringMatchesRegexp('blah', 'blah'));
  AssertTrue(StringMatchesRegexp('notblah', 'blah'));
  AssertFalse(StringMatchesRegexp('blah', 'notblah'));

  // test range [0-9]
  AssertTrue(StringMatchesRegexp('blah12', 'blah[0-9][0-9]'));
  AssertTrue(StringMatchesRegexp('blah123', 'blah[0-9][0-9]'));
  AssertFalse(StringMatchesRegexp('blah123', '^blah[0-9][0-9]$'));
  AssertFalse(StringMatchesRegexp('blah1', 'blah[0-9][0-9]'));
  AssertFalse(StringMatchesRegexp('blah[0-9][0-9]', 'blah[0-9][0-9]'));

  // test range [\d]
  AssertTrue(StringMatchesRegexp('blah12', 'blah[\d][\d]'));
  AssertTrue(StringMatchesRegexp('blah123', 'blah[\d][\d]'));
  AssertFalse(StringMatchesRegexp('blah123', '^blah[\d][\d]$'));
  AssertFalse(StringMatchesRegexp('blah1', 'blah[\d][\d]'));
  AssertFalse(StringMatchesRegexp('blah[\d][\d]', 'blah[\d][\d]'));

  // test +
  { First test unfortunately fails with FPC 3.2.0, fixed only in 3.2.2.

    It's a rather important bug (the regexp tested is very simple),
    but luckily CGE doesn't really use regexp much now (and probably never will),
    so it's not a big issue in CGE case. }
  {$ifndef VER3_2_0}
  AssertTrue(StringMatchesRegexp('blah111foo', 'blah1+foo'));
  {$endif}
  AssertTrue(StringMatchesRegexp('blah1foo', 'blah1+foo'));
  AssertFalse(StringMatchesRegexp('blahfoo', 'blah1+foo'));

  // test *
  {$ifndef VER3_2_0} // fails with FPC 3.2.0, fixed only in 3.2.2
  AssertTrue(StringMatchesRegexp('blah111foo', 'blah1*foo'));
  {$endif}
  AssertTrue(StringMatchesRegexp('blah1foo', 'blah1*foo'));
  AssertTrue(StringMatchesRegexp('blahfoo', 'blah1*foo'));
end;

initialization
  RegisterTest(TTestCastleStringUtils);
end.
