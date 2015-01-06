{
  Copyright 2004-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleUtils;

{ $define CASTLEUTILS_SPEED_TESTS}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleBaseTestCase;

type
  TTestCastleUtils = class(TCastleBaseTestCase)
  published
    procedure TestMilisecTime;
    procedure TestIndexMinMax_RestOf3dCoords;
    procedure TestCheckIsMemCharFilled;
    procedure TestSmallest2Exp;
    procedure TestPathDelim;
    procedure TestOSError;
    procedure TestStrings;
    procedure TestOthers;
    procedure TestIntSqrt;
    procedure TestDivMod;
    procedure TestClamp;
    procedure TestSimpleMath;
    procedure TestMinMax;
    //procedure TestStableSort;
    procedure TestSort;
    procedure TestFileExt;
    procedure TestFloatModulo;
  end;

implementation

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX} Unix, BaseUnix, {$endif}
  Math, CastleUtils, CastleTimeUtils, CastleVectors;

{$I macspeedtest.inc}

procedure TTestCastleUtils.TestMilisecTime;
const t1: TMilisecTime = High(TMilisecTime) - 10;
      t2: TMilisecTime = 11;
begin
 {Writeln(MilisecTimesSubtract(t2, t1));}
 { 22 - bo jak dodam 10 do t1 to bede na High, +1 bede na 0, +11 bede na 11;
   Razem 11+1+10 = 22; }
 Assert(MilisecTimesSubtract(t2, t1) = 22);
end;

procedure TTestCastleUtils.TestIndexMinMax_RestOf3dCoords;
var a: array[0..2]of Double;
    i, c1, c2, cm: integer;
begin
 for i := 1 to 100 do
 begin
  a[0] := Random; a[1] := Random; a[2] := Random;

  cm := IndexMin(a[0], a[1], a[2]);
  RestOf3dCoords(cm, c1, c2);
  Assert( (a[cm] <= a[c1]) and (a[cm] <= a[c2]) );

  cm := IndexMax(a[0], a[1], a[2]);
  RestOf3dCoords(cm, c1, c2);
  Assert( (a[cm] >= a[c1]) and (a[cm] >= a[c2]) );
 end;
end;

procedure WritelnMem(const Data; Size: Integer);
var p: PChar;
begin
 p := @Data;
 while Size > 0 do
 begin
  if Ord(p^) < Ord(' ') then Write('(',Ord(p^), ')') else Write(p^);
  Inc(p);
  Dec(Size);
 end;
 Writeln;
end;

function NaiveIsMemCharFilled(const Data; Size: Integer; AChar: char): boolean;
begin
 result := CheckIsMemCharFilled(Data, Size, AChar) = -1;
end;

procedure TTestCastleUtils.TestCheckIsMemCharFilled;

  procedure TimeTestIsMemCharFilled(SizeOfA: Integer);
  SpeedTest_Declare
  var pa: pointer;
  begin
   { testowanie na danych ktore nie sa CharFilled jest problematyczne.
     Czas wtedy bedzie zalezal liniowo od pozycji na ktorej jest przeklamanie.
     Zreszta dla danych losowych mamy prawie pewnosc ze przeklamanie bedzie
     juz na 1 pozycji, wiec jaki sens tu testowac szybkosc ?
     sChcemy sprawdzic szybkosc dla przypadku pesymistycznego. }
   pa := GetMem(SizeOfA);
   try
    FillChar(pa^, SizeOfA, 'x');
    {$define SpeedTest_Name := 'IsMemCharFilled on SizeOfA = '+IntToStr(SizeOfA)}
    {$define SpeedTest_Cycles := 100000}
    {$define SpeedTest_DoSlowerCycle := NaiveIsMemCharFilled(pa^, SizeOfA, 'x')}
    {$define SpeedTest_DoFasterCycle := IsMemCharFilled(pa^, SizeOfA, 'x')}
    {$define SpeedTest_SlowerName := 'NaiveIsMemCharFilled'}
    {$define SpeedTest_FasterName := 'IsMemCharFilled'}
    SpeedTest
   finally FreeMem(pa) end;
  end;

var a: array[0..100]of char;
    SizeOfA: Integer;
    i, YPos: Integer;
begin
 for i := 1 to 1000 do
 begin
  { losuje SizeOfA bo chce zeby sprawdzil czy funkcje
    [Check]IsMemCharFilled dzialaja dla roznych Size.
    Zawsze niech SizeOfA >= 2, przypadki SizeOfA < 2 sprawdzimy pozniej osobno. }
  if Random(2) = 0 then SizeOfA := Random(4) else SizeOfA := Random(100);
  SizeOfA += 2;

  FillChar(a, SizeOfA, 'x');
  if Random(2) = 0 then
  begin
   Assert(CheckIsMemCharFilled(a, SizeOfA, 'x') = -1);
   Assert(IsMemCharFilled(a, SizeOfA, 'x'));
   if not (CheckIsMemCharFilled(a, SizeOfA, 'y') = 0) then
   begin
    WritelnMem(a, SizeOfA);
    raise Exception.Create('failed');
   end;
   Assert(not IsMemCharFilled(a, SizeOfA, 'y'));
  end else
  begin
   YPos := Random(SizeOfA);
   a[YPos] := 'y';
   Assert(CheckIsMemCharFilled(a, SizeOfA, 'x') = YPos);
   Assert(not IsMemCharFilled(a, SizeOfA, 'x'));
   if YPos = 0 then
    Assert(CheckIsMemCharFilled(a, SizeOfA, 'y') = 1) else
    Assert(CheckIsMemCharFilled(a, SizeOfA, 'y') = 0);
   Assert(not IsMemCharFilled(a, SizeOfA, 'y'));
  end;
 end;

 { sprawdz dla SizeOfA = 1 }
 a[0] := 'k';
 Assert(CheckIsMemCharFilled(a, 1, 'k') = -1);
 Assert(IsMemCharFilled(a, 1, 'k'));
 Assert(CheckIsMemCharFilled(a, 1, 'g') = 0);
 Assert(not IsMemCharFilled(a, 1, 'g'));

 { sprawdz dla SizeOfA = 0 (zawsze odpowiedz bedzie brzmiala -1 / true,
   tak jak kwantyfikator ForAll jest zawsze true dla pustego zbioru...) }
 Assert(CheckIsMemCharFilled(a, 0, 'd') = -1);
 Assert(IsMemCharFilled(a, 0, 'd'));

 {$ifdef CASTLEUTILS_SPEED_TESTS}
 TimeTestIsMemCharFilled(100);
 TimeTestIsMemCharFilled(1000);
 TimeTestIsMemCharFilled(10000);
 {$endif}
end;

procedure TTestCastleUtils.TestSmallest2Exp;
var i: Cardinal;
begin
 Assert(Smallest2Exponent(0) = -1);
 Assert(Smallest2Power(0) = 0);
 Assert(Smallest2Exponent(1) = 0);
 Assert(Smallest2Power(1) = 1);
 Assert(Smallest2Exponent(2) = 1);
 Assert(Smallest2Power(2) = 2);
 for i := 3 to 4 do
 begin
  Assert(Smallest2Exponent(i) = 2);
  Assert(Smallest2Power(i) = 4);
 end;
 for i := 5 to 8 do
 begin
  Assert(Smallest2Exponent(i) = 3);
  Assert(Smallest2Power(i) = 8);
 end;
end;

procedure TTestCastleUtils.TestPathDelim;
{$ifdef UNIX}
begin
 Assert(InclPathDelim('/c/blah/') = '/c/blah/');
 Assert(InclPathDelim('/c/blah' ) = '/c/blah/');
 Assert(ExclPathDelim('/c/blah/') = '/c/blah' );
 Assert(ExclPathDelim('/c/blah' ) = '/c/blah' );
{$endif}
{$ifdef MSWINDOWS}
begin
 Assert(InclPathDelim('c:\blah\') = 'c:\blah\');
 Assert(InclPathDelim('c:\blah' ) = 'c:\blah\');
 Assert(ExclPathDelim('c:\blah\') = 'c:\blah' );
 Assert(ExclPathDelim('c:\blah' ) = 'c:\blah' );

 Assert(InclPathDelim('c:\blah/') = 'c:\blah/');
 Assert(ExclPathDelim('c:\blah/') = 'c:\blah' );
{$endif}
end;

procedure TTestCastleUtils.TestOSError;
begin
 try
  OSCheck(
    {$ifdef MSWINDOWS} Windows.MoveFile('some_not_existing_file_name', 'foo') {$endif}
    {$ifdef UNIX} FpRename('some_not_existing_file_name', 'foo') <> -1 {$endif}
    );
  raise Exception.Create('uups ! OSCheck failed !');
 except
  on EOSError do ;
 end;
end;

procedure TTestCastleUtils.TestStrings;
begin
 {$ifdef UNIX}
 { only on systems with locale properly de/encoding ISO-8859-2 ! }

 { does not pass !! But should !! FPC rtl not ready yet !! }
 { Assert(AnsiSameText('bêcwa³', 'BÊCWA£')); }

 { Assert(not AnsiSameStr('bêcwa³', 'BÊCWA£')); }
 {$endif}
 Assert(SameText('becwal', 'BECWAL'));
 Assert(not SameText('becwal', 'becwal '));
end;

procedure TTestCastleUtils.TestOthers;
begin
 Assert(StringOfChar('x', 0) = '');
 Assert(StringOfChar('x', 1) = 'x');
 Assert(StringOfChar('s', 3) = 'sss');
 { We require Math.Power to work even with base <= 0 (but integer).
   We even used to have our own GeneralPower implemented for this purpose,
   but it's not needed since FPC 1.9.5 (bug 3005 is fixed). }
 Assert(Power( 2.0, 2.0) = 4.0);
 Assert(Power(-2.0, 2.0) = 4.0);
end;

procedure TTestCastleUtils.TestIntSqrt;

  procedure Test(const Value: Cardinal);
  var
    S: Cardinal;
  begin
    S := IntSqrt(Value);
    Assert(Sqr(S) <= Value);
    Assert(Sqr(S+1) > Value);
  end;

var
  I: Cardinal;
begin
  for I := 0       to 100 do Test(I);
  for I := 1000    to 1100 do Test(I);
  for I := 10000   to 10100 do Test(I);
  for I := 100000  to 100100 do Test(I);
  for I := 1000000 to 1000100 do Test(I);
end;

procedure TTestCastleUtils.TestDivMod;

  procedure OneTest(const Divident: Integer; const Divisor: Word;
    const CorrectDivResult, CorrectModResult: SmallInt);
  var
    DivResult: SmallInt;
    ModResult: SmallInt;
  begin
    CastleDivMod(Divident, Divisor, DivResult, ModResult);
    Assert(DivResult = CorrectDivResult);
    Assert(ModResult = CorrectModResult);

    DivResult := Divident div Divisor;
    ModResult := Divident mod Divisor;
    Assert(DivResult = CorrectDivResult);
    Assert(ModResult = CorrectModResult);
  end;

begin
  OneTest(-39, 20, -1, -19);
  OneTest(-9, 5, -1, -4);
end;

procedure TTestCastleUtils.TestClamp;
var
  A: Integer;
begin
  A := 123;
  Clamp(A, 100, 200);
  Assert(A = 123);
  Clamp(A, 50, 100);
  Assert(A = 100);
  Clamp(A, 200, 300);
  Assert(A = 200);

  Assert(Clamped(123, 100, 200) = 123);
  Assert(Clamped(123, 50, 100) = 100);
  Assert(Clamped(123, 200, 300) = 200);
end;

procedure TTestCastleUtils.TestSimpleMath;
var
  A, B: Integer;
begin
  A := 123;
  B := 456;
  SwapValues(A, B);
  Assert(A = 456);
  Assert(B = 123);

  OrderUp(A, B);
  Assert(A = 123);
  Assert(B = 456);

  OrderUp(789, 1024, A, B);
  Assert(A = 789);
  Assert(B = 1024);

  Assert(Between(3, -100, 100));
  Assert(not Between(-300, -100, 100));

  AssertFloatsEqual( 0.5, MapRange(2  , 1  , 3  , 0  , 1  ), 0.01);
  AssertFloatsEqual( 0.5, MapRange(2.0, 1.0, 3.0, 0.0, 1.0), 0.01);

  AssertFloatsEqual(0.75, MapRange(1.5, 3  , 1  , 0  , 1  ), 0.01);
  AssertFloatsEqual(0.75, MapRange(1.5, 3.0, 1.0, 0.0, 1.0), 0.01);

  AssertFloatsEqual(0.25, MapRange(1.5, 1  , 3  , 0  , 1  ), 0.01);
  AssertFloatsEqual(0.25, MapRange(1.5, 1.0, 3.0, 0.0, 1.0), 0.01);

  Assert(DivRoundUp(40, 4) = 10);
  Assert(DivRoundUp(42, 4) = 11);
end;

procedure TTestCastleUtils.TestMinMax;
begin
  Assert(Min(345, 789) = 345);
  Assert(Max(345, 789) = 789);
  Assert(Min(345, 123, 789) = 123);
  Assert(Max(345, 123, 789) = 789);
end;

type
  TRec = record Id: Integer; SortKey: Integer; end;
  PRec = ^TRec;

function IsSmallerRec(const A, B, Data: Pointer): boolean;
begin
  Result := PRec(A)^.SortKey < PRec(B)^.SortKey;
end;

procedure TTestCastleUtils.TestSort;
var
  Recs: array of TRec;
begin
  SetLength(Recs, 6);
  Recs[0].Id := 0;
  Recs[0].SortKey := -1;
  Recs[1].Id := 1;
  Recs[1].SortKey := -1;
  Recs[2].Id := 2;
  Recs[2].SortKey := -1;
  Recs[3].Id := 3;
  Recs[3].SortKey := -1;
  Recs[4].Id := 4;
  Recs[4].SortKey := -10;
  Recs[5].Id := 5;
  Recs[5].SortKey := 10;

  Sort(Pointer(Recs), SizeOf(TRec), @IsSmallerRec, nil, 0, Length(Recs) - 1);

  Assert(Recs[0].SortKey = -10);
  Assert(Recs[1].SortKey = -1);
  Assert(Recs[2].SortKey = -1);
  Assert(Recs[3].SortKey = -1);
  Assert(Recs[4].SortKey = -1);
  Assert(Recs[5].SortKey = 10);
end;

{
procedure TTestCastleUtils.TestStableSort;
var
  Recs: array of TRec;
begin
  SetLength(Recs, 6);
  Recs[0].Id := 0;
  Recs[0].SortKey := -1;
  Recs[1].Id := 1;
  Recs[1].SortKey := -1;
  Recs[2].Id := 2;
  Recs[2].SortKey := -1;
  Recs[3].Id := 3;
  Recs[3].SortKey := -1;
  Recs[4].Id := 4;
  Recs[4].SortKey := -10;
  Recs[5].Id := 5;
  Recs[5].SortKey := 10;

  StableSort(Pointer(Recs), SizeOf(TRec), @IsSmallerRec, nil, 0, Length(Recs) - 1);

  Assert(Recs[0].Id = 4);
  Assert(Recs[0].SortKey = -10);
  Assert(Recs[1].Id = 0);
  Assert(Recs[1].SortKey = -1);
  Assert(Recs[2].Id = 1);
  Assert(Recs[2].SortKey = -1);
  Assert(Recs[3].Id = 2);
  Assert(Recs[3].SortKey = -1);
  Assert(Recs[4].Id = 3);
  Assert(Recs[4].SortKey = -1);
  Assert(Recs[5].Id = 5);
  Assert(Recs[5].SortKey = 10);
end;
}

procedure TTestCastleUtils.TestFileExt;
begin
  AssertEquals('.wrl', ExtractFileExt('blah.blah.blah' + PathDelim + 'foo.wrl'));
  AssertEquals('.wrl', ExtractFileExt('blah.blah.blah' + PathDelim + 'foo.bar.wrl'));
  AssertEquals('.wrl', ExtractFileDoubleExt('blah.blah.blah' + PathDelim + 'foo.wrl'));
  AssertEquals('.bar.wrl', ExtractFileDoubleExt('blah.blah.blah' + PathDelim + 'foo.bar.wrl'));
  AssertEquals('blah.blah.blah' + PathDelim + 'foo', DeleteFileExt('blah.blah.blah' + PathDelim + 'foo.wrl'));
  AssertEquals('blah.blah.blah' + PathDelim + 'foo.bar', DeleteFileExt('blah.blah.blah' + PathDelim + 'foo.bar.wrl'));
end;

procedure TTestCastleUtils.TestFloatModulo;
begin
  AssertFloatsEqual(0.5, FloatModulo( 0.5, 2));
  AssertFloatsEqual(0.5, FloatModulo( 2.5, 2));
  AssertFloatsEqual(1.5, FloatModulo( 3.5, 2));
  AssertFloatsEqual(1.5, FloatModulo(-0.5, 2));
  AssertFloatsEqual(1.5, FloatModulo(-2.5, 2));
  AssertFloatsEqual(0.5, FloatModulo(-3.5, 2));
end;

initialization
 RegisterTest(TTestCastleUtils);
end.
