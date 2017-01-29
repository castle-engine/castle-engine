{
  Copyright 2004-2017 Michalis Kamburelis.

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
    procedure TestRandomIntRange;
  end;

implementation

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX} Unix, BaseUnix, {$endif}
  Math, CastleUtils, CastleTimeUtils, CastleVectors;

{$I macspeedtest.inc}

{$warnings off} // knowingly using deprecated, to check they are working

procedure TTestCastleUtils.TestMilisecTime;
const t1: TMilisecTime = High(TMilisecTime) - 10;
      t2: TMilisecTime = 11;
begin
 {Writeln(MilisecTimesSubtract(t2, t1));}
 { 22 - bo jak dodam 10 do t1 to bede na High, +1 bede na 0, +11 bede na 11;
   Razem 11+1+10 = 22; }
 AssertTrue(MilisecTimesSubtract(t2, t1) = 22);
end;

{$warnings on}

procedure TTestCastleUtils.TestIndexMinMax_RestOf3dCoords;
var a: array[0..2]of Double;
    i, c1, c2, cm: integer;
begin
 for i := 1 to 100 do
 begin
  a[0] := Random; a[1] := Random; a[2] := Random;

  cm := IndexMin(a[0], a[1], a[2]);
  RestOf3dCoords(cm, c1, c2);
  AssertTrue( (a[cm] <= a[c1]) and (a[cm] <= a[c2]) );

  cm := IndexMax(a[0], a[1], a[2]);
  RestOf3dCoords(cm, c1, c2);
  AssertTrue( (a[cm] >= a[c1]) and (a[cm] >= a[c2]) );
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
   AssertTrue(CheckIsMemCharFilled(a, SizeOfA, 'x') = -1);
   AssertTrue(IsMemCharFilled(a, SizeOfA, 'x'));
   if not (CheckIsMemCharFilled(a, SizeOfA, 'y') = 0) then
   begin
    WritelnMem(a, SizeOfA);
    raise Exception.Create('failed');
   end;
   AssertTrue(not IsMemCharFilled(a, SizeOfA, 'y'));
  end else
  begin
   YPos := Random(SizeOfA);
   a[YPos] := 'y';
   AssertTrue(CheckIsMemCharFilled(a, SizeOfA, 'x') = YPos);
   AssertTrue(not IsMemCharFilled(a, SizeOfA, 'x'));
   if YPos = 0 then
    AssertTrue(CheckIsMemCharFilled(a, SizeOfA, 'y') = 1) else
    AssertTrue(CheckIsMemCharFilled(a, SizeOfA, 'y') = 0);
   AssertTrue(not IsMemCharFilled(a, SizeOfA, 'y'));
  end;
 end;

 { sprawdz dla SizeOfA = 1 }
 a[0] := 'k';
 AssertTrue(CheckIsMemCharFilled(a, 1, 'k') = -1);
 AssertTrue(IsMemCharFilled(a, 1, 'k'));
 AssertTrue(CheckIsMemCharFilled(a, 1, 'g') = 0);
 AssertTrue(not IsMemCharFilled(a, 1, 'g'));

 { sprawdz dla SizeOfA = 0 (zawsze odpowiedz bedzie brzmiala -1 / true,
   tak jak kwantyfikator ForAll jest zawsze true dla pustego zbioru...) }
 AssertTrue(CheckIsMemCharFilled(a, 0, 'd') = -1);
 AssertTrue(IsMemCharFilled(a, 0, 'd'));

 {$ifdef CASTLEUTILS_SPEED_TESTS}
 TimeTestIsMemCharFilled(100);
 TimeTestIsMemCharFilled(1000);
 TimeTestIsMemCharFilled(10000);
 {$endif}
end;

procedure TTestCastleUtils.TestSmallest2Exp;
var i: Cardinal;
begin
 AssertTrue(Smallest2Exponent(0) = -1);
 AssertTrue(Smallest2Power(0) = 0);
 AssertTrue(Smallest2Exponent(1) = 0);
 AssertTrue(Smallest2Power(1) = 1);
 AssertTrue(Smallest2Exponent(2) = 1);
 AssertTrue(Smallest2Power(2) = 2);
 for i := 3 to 4 do
 begin
  AssertTrue(Smallest2Exponent(i) = 2);
  AssertTrue(Smallest2Power(i) = 4);
 end;
 for i := 5 to 8 do
 begin
  AssertTrue(Smallest2Exponent(i) = 3);
  AssertTrue(Smallest2Power(i) = 8);
 end;
end;

procedure TTestCastleUtils.TestPathDelim;
{$ifdef UNIX}
begin
 AssertTrue(InclPathDelim('/c/blah/') = '/c/blah/');
 AssertTrue(InclPathDelim('/c/blah' ) = '/c/blah/');
 AssertTrue(ExclPathDelim('/c/blah/') = '/c/blah' );
 AssertTrue(ExclPathDelim('/c/blah' ) = '/c/blah' );
{$endif}
{$ifdef MSWINDOWS}
begin
 AssertTrue(InclPathDelim('c:\blah\') = 'c:\blah\');
 AssertTrue(InclPathDelim('c:\blah' ) = 'c:\blah\');
 AssertTrue(ExclPathDelim('c:\blah\') = 'c:\blah' );
 AssertTrue(ExclPathDelim('c:\blah' ) = 'c:\blah' );

 AssertTrue(InclPathDelim('c:\blah/') = 'c:\blah/');
 AssertTrue(ExclPathDelim('c:\blah/') = 'c:\blah' );
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
 { AssertTrue(AnsiSameText('bêcwa³', 'BÊCWA£')); }

 { AssertTrue(not AnsiSameStr('bêcwa³', 'BÊCWA£')); }
 {$endif}
 AssertTrue(SameText('becwal', 'BECWAL'));
 AssertTrue(not SameText('becwal', 'becwal '));
end;

procedure TTestCastleUtils.TestOthers;
begin
 AssertTrue(StringOfChar('x', 0) = '');
 AssertTrue(StringOfChar('x', 1) = 'x');
 AssertTrue(StringOfChar('s', 3) = 'sss');
 { We require Math.Power to work even with base <= 0 (but integer).
   We even used to have our own GeneralPower implemented for this purpose,
   but it's not needed since FPC 1.9.5 (bug 3005 is fixed). }
 AssertTrue(Power( 2.0, 2.0) = 4.0);
 AssertTrue(Power(-2.0, 2.0) = 4.0);
end;

procedure TTestCastleUtils.TestIntSqrt;

  procedure Test(const Value: Cardinal);
  var
    S: Cardinal;
  begin
    S := IntSqrt(Value);
    AssertTrue(Sqr(S) <= Value);
    AssertTrue(Sqr(S+1) > Value);
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
    AssertTrue(DivResult = CorrectDivResult);
    AssertTrue(ModResult = CorrectModResult);

    DivResult := Divident div Divisor;
    ModResult := Divident mod Divisor;
    AssertTrue(DivResult = CorrectDivResult);
    AssertTrue(ModResult = CorrectModResult);
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
  ClampVar(A, 100, 200);
  AssertTrue(A = 123);
  ClampVar(A, 50, 100);
  AssertTrue(A = 100);
  ClampVar(A, 200, 300);
  AssertTrue(A = 200);

  AssertTrue(Clamped(123, 100, 200) = 123);
  AssertTrue(Clamped(123, 50, 100) = 100);
  AssertTrue(Clamped(123, 200, 300) = 200);
end;

procedure TTestCastleUtils.TestSimpleMath;
var
  A, B: Integer;
begin
  A := 123;
  B := 456;
  SwapValues(A, B);
  AssertTrue(A = 456);
  AssertTrue(B = 123);

  OrderUp(A, B);
  AssertTrue(A = 123);
  AssertTrue(B = 456);

  OrderUp(789, 1024, A, B);
  AssertTrue(A = 789);
  AssertTrue(B = 1024);

  AssertTrue(Between(3, -100, 100));
  AssertTrue(not Between(-300, -100, 100));

  AssertFloatsEqual( 0.5, MapRange(2  , 1  , 3  , 0  , 1  ), 0.01);
  AssertFloatsEqual( 0.5, MapRange(2.0, 1.0, 3.0, 0.0, 1.0), 0.01);

  AssertFloatsEqual(0.75, MapRange(1.5, 3  , 1  , 0  , 1  ), 0.01);
  AssertFloatsEqual(0.75, MapRange(1.5, 3.0, 1.0, 0.0, 1.0), 0.01);

  AssertFloatsEqual(0.25, MapRange(1.5, 1  , 3  , 0  , 1  ), 0.01);
  AssertFloatsEqual(0.25, MapRange(1.5, 1.0, 3.0, 0.0, 1.0), 0.01);

  AssertTrue(DivRoundUp(40, 4) = 10);
  AssertTrue(DivRoundUp(42, 4) = 11);
end;

procedure TTestCastleUtils.TestMinMax;
begin
  AssertTrue(Min(345, 789) = 345);
  AssertTrue(Max(345, 789) = 789);
  AssertTrue(Min(345, 123, 789) = 123);
  AssertTrue(Max(345, 123, 789) = 789);
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

  AssertTrue(Recs[0].SortKey = -10);
  AssertTrue(Recs[1].SortKey = -1);
  AssertTrue(Recs[2].SortKey = -1);
  AssertTrue(Recs[3].SortKey = -1);
  AssertTrue(Recs[4].SortKey = -1);
  AssertTrue(Recs[5].SortKey = 10);
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

  AssertTrue(Recs[0].Id = 4);
  AssertTrue(Recs[0].SortKey = -10);
  AssertTrue(Recs[1].Id = 0);
  AssertTrue(Recs[1].SortKey = -1);
  AssertTrue(Recs[2].Id = 1);
  AssertTrue(Recs[2].SortKey = -1);
  AssertTrue(Recs[3].Id = 2);
  AssertTrue(Recs[3].SortKey = -1);
  AssertTrue(Recs[4].Id = 3);
  AssertTrue(Recs[4].SortKey = -1);
  AssertTrue(Recs[5].Id = 5);
  AssertTrue(Recs[5].SortKey = 10);
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

procedure TTestCastleUtils.TestRandomIntRange;

  procedure TestRange(const R1, R2: Integer);
  var
    I: Integer;
    MinRes, MaxRes, V: Integer;
  begin
    AssertTrue(R1 < R2);
    MinRes := High(Integer);
    MaxRes := Low(Integer);
    for I := 1 to 1000 do
    begin
      V := RandomIntRange(R1, R2);
      MinVar(MinRes, V);
      MaxVar(MaxRes, V);
    end;
    AssertTrue(MinRes <= MaxRes);
    AssertTrue(MinRes >= R1);
    AssertTrue(MaxRes < R2);
    // Writeln('For range [', R1, ',', R2 - 1, '], the random numbers were within [', MinRes, ',', MaxRes, ']. Should be usually equal, for large test count');
  end;

begin
  TestRange(10, 100);
  TestRange(-100, -13);
  TestRange(-5, 5);
end;

initialization
 RegisterTest(TTestCastleUtils);
end.
