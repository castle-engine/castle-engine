{
  Copyright 2004-2005 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit TestKambiUtils;

{$I kambiconf.inc}

{ $define KAMBIUTILS_SPEED_TESTS}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestKambiUtils = class(TTestCase)
  published
    procedure TestMilisecTime;
    procedure TestIndexMinMax_RestOf3dCoords;
    procedure TestCheckIsMemCharFilled;
    procedure TestSmallest2Exp;
    procedure TestPathDelim;
    procedure TestReadln;
    procedure TestKambiOSError;
    procedure TestStrings;
    procedure TestOthers;
  end;

implementation

uses
  {$ifdef WIN32} Windows, {$endif}
  {$ifdef UNIX} {$ifdef USE_LIBC} Libc, {$else} Unix, BaseUnix, {$endif} {$endif}
  KambiUtils, Math;

{$I macspeedtest.inc}

procedure TTestKambiUtils.TestMilisecTime;
const t1: TMilisecTime = High(TMilisecTime) - 10;
      t2: TMilisecTime = 11;
begin
 {Writeln(MilisecTimesSubtract(t2, t1));}
 { 22 - bo jak dodam 10 do t1 to bede na High, +1 bede na 0, +11 bede na 11;
   Razem 11+1+10 = 22; }
 Assert(MilisecTimesSubtract(t2, t1) = 22);
end;

procedure TTestKambiUtils.TestIndexMinMax_RestOf3dCoords;
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

procedure TTestKambiUtils.TestCheckIsMemCharFilled;

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

 {$ifdef KAMBIUTILS_SPEED_TESTS}
 TimeTestIsMemCharFilled(100);
 TimeTestIsMemCharFilled(1000);
 TimeTestIsMemCharFilled(10000);
 {$endif}
end;

procedure TTestKambiUtils.TestSmallest2Exp;
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

procedure TTestKambiUtils.TestPathDelim;
{$ifdef UNIX}
begin
 Assert(InclPathDelim('/c/blah/') = '/c/blah/');
 Assert(InclPathDelim('/c/blah' ) = '/c/blah/');
 Assert(ExclPathDelim('/c/blah/') = '/c/blah' );
 Assert(ExclPathDelim('/c/blah' ) = '/c/blah' );
{$endif}
{$ifdef WIN32}
begin
 Assert(InclPathDelim('c:\blah\') = 'c:\blah\');
 Assert(InclPathDelim('c:\blah' ) = 'c:\blah\');
 Assert(ExclPathDelim('c:\blah\') = 'c:\blah' );
 Assert(ExclPathDelim('c:\blah' ) = 'c:\blah' );

 Assert(InclPathDelim('c:\blah/') = 'c:\blah/');
 Assert(ExclPathDelim('c:\blah/') = 'c:\blah' );
{$endif}
end;

procedure TTestKambiUtils.TestReadln;
var i: Integer;
begin
{ for i := 1 to 3 do
  Writeln('You answered '+ BoolToStr[ReadlnYesNo('Answer defTrue ?', defTrue)]);
 for i := 1 to 3 do
  Writeln('You answered '+ BoolToStr[ReadlnYesNo('Answer defNone ?', defNone)]);
 for i := 1 to 3 do
  Writeln('You answered '+ BoolToStr[ReadlnYesNo('Answer defFalse ?', defFalse)]);  }
end;

procedure TTestKambiUtils.TestKambiOSError;
begin
 try
  KambiOSCheck(
    {$ifdef WIN32} Windows.MoveFile('some_not_existing_file_name', 'foo') {$endif}
    {$ifdef UNIX}
      {$ifdef USE_LIBC} Libc.__rename('some_not_existing_file_name', 'foo') <> -1
      {$else}           FpRename('some_not_existing_file_name', 'foo') <> -1
      {$endif}
    {$endif UNIX}
    );
  raise Exception.Create('uups ! KambiOSCheck failed !');
 except
  on EKambiOSError do ;
 end;
end;

procedure TTestKambiUtils.TestStrings;
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

procedure TTestKambiUtils.TestOthers;
begin
 Assert(StringOfChar('x', 0) = '');
 Assert(StringOfChar('x', 1) = 'x');
 Assert(StringOfChar('s', 3) = 'sss');
 Assert(GeneralPower( 2.0, 2.0) = 4.0);
 Assert(GeneralPower(-2.0, 2.0) = 4.0);
end;

initialization
 RegisterTest(TTestKambiUtils);
end.
