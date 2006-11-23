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

unit TestDynArrays;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestDynArrays = class(TTestCase)
    procedure TestDynArrays;
  end;

implementation

uses KambiUtils;

{ DynArray.inc KambiUtils_StandardDynArrays.inc }

function IsSmallerString(const a, b: string): boolean;
begin result := a < b end;

procedure TTestDynArrays.TestDynArrays;
var sarr, sarr2: TDynStringArray;
    iarr, iarr2: TDynIntegerArray; { tablica int nie jest init_fini }
    i, j: integer;
const twoStrings: array[0..1]of string = ('raz','dwa');
begin
 for i := 1 to 100 do
 begin
  sarr := TDynStringArray.Create;
  try
   sarr.AllowedCapacityOverflow := Random(8);
   sarr.SetLength(4);
   sarr[0] := 'FOO';
   sarr[1] := 'foo bar xyz';
   sarr.Delete(0, 1);
   sarr.AppendArray(twoStrings);
   sarr.AppendItem('trzy?');

   Assert(not sarr.Equal(['foo bar xyz', '', '']));
   Assert(sarr.Equal(['foo bar xyz', '', '', 'raz', 'dwa', 'trzy?']));

   sarr.Reverse;
   Assert(sarr.Equal(['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));

   sarr2 := TDynStringArray.Create;
   try
    sarr2.AppendItem('blah');
    Assert(sarr2.Equal(['blah']));
    sarr2.Assign(sarr);
    Assert(sarr2.Equal(['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));

    {sortuj ustalone 6 stringow}
    sarr.Sort(@IsSmallerString);
    Assert(sarr.Equal(['', '', 'dwa', 'foo bar xyz', 'raz', 'trzy?']));

    { sprawdz ze kolejnosc na sarr2 pozostala niezmieniona }
    Assert(sarr2.Equal(['trzy?', 'dwa', 'raz', '', '', 'foo bar xyz']));
   finally sarr2.Free end;

   {dodaj losowe stringi, sortuj, sprawdz}
   for j := 0 to 20 do
    sarr.AppendItem( Chr(Random(256)) + Chr(Random(256)) + Chr(Random(256)) );
   sarr.Sort(@IsSmallerString);
   for j := 0 to sarr.Count-2 do Assert(sarr[j] <= sarr[j+1]);

  finally sarr.Free end;
 end;

 sarr := TDynStringArray.Create;
 try
  { na tablicy o 0 liczbie elementow tez wszystko powinno isc ok }
  Assert(sarr.Length = 0);
  sarr.Reverse;
  Assert(sarr.Length = 0);
 finally sarr.Free end;

 iarr := nil;
 iarr2 := nil;
 try
  iarr := TDynIntegerArray.Create;
  iarr2 := TDynIntegerArray.Create;
  Assert(iarr.Equal(iarr2));

  iarr.AppendArray([1, 3, 6, 8]);
  Assert(not iarr.Equal(iarr2));
  iarr2.AppendArray([1, 3, 6, 8]);
  Assert(iarr.Equal(iarr2));
  iarr2.Insert(0, 99);
  Assert(not iarr.Equal(iarr2));
  Assert(iarr2.Equal([99, 1, 3, 6, 8]));

  iarr.Assign(iarr2);
  Assert(iarr.Equal(iarr2));
  Assert(iarr.Equal([99, 1, 3, 6, 8]));
 finally
  iarr.Free;
  iarr2.Free;
 end;
end;

initialization
 RegisterTest(TTestDynArrays);
end.
