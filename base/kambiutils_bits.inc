{
  Copyright 2002-2004 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Some trivial operations on bits }

{$ifdef read_interface}

{ bit nr 0 = bit na 1-wszej pozycji od prawej (oznaczajacy jednosc)
  bit nr 1 = na drugiej (oznaczajacy "dwojkowosc", tzn. dwa do potegi pierwszej) itd. }
const bit0=1; bit1=2; bit2=4; bit3=8;
      bit4=16; bit5=32; bit6=64; bit7=128;
      bit8=256;

function SetBit(bitnum:byte; bitset:byte; value:boolean):byte; { jesli value=stala, to lepiej uzyj setbit0/1 ! }
function SetBit1(bitnum:byte; bitset:byte):byte;
function SetBit0(bitnum:byte; bitset:byte):byte;

procedure SetBit_var(bitnum:byte; var bitset:byte; value:boolean);
procedure SetBit1_var(bitnum:byte; var bitset:byte);
procedure SetBit0_var(bitnum:byte; var bitset:byte);

function GetBit(bitnum:byte; bitset:byte):boolean;

function GetBitLongWord(bitnum:byte; bitset:LongWord):boolean;
function SetBit1LongWord(bitnum:byte; bitset:LongWord):LongWord;
function SetBit0LongWord(bitnum:byte; bitset:LongWord):LongWord;
function SetBitLongWord(bitnum:byte; bitset:LongWord; value:boolean):LongWord;

{ cos speszial : bajt ktorego kazdy bit jest wylosowany na 1 z szansa OneChance }
function RandomBitsByte(OneChance:Extended):byte;

{$endif read_interface}

{$ifdef read_implementation}

function SetBit(bitnum:byte; bitset:byte; value:boolean): byte;
begin
 if value then result:=SetBit1(bitnum,bitset) else
               result:=SetBit0(bitnum,bitset);
end;

procedure Setbit_var(bitnum:byte; var bitset:byte; value:boolean);
begin
 if value then bitset:=SetBit1(bitnum,bitset) else
               bitset:=SetBit0(bitnum,bitset);
end;

function SetBit1(bitnum:byte; bitset:byte):byte;
begin  result:=bitset or (1 shl bitnum)  end;

procedure SetBit1_var(bitnum:byte; var bitset:byte);
begin  bitset:=SetBit1(bitnum,bitset)  end;

function SetBit0(bitnum:byte; bitset:byte):byte;
begin  result:=bitset and not (1 shl bitnum)  end;

procedure SetBit0_var(bitnum:byte; var bitset:byte);
begin bitset:=SetBit0(bitnum,bitset) end;

function GetBit(bitnum:byte; bitset:byte):boolean;
begin  result:=(bitset and (1 shl bitnum))<>0  end;

function GetBitLongWord(bitnum:byte; bitset:LongWord):boolean;
begin result:=(bitset and LongWord(1 shl bitnum))<>0 end;

function SetBit1LongWord(bitnum:byte; bitset:LongWord):LongWord;
begin result:=bitset or LongWord(1 shl bitnum) end;

function SetBit0LongWord(bitnum:byte; bitset:LongWord):LongWord;
begin result:=bitset and not LongWord(1 shl bitnum) end;

function SetBitLongWord(bitnum:byte; bitset:LongWord; value:boolean):LongWord;
begin
 if value then result:=SetBit1LongWord(bitnum,bitset) else
               result:=SetBit0LongWord(bitnum,bitset);
end;

function RandomBitsByte(OneChance:Extended):byte;
var i:integer;
begin
 { notka - Wynik Random jest zawsze w przedziale [0,1). To wazne zeby tu bylo
   porownanie ostre "<". Wtedy dla szansy = dokladnie 0 nigdy nie bedzie Random<0.
   (dla szansy = 1 warunek zarowno z "<=" jak i z "<" gwarantowalby ze
   zawsze taka szansa zachodzi - bo zawsze zachodzilaby ostra nierownosc wiec
   i nieostra takze.) }
 result:=0;
 for i:=0 to 7 do if Random<OneChance then result := result + (1 shl i);  
end;

{$endif read_implementation}