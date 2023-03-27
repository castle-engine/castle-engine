{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Auxiliary routines for image support.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPImgCmn;

interface

function Swap(This : qword): qword;
function Swap(This : int64): int64;
function Swap(This : Longword): longword;
function Swap(This : integer): integer;
function Swap(This : Word): Word;
function CalculateCRC (var data; alength:integer) : longword;
function CalculateCRC (CRC:longword; var data; alength:integer) : longword;

implementation

uses sysutils;

function Swap(This : Word): Word;
var
  Tmp1, Tmp2 : Byte;
  AWord      : Word;
begin
  Tmp1 := This AND $00FF;
  Tmp2 := (This AND $FF00) SHR 8;
  AWord := Tmp1;
  result := (AWord SHL 8) + Tmp2;
end;

function Swap(This : integer): integer;
begin
  result := integer(Swap(longword(This)));
end;

function Swap(This : longword): longword;
var
  TmpW1 : Word;
  TmpB1,
  TmpB2 : Byte;
  AnInt : longword;
begin
  TmpW1 := This AND $0000FFFF;
  TmpB1 := TmpW1 AND $00FF;
  TmpB2 := (TmpW1 AND $FF00) SHR 8;
  AnInt := TmpB1;
  AnInt := (AnInt SHL 8) + TmpB2;
  TmpW1 := (This AND $FFFF0000) SHR 16;
  TmpB1 := TmpW1 AND $00FF;
  TmpB2 := (TmpW1 AND $FF00) SHR 8;
  TmpW1 := TmpB1;
  result := (AnInt SHL 16) + (TmpW1 SHL 8) + TmpB2;
end;

function Swap(This : qword): qword;
var l1, l2 : longword;
    res : qword;
begin
  l1:=This and $00000000FFFFFFFF;
  l2:=(This and $FFFFFFFF00000000) shr 32;
  l1:=swap(l1);
  l2:=swap(l2);
  res:=l1;
  Result:=(res shl 32) + l2;
end;

function Swap(This : int64): int64;
begin
  result := int64(Swap(qword(This)));
end;

var CRCtable : array[0..255] of longword;

procedure MakeCRCtable;
var c : longword;
    r, t : integer;
begin
  for r := 0 to 255 do
    begin
    c := r;
    for t := 0 to 7 do
      begin
      if (c and 1) = 1 then
        c := $EDB88320 xor (c shr 1)
      else
        c := c shr 1
      end;
    CRCtable[r] := c;
    end;
end;

function CalculateCRC (CRC:longword; var data; alength:integer) : longword;
var d : pbyte;
    r, t : integer;
begin
  d := @data;
  result := CRC;
  for r := 0 to alength-1 do
    begin
    t := (byte(result) xor d^);
    result := CRCtable[t] xor (result shr 8);
    inc (d);
    end;
end;

function CalculateCRC (var data; alength:integer) : longword;
var f : longword;
begin
  f := CalculateCRC($FFFFFFFF, data, alength);
  result := f xor $FFFFFFFF;
end;

initialization
  MakeCRCtable;
end.
