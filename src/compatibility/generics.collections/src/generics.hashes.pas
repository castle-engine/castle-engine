{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Maciej Izak (hnb)
    member of the Free Sparta development team (http://freesparta.com)

    Copyright(c) 2004-2014 DaThoX

    It contains the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Generics.Hashes;

{$MODE DELPHI}{$H+}
{$POINTERMATH ON}
{$MACRO ON}
{$COPERATORS ON}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  Classes, SysUtils;

// Original version of Bob Jenkins Hash
// http://burtleburtle.net/bob/c/lookup3.c
function HashWord(
  AKey: PLongWord;                   //* the key, an array of uint32_t values */
  ALength: SizeInt;                  //* the length of the key, in uint32_ts */
  AInitVal: UInt32): UInt32;         //* the previous hash, or an arbitrary value */
procedure HashWord2 (
  AKey: PLongWord;                   //* the key, an array of uint32_t values */
  ALength: SizeInt;                  //* the length of the key, in uint32_ts */
  var APrimaryHashAndInitVal: UInt32;                  //* IN: seed OUT: primary hash value */
  var ASecondaryHashAndInitVal: UInt32);               //* IN: more seed OUT: secondary hash value */

function HashLittle(AKey: Pointer; ALength: SizeInt; AInitVal: UInt32): UInt32;
procedure HashLittle2(
  AKey: Pointer;        //* the key to hash */
  ALength: SizeInt;     //* length of the key */
  var APrimaryHashAndInitVal: UInt32;                  //* IN: primary initval, OUT: primary hash */
  var ASecondaryHashAndInitVal: UInt32);               //* IN: secondary initval, OUT: secondary hash */

function DelphiHashLittle(AKey: Pointer; ALength: SizeInt; AInitVal: UInt32): Int32;
procedure DelphiHashLittle2(AKey: Pointer; ALength: SizeInt; var APrimaryHashAndInitVal, ASecondaryHashAndInitVal: UInt32);

// hash function from fstl
function SimpleChecksumHash(AKey: Pointer; ALength: SizeInt): UInt32;

// some other hashes
// http://stackoverflow.com/questions/14409466/simple-hash-functions
// http://www.partow.net/programming/hashfunctions/
// http://en.wikipedia.org/wiki/List_of_hash_functions
// http://www.cse.yorku.ca/~oz/hash.html

// https://code.google.com/p/hedgewars/source/browse/hedgewars/adler32.pas
function Adler32(AKey: Pointer; ALength: SizeInt): UInt32;
function sdbm(AKey: Pointer; ALength: SizeInt): UInt32;

implementation

function SimpleChecksumHash(AKey: Pointer; ALength: SizeInt): UInt32;
var
  i: Integer;
  ABuffer: PUInt8 absolute AKey;
begin
  Result := 0;
  for i := 0 to ALength - 1 do
     Inc(Result,ABuffer[i]);
end;

function Adler32(AKey: Pointer; ALength: SizeInt): UInt32;
const
  MOD_ADLER = 65521;
var
  ABuffer: PUInt8 absolute AKey;
  a: UInt32 = 1;
  b: UInt32 = 0;
  n: Integer;
begin
  for n := 0 to ALength -1 do
  begin
    a := (a + ABuffer[n]) mod MOD_ADLER;
    b := (b + a) mod MOD_ADLER;
  end;
  Result := (b shl 16) or a;
end;

function sdbm(AKey: Pointer; ALength: SizeInt): UInt32;
var
  c: PUInt8 absolute AKey;
  i: Integer;
begin
  Result := 0;
  c := AKey;
  for i := 0 to ALength - 1 do
  begin
    Result := c^ + (Result shl 6) + (Result shl 16) {%H-}- Result;
    Inc(c);
  end;
end;

{ BobJenkinsHash }

{$define mix_abc :=
  a -= c;  a := a xor (((c)shl(4)) or ((c)shr(32-(4))));  c += b;
  b -= a;  b := b xor (((a)shl(6)) or ((a)shr(32-(6))));  a += c;
  c -= b;  c := c xor (((b)shl(8)) or ((b)shr(32-(8))));  b += a;
  a -= c;  a := a xor (((c)shl(16)) or ((c)shr(32-(16))));  c += b;
  b -= a;  b := b xor (((a)shl(19)) or ((a)shr(32-(19))));  a += c;
  c -= b;  c := c xor (((b)shl(4)) or ((b)shr(32-(4))));  b += a
}

{$define final_abc :=
  c := c xor b; c -= (((b)shl(14)) or ((b)shr(32-(14))));
  a := a xor c; a -= (((c)shl(11)) or ((c)shr(32-(11))));
  b := b xor a; b -= (((a)shl(25)) or ((a)shr(32-(25))));
  c := c xor b; c -= (((b)shl(16)) or ((b)shr(32-(16))));
  a := a xor c; a -= (((c)shl(4)) or ((c)shr(32-(4))));
  b := b xor a; b -= (((a)shl(14)) or ((a)shr(32-(14))));
  c := c xor b; c -= (((b)shl(24)) or ((b)shr(32-(24))))
}

function HashWord(
  AKey: PLongWord;                   //* the key, an array of uint32_t values */
  ALength: SizeInt;               //* the length of the key, in uint32_ts */
  AInitVal: UInt32): UInt32;         //* the previous hash, or an arbitrary value */
var
  a,b,c: UInt32;
label
  Case0, Case1, Case2, Case3;
begin
  //* Set up the internal state */
  a := $DEADBEEF + (UInt32(ALength) shl 2) + AInitVal;
  b := a;
  c := b;

  //*------------------------------------------------- handle most of the key */
  while ALength > 3 do
  begin
    a += AKey[0];
    b += AKey[1];
    c += AKey[2];
    mix_abc;
    ALength -= 3;
    AKey += 3;
  end;

  //*------------------------------------------- handle the last 3 uint32_t's */
  case ALength of //* all the case statements fall through */
    3: goto Case3;
    2: goto Case2;
    1: goto Case1;
    0: goto Case0;
  end;
  Case3: c+=AKey[2];
  Case2: b+=AKey[1];
  Case1: a+=AKey[0];
    final_abc;
  Case0:     //* case 0: nothing left to add */
  //*------------------------------------------------------ report the result */
  Result := c;
end;

procedure HashWord2 (
AKey: PLongWord;                   //* the key, an array of uint32_t values */
ALength: SizeInt;               //* the length of the key, in uint32_ts */
var APrimaryHashAndInitVal: UInt32;                      //* IN: seed OUT: primary hash value */
var ASecondaryHashAndInitVal: UInt32);               //* IN: more seed OUT: secondary hash value */
var
  a,b,c: UInt32;
label
  Case0, Case1, Case2, Case3;
begin
  //* Set up the internal state */
  a := $deadbeef + (UInt32(ALength shl 2)) + APrimaryHashAndInitVal;
  b := a;
  c := b;
  c += ASecondaryHashAndInitVal;

  //*------------------------------------------------- handle most of the key */
  while ALength > 3 do
  begin
    a += AKey[0];
    b += AKey[1];
    c += AKey[2];
    mix_abc;
    ALength -= 3;
    AKey += 3;
  end;

  //*------------------------------------------- handle the last 3 uint32_t's */
  case ALength of                     //* all the case statements fall through */
    3: goto Case3;
    2: goto Case2;
    1: goto Case1;
    0: goto Case0;
  end;
  Case3: c+=AKey[2];
  Case2: b+=AKey[1];
  Case1: a+=AKey[0];
    final_abc;
  Case0:     //* case 0: nothing left to add */
  //*------------------------------------------------------ report the result */
  APrimaryHashAndInitVal := c;
  ASecondaryHashAndInitVal := b;
end;

function HashLittle(AKey: Pointer; ALength: SizeInt; AInitVal: UInt32): UInt32;
var
  a, b, c: UInt32;
  u: record case byte of
    0: (ptr: Pointer);
    1: (i: PtrUint);
  end absolute AKey;

  k32: ^UInt32 absolute AKey;
  k16: ^UInt16 absolute AKey;
  k8: ^UInt8 absolute AKey;

label _10, _8, _6, _4, _2;
label Case12, Case11, Case10, Case9, Case8, Case7, Case6, Case5, Case4, Case3, Case2, Case1;

begin
  a := $DEADBEEF + UInt32(ALength) + AInitVal;
  b := a;
  c := b;

{$IFDEF ENDIAN_LITTLE}
  if (u.i and $3) = 0 then
  begin
    while (ALength > 12) do
    begin
      a += k32[0];
      b += k32[1];
      c += k32[2];
      mix_abc;
      ALength -= 12;
      k32 += 3;
    end;

    case ALength of
      12: begin c += k32[2]; b += k32[1]; a += k32[0]; end;
      11: begin c += k32[2] and $ffffff; b += k32[1]; a += k32[0]; end;
      10: begin c += k32[2] and $ffff; b += k32[1]; a += k32[0]; end;
      9 : begin c += k32[2] and $ff; b += k32[1]; a += k32[0]; end;
      8 : begin b += k32[1]; a += k32[0]; end;
      7 : begin b += k32[1] and $ffffff; a += k32[0]; end;
      6 : begin b += k32[1] and $ffff; a += k32[0]; end;
      5 : begin b += k32[1] and $ff; a += k32[0]; end;
      4 : begin a += k32[0]; end;
      3 : begin a += k32[0] and $ffffff; end;
      2 : begin a += k32[0] and $ffff; end;
      1 : begin a += k32[0] and $ff; end;
      0 : Exit(c);              // zero length strings require no mixing
    end
  end
  else
  if (u.i and $1) = 0 then
  begin
    while (ALength > 12) do
    begin
      a += k16[0] + (UInt32(k16[1]) shl 16);
      b += k16[2] + (UInt32(k16[3]) shl 16);
      c += k16[4] + (UInt32(k16[5]) shl 16);
      mix_abc;
      ALength -= 12;
      k16 += 6;
    end;

    case ALength of
      12:
        begin
          c+=k16[4]+((UInt32(k16[5])) shl 16);
          b+=k16[2]+((UInt32(k16[3])) shl 16);
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      11:
        begin
          c+=(UInt32(k8[10])) shl 16;     //* fall through */
          goto _10;
        end;
      10:
        begin _10:
          c+=k16[4];
          b+=k16[2]+((UInt32(k16[3])) shl 16);
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      9 :
        begin
          c+=k8[8];                      //* fall through */
          goto _8;
        end;
      8 :
        begin _8:
          b+=k16[2]+((UInt32(k16[3])) shl 16);
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      7 :
        begin
          b+=(UInt32(k8[6])) shl 16;      //* fall through */
          goto _6;
        end;
      6 :
        begin _6:
          b+=k16[2];
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      5 :
        begin
          b+=k8[4];                      //* fall through */
          goto _4;
        end;
      4 :
        begin _4:
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      3 :
        begin
          a+=(UInt32(k8[2])) shl 16;      //* fall through */
          goto _2;
        end;
      2 :
        begin _2:
          a+=k16[0];
        end;
      1 :
        begin
          a+=k8[0];
        end;
      0 : Exit(c);                     //* zero length requires no mixing */
    end;
  end
  else
{$ENDIF}
  begin
    while ALength > 12 do
    begin
      a += k8[0];
      a += (UInt32(k8[1])) shl 8;
      a += (UInt32(k8[2])) shl 16;
      a += (UInt32(k8[3])) shl 24;
      b += k8[4];
      b += (UInt32(k8[5])) shl 8;
      b += (UInt32(k8[6])) shl 16;
      b += (UInt32(k8[7])) shl 24;
      c += k8[8];
      c += (UInt32(k8[9])) shl 8;
      c += (UInt32(k8[10])) shl 16;
      c += (UInt32(k8[11])) shl 24;
      mix_abc;
      ALength -= 12;
      k8 += 12;
    end;

    case ALength of
      12: goto Case12;
      11: goto Case11;
      10: goto Case10;
      9 : goto Case9;
      8 : goto Case8;
      7 : goto Case7;
      6 : goto Case6;
      5 : goto Case5;
      4 : goto Case4;
      3 : goto Case3;
      2 : goto Case2;
      1 : goto Case1;
      0 : Exit(c);
    end;

    Case12: c+=(UInt32(k8[11])) shl 24;
    Case11: c+=(UInt32(k8[10])) shl 16;
    Case10: c+=(UInt32(k8[9])) shl 8;
    Case9: c+=k8[8];
    Case8: b+=(UInt32(k8[7])) shl 24;
    Case7: b+=(UInt32(k8[6])) shl 16;
    Case6: b+=(UInt32(k8[5])) shl 8;
    Case5: b+=k8[4];
    Case4: a+=(UInt32(k8[3])) shl 24;
    Case3: a+=(UInt32(k8[2])) shl 16;
    Case2: a+=(UInt32(k8[1])) shl 8;
    Case1: a+=k8[0];
  end;

  final_abc;
  Result := c;
end;

(*
 * hashlittle2: return 2 32-bit hash values
 *
 * This is identical to hashlittle(), except it returns two 32-bit hash
 * values instead of just one.  This is good enough for hash table
 * lookup with 2^^64 buckets, or if you want a second hash if you're not
 * happy with the first, or if you want a probably-unique 64-bit ID for
 * the key.  *pc is better mixed than *pb, so use *pc first.  If you want
 * a 64-bit value do something like "*pc + (((uint64_t)*pb)<<32)".
 *)
procedure HashLittle2(
  AKey: Pointer;        //* the key to hash */
  ALength: SizeInt;    //* length of the key */
  var APrimaryHashAndInitVal: UInt32;                      //* IN: primary initval, OUT: primary hash */
  var ASecondaryHashAndInitVal: UInt32);               //* IN: secondary initval, OUT: secondary hash */
var
  a,b,c: UInt32;
  u: record case byte of
    0: (ptr: Pointer);
    1: (i: PtrUint);
  end absolute AKey;

  k32: ^UInt32 absolute AKey;
  k16: ^UInt16 absolute AKey;
  k8: ^UInt8 absolute AKey;

label _10, _8, _6, _4, _2;
label Case12, Case11, Case10, Case9, Case8, Case7, Case6, Case5, Case4, Case3, Case2, Case1;

begin
  //* Set up the internal state */
  a := $DEADBEEF + UInt32(ALength) + APrimaryHashAndInitVal;
  b := a;
  c := b;
  c += ASecondaryHashAndInitVal;

{$IFDEF ENDIAN_LITTLE}
  if (u.i and $3) = 0 then
  begin
    while (ALength > 12) do
    begin
      a += k32[0];
      b += k32[1];
      c += k32[2];
      mix_abc;
      ALength -= 12;
      k32 += 3;
    end;

    case ALength of
      12: begin c += k32[2]; b += k32[1]; a += k32[0]; end;
      11: begin c += k32[2] and $ffffff; b += k32[1]; a += k32[0]; end;
      10: begin c += k32[2] and $ffff; b += k32[1]; a += k32[0]; end;
      9 : begin c += k32[2] and $ff; b += k32[1]; a += k32[0]; end;
      8 : begin b += k32[1]; a += k32[0]; end;
      7 : begin b += k32[1] and $ffffff; a += k32[0]; end;
      6 : begin b += k32[1] and $ffff; a += k32[0]; end;
      5 : begin b += k32[1] and $ff; a += k32[0]; end;
      4 : begin a += k32[0]; end;
      3 : begin a += k32[0] and $ffffff; end;
      2 : begin a += k32[0] and $ffff; end;
      1 : begin a += k32[0] and $ff; end;
      0 :
        begin
          APrimaryHashAndInitVal := c;
          ASecondaryHashAndInitVal := b;
          Exit;              // zero length strings require no mixing
        end;
    end
  end
  else
  if (u.i and $1) = 0 then
  begin
    while (ALength > 12) do
    begin
      a += k16[0] + (UInt32(k16[1]) shl 16);
      b += k16[2] + (UInt32(k16[3]) shl 16);
      c += k16[4] + (UInt32(k16[5]) shl 16);
      mix_abc;
      ALength -= 12;
      k16 += 6;
    end;

    case ALength of
      12:
        begin
          c+=k16[4]+((UInt32(k16[5])) shl 16);
          b+=k16[2]+((UInt32(k16[3])) shl 16);
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      11:
        begin
          c+=(UInt32(k8[10])) shl 16;     //* fall through */
          goto _10;
        end;
      10:
        begin _10:
          c+=k16[4];
          b+=k16[2]+((UInt32(k16[3])) shl 16);
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      9 :
        begin
          c+=k8[8];                      //* fall through */
          goto _8;
        end;
      8 :
        begin _8:
          b+=k16[2]+((UInt32(k16[3])) shl 16);
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      7 :
        begin
          b+=(UInt32(k8[6])) shl 16;      //* fall through */
          goto _6;
        end;
      6 :
        begin _6:
          b+=k16[2];
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      5 :
        begin
          b+=k8[4];                      //* fall through */
          goto _4;
        end;
      4 :
        begin _4:
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      3 :
        begin
          a+=(UInt32(k8[2])) shl 16;      //* fall through */
          goto _2;
        end;
      2 :
        begin _2:
          a+=k16[0];
        end;
      1 :
        begin
          a+=k8[0];
        end;
      0 :
        begin
          APrimaryHashAndInitVal := c;
          ASecondaryHashAndInitVal := b;
          Exit;              // zero length strings require no mixing
        end;
    end;
  end
  else
{$ENDIF}
  begin
    while ALength > 12 do
    begin
      a += k8[0];
      a += (UInt32(k8[1])) shl 8;
      a += (UInt32(k8[2])) shl 16;
      a += (UInt32(k8[3])) shl 24;
      b += k8[4];
      b += (UInt32(k8[5])) shl 8;
      b += (UInt32(k8[6])) shl 16;
      b += (UInt32(k8[7])) shl 24;
      c += k8[8];
      c += (UInt32(k8[9])) shl 8;
      c += (UInt32(k8[10])) shl 16;
      c += (UInt32(k8[11])) shl 24;
      mix_abc;
      ALength -= 12;
      k8 += 12;
    end;

    case ALength of
      12: goto Case12;
      11: goto Case11;
      10: goto Case10;
      9 : goto Case9;
      8 : goto Case8;
      7 : goto Case7;
      6 : goto Case6;
      5 : goto Case5;
      4 : goto Case4;
      3 : goto Case3;
      2 : goto Case2;
      1 : goto Case1;
      0 :
        begin
          APrimaryHashAndInitVal := c;
          ASecondaryHashAndInitVal := b;
          Exit;              // zero length strings require no mixing
        end;
    end;

    Case12: c+=(UInt32(k8[11])) shl 24;
    Case11: c+=(UInt32(k8[10])) shl 16;
    Case10: c+=(UInt32(k8[9])) shl 8;
    Case9: c+=k8[8];
    Case8: b+=(UInt32(k8[7])) shl 24;
    Case7: b+=(UInt32(k8[6])) shl 16;
    Case6: b+=(UInt32(k8[5])) shl 8;
    Case5: b+=k8[4];
    Case4: a+=(UInt32(k8[3])) shl 24;
    Case3: a+=(UInt32(k8[2])) shl 16;
    Case2: a+=(UInt32(k8[1])) shl 8;
    Case1: a+=k8[0];
  end;

  final_abc;
  APrimaryHashAndInitVal := c;
  ASecondaryHashAndInitVal := b;
end;

procedure DelphiHashLittle2(AKey: Pointer; ALength: SizeInt; var APrimaryHashAndInitVal, ASecondaryHashAndInitVal: UInt32);
var
  a,b,c: UInt32;
  u: record case byte of
    0: (ptr: Pointer);
    1: (i: PtrUint);
  end absolute AKey;

  k32: ^UInt32 absolute AKey;
  k16: ^UInt16 absolute AKey;
  k8: ^UInt8 absolute AKey;

label _10, _8, _6, _4, _2;
label Case12, Case11, Case10, Case9, Case8, Case7, Case6, Case5, Case4, Case3, Case2, Case1;

begin
  //* Set up the internal state */
  a := $DEADBEEF + UInt32(ALength shl 2) + APrimaryHashAndInitVal; // delphi version bug? original version don't have "shl 2"
  b := a;
  c := b;
  c += ASecondaryHashAndInitVal;

{$IFDEF ENDIAN_LITTLE}
  if (u.i and $3) = 0 then
  begin
    while (ALength > 12) do
    begin
      a += k32[0];
      b += k32[1];
      c += k32[2];
      mix_abc;
      ALength -= 12;
      k32 += 3;
    end;

    case ALength of
      12: begin c += k32[2]; b += k32[1]; a += k32[0]; end;
      11: begin c += k32[2] and $ffffff; b += k32[1]; a += k32[0]; end;
      10: begin c += k32[2] and $ffff; b += k32[1]; a += k32[0]; end;
      9 : begin c += k32[2] and $ff; b += k32[1]; a += k32[0]; end;
      8 : begin b += k32[1]; a += k32[0]; end;
      7 : begin b += k32[1] and $ffffff; a += k32[0]; end;
      6 : begin b += k32[1] and $ffff; a += k32[0]; end;
      5 : begin b += k32[1] and $ff; a += k32[0]; end;
      4 : begin a += k32[0]; end;
      3 : begin a += k32[0] and $ffffff; end;
      2 : begin a += k32[0] and $ffff; end;
      1 : begin a += k32[0] and $ff; end;
      0 :
        begin
          APrimaryHashAndInitVal := c;
          ASecondaryHashAndInitVal := b;
          Exit;              // zero length strings require no mixing
        end;
    end
  end
  else
  if (u.i and $1) = 0 then
  begin
    while (ALength > 12) do
    begin
      a += k16[0] + (UInt32(k16[1]) shl 16);
      b += k16[2] + (UInt32(k16[3]) shl 16);
      c += k16[4] + (UInt32(k16[5]) shl 16);
      mix_abc;
      ALength -= 12;
      k16 += 6;
    end;

    case ALength of
      12:
        begin
          c+=k16[4]+((UInt32(k16[5])) shl 16);
          b+=k16[2]+((UInt32(k16[3])) shl 16);
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      11:
        begin
          c+=(UInt32(k8[10])) shl 16;     //* fall through */
          goto _10;
        end;
      10:
        begin _10:
          c+=k16[4];
          b+=k16[2]+((UInt32(k16[3])) shl 16);
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      9 :
        begin
          c+=k8[8];                      //* fall through */
          goto _8;
        end;
      8 :
        begin _8:
          b+=k16[2]+((UInt32(k16[3])) shl 16);
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      7 :
        begin
          b+=(UInt32(k8[6])) shl 16;      //* fall through */
          goto _6;
        end;
      6 :
        begin _6:
          b+=k16[2];
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      5 :
        begin
          b+=k8[4];                      //* fall through */
          goto _4;
        end;
      4 :
        begin _4:
          a+=k16[0]+((UInt32(k16[1])) shl 16);
        end;
      3 :
        begin
          a+=(UInt32(k8[2])) shl 16;      //* fall through */
          goto _2;
        end;
      2 :
        begin _2:
          a+=k16[0];
        end;
      1 :
        begin
          a+=k8[0];
        end;
      0 :
        begin
          APrimaryHashAndInitVal := c;
          ASecondaryHashAndInitVal := b;
          Exit;              // zero length strings require no mixing
        end;
    end;
  end
  else
{$ENDIF}
  begin
    while ALength > 12 do
    begin
      a += k8[0];
      a += (UInt32(k8[1])) shl 8;
      a += (UInt32(k8[2])) shl 16;
      a += (UInt32(k8[3])) shl 24;
      b += k8[4];
      b += (UInt32(k8[5])) shl 8;
      b += (UInt32(k8[6])) shl 16;
      b += (UInt32(k8[7])) shl 24;
      c += k8[8];
      c += (UInt32(k8[9])) shl 8;
      c += (UInt32(k8[10])) shl 16;
      c += (UInt32(k8[11])) shl 24;
      mix_abc;
      ALength -= 12;
      k8 += 12;
    end;

    case ALength of
      12: goto Case12;
      11: goto Case11;
      10: goto Case10;
      9 : goto Case9;
      8 : goto Case8;
      7 : goto Case7;
      6 : goto Case6;
      5 : goto Case5;
      4 : goto Case4;
      3 : goto Case3;
      2 : goto Case2;
      1 : goto Case1;
      0 :
        begin
          APrimaryHashAndInitVal := c;
          ASecondaryHashAndInitVal := b;
          Exit;              // zero length strings require no mixing
        end;
    end;

    Case12: c+=(UInt32(k8[11])) shl 24;
    Case11: c+=(UInt32(k8[10])) shl 16;
    Case10: c+=(UInt32(k8[9])) shl 8;
    Case9: c+=k8[8];
    Case8: b+=(UInt32(k8[7])) shl 24;
    Case7: b+=(UInt32(k8[6])) shl 16;
    Case6: b+=(UInt32(k8[5])) shl 8;
    Case5: b+=k8[4];
    Case4: a+=(UInt32(k8[3])) shl 24;
    Case3: a+=(UInt32(k8[2])) shl 16;
    Case2: a+=(UInt32(k8[1])) shl 8;
    Case1: a+=k8[0];
  end;

  final_abc;
  APrimaryHashAndInitVal := c;
  ASecondaryHashAndInitVal := b;
end;

function DelphiHashLittle(AKey: Pointer; ALength: SizeInt; AInitVal: UInt32): Int32;
var
  a, b, c: UInt32;
  u: record case byte of
    0: (ptr: Pointer);
    1: (i: PtrUint);
  end absolute AKey;

  k32: ^UInt32 absolute AKey;
  //k16: ^UInt16 absolute AKey;
  k8: ^UInt8 absolute AKey;

label Case12, Case11, Case10, Case9, Case8, Case7, Case6, Case5, Case4, Case3, Case2, Case1;

begin
  a := $DEADBEEF + UInt32(ALength shl 2) + AInitVal; // delphi version bug? original version don't have "shl 2"
  b := a;
  c := b;

{.$IFDEF ENDIAN_LITTLE} // Delphi version don't care
  if (u.i and $3) = 0 then
  begin
    while (ALength > 12) do
    begin
      a += k32[0];
      b += k32[1];
      c += k32[2];
      mix_abc;
      ALength -= 12;
      k32 += 3;
    end;

    case ALength of
      12: begin c += k32[2]; b += k32[1]; a += k32[0]; end;
      11: begin c += k32[2] and $ffffff; b += k32[1]; a += k32[0]; end;
      10: begin c += k32[2] and $ffff; b += k32[1]; a += k32[0]; end;
      9 : begin c += k32[2] and $ff; b += k32[1]; a += k32[0]; end;
      8 : begin b += k32[1]; a += k32[0]; end;
      7 : begin b += k32[1] and $ffffff; a += k32[0]; end;
      6 : begin b += k32[1] and $ffff; a += k32[0]; end;
      5 : begin b += k32[1] and $ff; a += k32[0]; end;
      4 : begin a += k32[0]; end;
      3 : begin a += k32[0] and $ffffff; end;
      2 : begin a += k32[0] and $ffff; end;
      1 : begin a += k32[0] and $ff; end;
      0 : Exit(c);              // zero length strings require no mixing
    end
  end
  else
{.$ENDIF}
  begin
    while ALength > 12 do
    begin
      a += k8[0];
      a += (UInt32(k8[1])) shl 8;
      a += (UInt32(k8[2])) shl 16;
      a += (UInt32(k8[3])) shl 24;
      b += k8[4];
      b += (UInt32(k8[5])) shl 8;
      b += (UInt32(k8[6])) shl 16;
      b += (UInt32(k8[7])) shl 24;
      c += k8[8];
      c += (UInt32(k8[9])) shl 8;
      c += (UInt32(k8[10])) shl 16;
      c += (UInt32(k8[11])) shl 24;
      mix_abc;
      ALength -= 12;
      k8 += 12;
    end;

    case ALength of
      12: goto Case12;
      11: goto Case11;
      10: goto Case10;
      9 : goto Case9;
      8 : goto Case8;
      7 : goto Case7;
      6 : goto Case6;
      5 : goto Case5;
      4 : goto Case4;
      3 : goto Case3;
      2 : goto Case2;
      1 : goto Case1;
      0 : Exit(c);
    end;

    Case12: c+=(UInt32(k8[11])) shl 24;
    Case11: c+=(UInt32(k8[10])) shl 16;
    Case10: c+=(UInt32(k8[9])) shl 8;
    Case9: c+=k8[8];
    Case8: b+=(UInt32(k8[7])) shl 24;
    Case7: b+=(UInt32(k8[6])) shl 16;
    Case6: b+=(UInt32(k8[5])) shl 8;
    Case5: b+=k8[4];
    Case4: a+=(UInt32(k8[3])) shl 24;
    Case3: a+=(UInt32(k8[2])) shl 16;
    Case2: a+=(UInt32(k8[1])) shl 8;
    Case1: a+=k8[0];
  end;

  final_abc;
  Result := Int32(c);
end;

end.

