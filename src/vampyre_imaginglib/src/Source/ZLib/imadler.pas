Unit imadler;

{
  adler32.c -- compute the Adler-32 checksum of a data stream
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I imzconf.inc}

uses
  imzutil;

function adler32(adler : uLong; buf : pBytef; len : uInt) : uLong;

{    Update a running Adler-32 checksum with the bytes buf[0..len-1] and
   return the updated checksum. If buf is NIL, this function returns
   the required initial value for the checksum.
   An Adler-32 checksum is almost as reliable as a CRC32 but can be computed
   much faster. Usage example:

   var
     adler : uLong;
   begin
     adler := adler32(0, Z_NULL, 0);

     while (read_buffer(buffer, length) <> EOF) do
       adler := adler32(adler, buffer, length);

     if (adler <> original_adler) then
       error();
   end;
}

implementation

const
  BASE = uLong(65521); { largest prime smaller than 65536 }
  {NMAX = 5552; original code with unsigned 32 bit integer }
  { NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 }
  NMAX = 3854;        { code with signed 32 bit integer }
  { NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^31-1 }
  { The penalty is the time loss in the extra MOD-calls. }


{ ========================================================================= }

function adler32(adler : uLong; buf : pBytef; len : uInt) : uLong;
var
  s1, s2 : uLong;
  k : int;
begin
  s1 := adler and $ffff;
  s2 := (adler shr 16) and $ffff;

  if not Assigned(buf) then
  begin
    adler32 := uLong(1);
    exit;
  end;

  while (len > 0) do
  begin
    if len < NMAX then
      k := len
    else
      k := NMAX;
    Dec(len, k);
    {
    while (k >= 16) do
    begin
      DO16(buf);
      Inc(buf, 16);
      Dec(k, 16);
    end;
    if (k <> 0) then
    repeat
      Inc(s1, buf^);
      Inc(puf);
      Inc(s2, s1);
      Dec(k);
    until (k = 0);
    }
    while (k > 0) do
    begin
      Inc(s1, buf^);
      Inc(s2, s1);
      Inc(buf);
      Dec(k);
    end;
    s1 := s1 mod BASE;
    s2 := s2 mod BASE;
  end;
  adler32 := (s2 shl 16) or s1;
end;

{
#define DO1(buf,i)
  begin
    Inc(s1, buf[i]);
    Inc(s2, s1);
  end;
#define DO2(buf,i)  DO1(buf,i); DO1(buf,i+1);
#define DO4(buf,i)  DO2(buf,i); DO2(buf,i+2);
#define DO8(buf,i)  DO4(buf,i); DO4(buf,i+4);
#define DO16(buf)   DO8(buf,0); DO8(buf,8);
}
end.

