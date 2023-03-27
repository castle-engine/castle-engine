Unit infutil;

{ types and macros common to blocks and codes
  Copyright (C) 1995-1998 Mark Adler

   WARNING: this file should *not* be used by applications. It is
   part of the implementation of the compression library and is
   subject to change.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

uses
  zbase;

{ copy as much as possible from the sliding window to the output area }
function inflate_flush(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : integer) : integer;

{ And'ing with mask[n] masks the lower n bits }
const
  inflate_mask : array[0..17-1] of cardinal = (
    $0000,
    $0001, $0003, $0007, $000f, $001f, $003f, $007f, $00ff,
    $01ff, $03ff, $07ff, $0fff, $1fff, $3fff, $7fff, $ffff);

{procedure GRABBITS(j : integer);}
{procedure DUMPBITS(j : integer);}
{procedure NEEDBITS(j : integer);}

implementation

{ macros for bit input with no checking and for returning unused bytes }
procedure GRABBITS(j : integer);
begin
  {while (k < j) do
  begin
    dec(z^.avail_in);
    inc(z^.total_in);
    b := b or (uLong(z^.next_in^) shl k);
    inc(z^.next_in);
    inc(k, 8);
  end;}
end;

procedure DUMPBITS(j : integer);
begin
  {b := b shr j;
  dec(k, j);}
end;

procedure NEEDBITS(j : integer);
begin
 (*
          while (k < j) do
          begin
            {NEEDBYTE;}
            if (n <> 0) then
              r :=Z_OK
            else
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              inc(z.total_in, LongInt(p)-LongInt(z.next_in));
              z.next_in := p;
              s.write := q;
              result := inflate_flush(s,z,r);
              exit;
            end;
            dec(n);
            b := b or (uLong(p^) shl k);
            inc(p);
            inc(k, 8);
          end;
 *)
end;

procedure NEEDOUT;
begin
 (*
  if (m = 0) then
  begin
    {WRAP}
    if (q = s.zend) and (s.read <> s.window) then
    begin
      q := s.window;
      if LongInt(q) < LongInt(s.read) then
        m := cardinal(LongInt(s.read)-LongInt(q)-1)
      else
        m := cardinal(LongInt(s.zend)-LongInt(q));
    end;

    if (m = 0) then
    begin
      {FLUSH}
      s.write := q;
      r := inflate_flush(s,z,r);
      q := s.write;
      if LongInt(q) < LongInt(s.read) then
        m := cardinal(LongInt(s.read)-LongInt(q)-1)
      else
        m := cardinal(LongInt(s.zend)-LongInt(q));

      {WRAP}
      if (q = s.zend) and (s.read <> s.window) then
      begin
        q := s.window;
        if LongInt(q) < LongInt(s.read) then
          m := cardinal(LongInt(s.read)-LongInt(q)-1)
        else
          m := cardinal(LongInt(s.zend)-LongInt(q));
      end;

      if (m = 0) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        inc(z.total_in, LongInt(p)-LongInt(z.next_in));
        z.next_in := p;
        s.write := q;
        result := inflate_flush(s,z,r);
        exit;
      end;
    end;
  end;
  r := Z_OK;
 *)
end;

{ copy as much as possible from the sliding window to the output area }
function inflate_flush(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : integer) : integer;
var
  n : cardinal;
  p : Pbyte;
  q : Pbyte;
begin
  { local copies of source and destination pointers }
  p := z.next_out;
  q := s.read;

  { compute number of bytes to copy as far as end of window }
  if ptruint(q) <= ptruint(s.write) then
    n := cardinal(ptruint(s.write) - ptruint(q))
  else
    n := cardinal(ptruint(s.zend) - ptruint(q));
  if (n > z.avail_out) then
    n := z.avail_out;
  if (n <> 0) and (r = Z_BUF_ERROR) then
    r := Z_OK;

  { update counters }
  dec(z.avail_out, n);
  inc(z.total_out, n);


  { update check information }
  if Assigned(s.checkfn) then
  begin
    s.check := s.checkfn(s.check, q, n);
    z.adler := s.check;
  end;

  { copy as far as end of window }
  move(q^,p^,n);
  inc(p, n);
  inc(q, n);

  { see if more to copy at beginning of window }
  if (q = s.zend) then
  begin
    { wrap pointers }
    q := s.window;
    if (s.write = s.zend) then
      s.write := s.window;

    { compute bytes to copy }
    n := cardinal(ptruint(s.write) - ptruint(q));
    if (n > z.avail_out) then
      n := z.avail_out;
    if (n <> 0) and (r = Z_BUF_ERROR) then
      r := Z_OK;

    { update counters }
    dec( z.avail_out, n);
    inc( z.total_out, n);

    { update check information }
    if Assigned(s.checkfn) then
    begin
      s.check := s.checkfn(s.check, q, n);
      z.adler := s.check;
    end;

    { copy }
    move(q^,p^,n);
    inc(p, n);
    inc(q, n);
  end;


  { update pointers }
  z.next_out := p;
  s.read := q;

  { done }
  inflate_flush := r;
end;

end.
