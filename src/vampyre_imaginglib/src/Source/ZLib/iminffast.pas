Unit iminffast;

{
  inffast.h and
  inffast.c -- process literals and length/distance pairs fast
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}


interface

{$I imzconf.inc}

uses
  {$ifdef DEBUG}
  SysUtils, strutils,
  {$ENDIF}
  imzutil, impaszlib;

function inflate_fast( bl : uInt;
                       bd : uInt;
                       tl : pInflate_huft;
                       td : pInflate_huft;
                      var s : inflate_blocks_state;
                      var z : z_stream) : int;


implementation

uses
  iminfutil;


{ Called with number of bytes left to write in window at least 258
  (the maximum string length) and number of input bytes available
  at least ten.  The ten bytes are six bytes for the longest length/
  distance pair plus four bytes for overloading the bit buffer. }

function inflate_fast( bl : uInt;
                       bd : uInt;
                       tl : pInflate_huft;
                       td : pInflate_huft;
                      var s : inflate_blocks_state;
                      var z : z_stream) : int;

var
  t : pInflate_huft;      { temporary pointer }
  e : uInt;               { extra bits or operation }
  b : uLong;              { bit buffer }
  k : uInt;               { bits in bit buffer }
  p : pBytef;             { input data pointer }
  n : uInt;               { bytes available there }
  q : pBytef;             { output window write pointer }
  m : uInt;               { bytes to end of window or read pointer }
  ml : uInt;              { mask for literal/length tree }
  md : uInt;              { mask for distance tree }
  c : uInt;               { bytes to copy }
  d : uInt;               { distance back to copy from }
  r : pBytef;             { copy source pointer }
begin
  { load input, output, bit values (macro LOAD) }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));

  { initialize masks }
  ml := inflate_mask[bl];
  md := inflate_mask[bd];

  { do until not enough input or output space for fast loop }
  repeat                      { assume called with (m >= 258) and (n >= 10) }
    { get literal/length code }
    {GRABBITS(20);}             { max bits for literal/length code }
    while (k < 20) do
    begin
      Dec(n);
      b := b or (uLong(p^) shl k);
      Inc(p);
      Inc(k, 8);
    end;

    t := @(huft_ptr(tl)^[uInt(b) and ml]);

    e := t^.exop;
    if (e = 0) then
    begin
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);
     {$IFDEF DEBUG}
      if (t^.base >= $20) and (t^.base < $7f) then
        Tracevv('inflate:         * literal '+AnsiChar(t^.base))
      else
        Tracevv('inflate:         * literal '+ IntToStr(t^.base));
      {$ENDIF}
      q^ := Byte(t^.base);
      Inc(q);
      Dec(m);
      continue;
    end;
    repeat
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);

      if (e and 16 <> 0) then
      begin
        { get extra bits for length }
        e := e and 15;
        c := t^.base + (uInt(b) and inflate_mask[e]);
        {DUMPBITS(e);}
        b := b shr e;
        Dec(k, e);
        {$IFDEF DEBUG}
        Tracevv('inflate:         * length ' + IntToStr(c));
        {$ENDIF}
        { decode distance base of block to copy }
        {GRABBITS(15);}           { max bits for distance code }
        while (k < 15) do
        begin
          Dec(n);
          b := b or (uLong(p^) shl k);
          Inc(p);
          Inc(k, 8);
        end;

        t := @huft_ptr(td)^[uInt(b) and md];
        e := t^.exop;
        repeat
          {DUMPBITS(t^.bits);}
          b := b shr t^.bits;
          Dec(k, t^.bits);

          if (e and 16 <> 0) then
          begin
            { get extra bits to add to distance base }
            e := e and 15;
            {GRABBITS(e);}         { get extra bits (up to 13) }
            while (k < e) do
            begin
              Dec(n);
              b := b or (uLong(p^) shl k);
              Inc(p);
              Inc(k, 8);
            end;

            d := t^.base + (uInt(b) and inflate_mask[e]);
            {DUMPBITS(e);}
            b := b shr e;
            Dec(k, e);

            {$IFDEF DEBUG}
            Tracevv('inflate:         * distance '+IntToStr(d));
            {$ENDIF}
            { do the copy }
            Dec(m, c);
            if (uInt(ptr2int(q) - ptr2int(s.window)) >= d) then     { offset before dest }
            begin                                  {  just copy }
              r := q;
              Dec(r, d);
              q^ := r^;  Inc(q); Inc(r); Dec(c); { minimum count is three, }
              q^ := r^;  Inc(q); Inc(r); Dec(c); { so unroll loop a little }
            end
            else                        { else offset after destination }
            begin
              e := d - uInt(ptr2int(q) - ptr2int(s.window)); { bytes from offset to end }
              r := s.zend;
              Dec(r, e);                  { pointer to offset }
              if (c > e) then             { if source crosses, }
              begin
                Dec(c, e);                { copy to end of window }
                repeat
                  q^ := r^;
                  Inc(q);
                  Inc(r);
                  Dec(e);
                until (e=0);
                r := s.window;           { copy rest from start of window }
              end;
            end;
            repeat                       { copy all or what's left }
              q^ := r^;
              Inc(q);
              Inc(r);
              Dec(c);
            until (c = 0);
            break;
          end
          else
            if (e and 64 = 0) then
            begin
              Inc(t, t^.base + (uInt(b) and inflate_mask[e]));
              e := t^.exop;
            end
          else
          begin
            z.msg := 'invalid distance code';
            {UNGRAB}
            c := z.avail_in-n;
            if (k shr 3) < c then
              c := k shr 3;
            Inc(n, c);
            Dec(p, c);
            Dec(k, c shl 3);
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;

            inflate_fast := Z_DATA_ERROR;
            exit;
          end;
        until FALSE;
        break;
      end;
      if (e and 64 = 0) then
      begin
         {t += t->base;
          e = (t += ((uInt)b & inflate_mask[e]))->exop;}

        Inc(t, t^.base + (uInt(b) and inflate_mask[e]));
        e := t^.exop;
        if (e = 0) then
        begin
          {DUMPBITS(t^.bits);}
          b := b shr t^.bits;
          Dec(k, t^.bits);

         {$IFDEF DEBUG}
          if (t^.base >= $20) and (t^.base < $7f) then
            Tracevv('inflate:         * literal '+AnsiChar(t^.base))
          else
            Tracevv('inflate:         * literal '+IntToStr(t^.base));
          {$ENDIF}            
          q^ := Byte(t^.base);
          Inc(q);
          Dec(m);
          break;
        end;
      end
      else
        if (e and 32 <> 0) then
        begin
          {$IFDEF DEBUG}
          Tracevv('inflate:         * end of block');
          {$ENDIF}
          {UNGRAB}
          c := z.avail_in-n;
          if (k shr 3) < c then
            c := k shr 3;
          Inc(n, c);
          Dec(p, c);
          Dec(k, c shl 3);
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_fast := Z_STREAM_END;
          exit;
        end
        else
        begin
          z.msg := 'invalid literal/length code';
          {UNGRAB}
          c := z.avail_in-n;
          if (k shr 3) < c then
            c := k shr 3;
          Inc(n, c);
          Dec(p, c);
          Dec(k, c shl 3);
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_fast := Z_DATA_ERROR;
          exit;
        end;
    until FALSE;
  until (m < 258) or (n < 10);

  { not enough input or output--restore pointers and return }
  {UNGRAB}
  c := z.avail_in-n;
  if (k shr 3) < c then
    c := k shr 3;
  Inc(n, c);
  Dec(p, c);
  Dec(k, c shl 3);
  {UPDATE}
  s.bitb := b;
  s.bitk := k;
  z.avail_in := n;
  Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
  z.next_in := p;
  s.write := q;
  inflate_fast := Z_OK;
end;

end.
