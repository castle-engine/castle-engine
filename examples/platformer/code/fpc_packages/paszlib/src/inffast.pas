Unit InfFast;

{
  inffast.h and
  inffast.c -- process literals and length/distance pairs fast
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}


interface

{$I zconf.inc}

uses
  zbase;

function inflate_fast( bl : cardinal;
                       bd : cardinal;
                       tl : pInflate_huft;
                       td : pInflate_huft;
                      var s : inflate_blocks_state;
                      var z : z_stream) : integer;


implementation

uses
  infutil{$IFDEF ZLIB_DEBUG}, SysUtils{$ENDIF};


{ Called with number of bytes left to write in window at least 258
  (the maximum string length) and number of input bytes available
  at least ten.  The ten bytes are six bytes for the longest length/
  distance pair plus four bytes for overloading the bit buffer. }

function inflate_fast( bl : cardinal;
                       bd : cardinal;
                       tl : pInflate_huft;
                       td : pInflate_huft;
                      var s : inflate_blocks_state;
                      var z : z_stream) : integer;

var
  t : pInflate_huft;      { temporary pointer }
  e : cardinal;               { extra bits or operation }
  b : longint;              { bit buffer }
  k : cardinal;               { bits in bit buffer }
  p : Pbyte;             { input data pointer }
  n : cardinal;               { bytes available there }
  q : Pbyte;             { output window write pointer }
  m : cardinal;               { bytes to end of window or read pointer }
  ml : cardinal;              { mask for literal/length tree }
  md : cardinal;              { mask for distance tree }
  c : cardinal;               { bytes to copy }
  d : cardinal;               { distance back to copy from }
  r : Pbyte;             { copy source pointer }
begin
  { load input, output, bit values (macro LOAD) }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptruint(q) < ptruint(s.read) then
    m := cardinal(ptruint(s.read)-ptruint(q)-1)
  else
    m := cardinal(ptruint(s.zend)-ptruint(q));

  { initialize masks }
  ml := inflate_mask[bl];
  md := inflate_mask[bd];

  { do until not enough input or output space for fast loop }
  repeat                      { assume called with (m >= 258) and (n >= 10) }
    { get literal/length code }
    {GRABBITS(20);}             { max bits for literal/length code }
    while (k < 20) do
    begin
      dec(n);
      b := b or (longint(p^) shl k);
      inc(p);
      inc(k, 8);
    end;

    t := @(huft_ptr(tl)^[cardinal(b) and ml]);

    e := t^.exop;
    if (e = 0) then
    begin
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      dec(k, t^.bits);
     {$IFDEF ZLIB_DEBUG}
      if (t^.base >= $20) and (t^.base < $7f) then
        Tracevv('inflate:         * literal '+char(t^.base))
      else
        Tracevv('inflate:         * literal '+ IntToStr(t^.base));
      {$ENDIF}
      q^ := Byte(t^.base);
      inc(q);
      dec(m);
      continue;
    end;
    repeat
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      dec(k, t^.bits);

      if (e and 16 <> 0) then
      begin
        { get extra bits for length }
        e := e and 15;
        c := t^.base + (cardinal(b) and inflate_mask[e]);
        {DUMPBITS(e);}
        b := b shr e;
        dec(k, e);
        {$IFDEF ZLIB_DEBUG}
        Tracevv('inflate:         * length ' + IntToStr(c));
        {$ENDIF}
        { decode distance base of block to copy }
        {GRABBITS(15);}           { max bits for distance code }
        while (k < 15) do
        begin
          dec(n);
          b := b or (longint(p^) shl k);
          inc(p);
          inc(k, 8);
        end;

        t := @huft_ptr(td)^[cardinal(b) and md];
        e := t^.exop;
        repeat
          {DUMPBITS(t^.bits);}
          b := b shr t^.bits;
          dec(k, t^.bits);

          if (e and 16 <> 0) then
          begin
            { get extra bits to add to distance base }
            e := e and 15;
            {GRABBITS(e);}         { get extra bits (up to 13) }
            while (k < e) do
            begin
              dec(n);
              b := b or (longint(p^) shl k);
              inc(p);
              inc(k, 8);
            end;

            d := t^.base + (cardinal(b) and inflate_mask[e]);
            {DUMPBITS(e);}
            b := b shr e;
            dec(k, e);

            {$IFDEF ZLIB_DEBUG}
            Tracevv('inflate:         * distance '+IntToStr(d));
            {$ENDIF}
            { do the copy }
            dec(m, c);
            if (cardinal(ptruint(q) - ptruint(s.window)) >= d) then     { offset before dest }
            begin                                  {  just copy }
              r := q;
              dec(r, d);
              q^ := r^;  inc(q); inc(r); dec(c); { minimum count is three, }
              q^ := r^;  inc(q); inc(r); dec(c); { so unroll loop a little }
            end
            else                        { else offset after destination }
            begin
              e := d - cardinal(ptruint(q) - ptruint(s.window)); { bytes from offset to end }
              r := s.zend;
              dec(r, e);                  { pointer to offset }
              if (c > e) then             { if source crosses, }
              begin
                dec(c, e);                { copy to end of window }
                repeat
                  q^ := r^;
                  inc(q);
                  inc(r);
                  dec(e);
                until (e=0);
                r := s.window;           { copy rest from start of window }
              end;
            end;
            repeat                       { copy all or what's left }
              q^ := r^;
              inc(q);
              inc(r);
              dec(c);
            until (c = 0);
            break;
          end
          else
            if (e and 64 = 0) then
            begin
              inc(t, t^.base + (cardinal(b) and inflate_mask[e]));
              e := t^.exop;
            end
          else
          begin
            z.msg := 'invalid distance code';
            {UNGRAB}
            c := z.avail_in-n;
            if (k shr 3) < c then
              c := k shr 3;
            inc(n, c);
            dec(p, c);
            dec(k, c shl 3);
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            inc(z.total_in, ptruint(p)-ptruint(z.next_in));
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
          e = (t += ((cardinal)b & inflate_mask[e]))->exop;}

        inc(t, t^.base + (cardinal(b) and inflate_mask[e]));
        e := t^.exop;
        if (e = 0) then
        begin
          {DUMPBITS(t^.bits);}
          b := b shr t^.bits;
          dec(k, t^.bits);

         {$IFDEF ZLIB_DEBUG}
          if (t^.base >= $20) and (t^.base < $7f) then
            Tracevv('inflate:         * literal '+char(t^.base))
          else
            Tracevv('inflate:         * literal '+IntToStr(t^.base));
          {$ENDIF}            
          q^ := Byte(t^.base);
          inc(q);
          dec(m);
          break;
        end;
      end
      else
        if (e and 32 <> 0) then
        begin
          {$IFDEF ZLIB_DEBUG}
          Tracevv('inflate:         * end of block');
          {$ENDIF}
          {UNGRAB}
          c := z.avail_in-n;
          if (k shr 3) < c then
            c := k shr 3;
          inc(n, c);
          dec(p, c);
          dec(k, c shl 3);
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          inc(z.total_in, ptruint(p)-ptruint(z.next_in));
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
          inc(n, c);
          dec(p, c);
          dec(k, c shl 3);
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          inc(z.total_in, ptruint(p)-ptruint(z.next_in));
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
  inc(n, c);
  dec(p, c);
  dec(k, c shl 3);
  {UPDATE}
  s.bitb := b;
  s.bitk := k;
  z.avail_in := n;
  inc(z.total_in, ptruint(p)-ptruint(z.next_in));
  z.next_in := p;
  s.write := q;
  inflate_fast := Z_OK;
end;

end.