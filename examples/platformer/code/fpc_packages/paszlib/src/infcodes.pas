unit infcodes;

{ infcodes.c -- process literals and length/distance pairs
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

uses
  zbase;

function inflate_codes_new (bl : cardinal;
                            bd : cardinal;
                            tl : pInflate_huft;
                            td : pInflate_huft;
                            var z : z_stream): pInflate_codes_state;

function inflate_codes(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : integer) : integer;

procedure inflate_codes_free(var c : pInflate_codes_state;
                             var z : z_stream);

implementation

uses
  infutil, inffast{$IFDEF ZLIB_DEBUG}, SysUtils{$ENDIF};


function inflate_codes_new (bl : cardinal;
                            bd : cardinal;
                            tl : pInflate_huft;
                            td : pInflate_huft;
                            var z : z_stream): pInflate_codes_state;
var
 c : pInflate_codes_state;
begin
  new(c);
  if c<>nil then
  begin
    c^.mode := START;
    c^.lbits := Byte(bl);
    c^.dbits := Byte(bd);
    c^.ltree := tl;
    c^.dtree := td;
    {$IFDEF ZLIB_DEBUG}
    Tracev('inflate:       codes new');
    {$ENDIF}
  end;
  inflate_codes_new := c;
end;


function inflate_codes(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : integer) : integer;
var
  j : cardinal;               { temporary storage }
  t : pInflate_huft;      { temporary pointer }
  e : cardinal;               { extra bits or operation }
  b : cardinal;              { bit buffer }
  k : cardinal;               { bits in bit buffer }
  p : Pbyte;             { input data pointer }
  n : cardinal;               { bytes available there }
  q : Pbyte;             { output window write pointer }
  m : cardinal;               { bytes to end of window or read pointer }
  f : Pbyte;             { pointer to copy strings from }
var
  c : pInflate_codes_state;
begin
  c := s.sub.decode.codes;  { codes state }

  { copy input/output information to locals }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptruint(q) < ptruint(s.read) then
    m := cardinal(ptruint(s.read)-ptruint(q)-1)
  else
    m := cardinal(ptruint(s.zend)-ptruint(q));

  { process input and output based on current state }
  while True do
  case (c^.mode) of
    { waiting for "i:"=input, "o:"=output, "x:"=nothing }
  START:         { x: set up for LEN }
    begin
{$ifndef SLOW}
      if (m >= 258) and (n >= 10) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
        z.next_in := p;
        s.write := q;

        r := inflate_fast(c^.lbits, c^.dbits, c^.ltree, c^.dtree, s, z);
        {LOAD}
        p := z.next_in;
        n := z.avail_in;
        b := s.bitb;
        k := s.bitk;
        q := s.write;
        if ptruint(q) < ptruint(s.read) then
          m := cardinal(ptruint(s.read)-ptruint(q)-1)
        else
          m := cardinal(ptruint(s.zend)-ptruint(q));

        if (r <> Z_OK) then
        begin
          if (r = Z_STREAM_END) then
            c^.mode := WASH
          else
            c^.mode := BADCODE;
          continue;    { break for switch-statement in C }
        end;
      end;
{$endif} { not SLOW }
      c^.sub.code.need := c^.lbits;
      c^.sub.code.tree := c^.ltree;
      c^.mode := LEN;  { falltrough }
    end;
  LEN:           { i: get length/literal/eob next }
    begin
      j := c^.sub.code.need;
      {NEEDBITS(j);}
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
          Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);

          //if this is the last block, there are no bytes left in stream and the block end code follows, finish processing this block
          if s.last then
          begin
            t := c^.sub.code.tree;
            { update t (like as in following code), and check, if requested
              bits are available }
            Inc(t, cardinal(b) and inflate_mask[j]);
            if k >= t^.bits then
            { now, we can examine t^.exop value }
              if t^.exop and 32 <> 0 then
                break;
          end;

          exit;
        end;
        dec(n);
        b := b or (cardinal(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      t := c^.sub.code.tree;
      Inc(t, cardinal(b) and inflate_mask[j]);
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      dec(k, t^.bits);

      e := cardinal(t^.exop);
      if (e = 0) then            { literal }
      begin
        c^.sub.lit := t^.base;
       {$IFDEF ZLIB_DEBUG}
        if (t^.base >= $20) and (t^.base < $7f) then
          Tracevv('inflate:         literal '+char(t^.base))
        else
          Tracevv('inflate:         literal $'+IntToHex(t^.base, 2));
        {$ENDIF}          
        c^.mode := LIT;
        continue;  { break switch statement }
      end;
      if (e and 16 <> 0) then            { length }
      begin
        c^.sub.copy.get := e and 15;
        c^.len := t^.base;
        c^.mode := LENEXT;
        continue;         { break C-switch statement }
      end;
      if (e and 64 = 0) then             { next table }
      begin
        c^.sub.code.need := e;
        c^.sub.code.tree := @huft_ptr(t)^[t^.base];
        continue;         { break C-switch statement }
      end;
      if (e and 32 <> 0) then            { end of block }
      begin
        {$IFDEF ZLIB_DEBUG}
        Tracevv('inflate:         end of block');
        {$ENDIF}        
        c^.mode := WASH;
        continue;         { break C-switch statement }
      end;
      c^.mode := BADCODE;        { invalid code }
      z.msg := 'invalid literal/length code';
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  LENEXT:        { i: getting length extra (have base) }
    begin
      j := c^.sub.copy.get;
      {NEEDBITS(j);}
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
          Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        dec(n);
        b := b or (cardinal(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      Inc(c^.len, cardinal(b and inflate_mask[j]));
      {DUMPBITS(j);}
      b := b shr j;
      dec(k, j);

      c^.sub.code.need := c^.dbits;
      c^.sub.code.tree := c^.dtree;
      {$IFDEF ZLIB_DEBUG}
      Tracevv('inflate:         length '+IntToStr(c^.len));
      {$ENDIF}
      c^.mode := DIST;
      { falltrough }
    end;
  DIST:          { i: get distance next }
    begin
      j := c^.sub.code.need;
      {NEEDBITS(j);}
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
          Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        dec(n);
        b := b or (cardinal(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      t := @huft_ptr(c^.sub.code.tree)^[cardinal(b) and inflate_mask[j]];
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      dec(k, t^.bits);

      e := cardinal(t^.exop);
      if (e and 16 <> 0) then            { distance }
      begin
        c^.sub.copy.get := e and 15;
        c^.sub.copy.dist := t^.base;
        c^.mode := DISTEXT;
        continue;     { break C-switch statement }
      end;
      if (e and 64 = 0) then     { next table }
      begin
        c^.sub.code.need := e;
        c^.sub.code.tree := @huft_ptr(t)^[t^.base];
        continue;     { break C-switch statement }
      end;
      c^.mode := BADCODE;        { invalid code }
      z.msg := 'invalid distance code';
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  DISTEXT:       { i: getting distance extra }
    begin
      j := c^.sub.copy.get;
      {NEEDBITS(j);}
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
          Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        dec(n);
        b := b or (cardinal(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      Inc(c^.sub.copy.dist, cardinal(b) and inflate_mask[j]);
      {DUMPBITS(j);}
      b := b shr j;
      dec(k, j);
      {$IFDEF ZLIB_DEBUG}
      Tracevv('inflate:         distance '+ IntToStr(c^.sub.copy.dist));
      {$ENDIF}
      c^.mode := COPY;
      { falltrough }
    end;
  COPY:          { o: copying bytes in window, waiting for space }
    begin
      f := q;
      dec(f, c^.sub.copy.dist);
      if (cardinal(ptruint(q) - ptruint(s.window)) < c^.sub.copy.dist) then
      begin
        f := s.zend;
        dec(f, c^.sub.copy.dist - cardinal(ptruint(q) - ptruint(s.window)));
      end;

      while (c^.len <> 0) do
      begin
        {NEEDOUT}
        if (m = 0) then
        begin
          {WRAP}
          if (q = s.zend) and (s.read <> s.window) then
          begin
            q := s.window;
            if ptruint(q) < ptruint(s.read) then
              m := cardinal(ptruint(s.read)-ptruint(q)-1)
            else
              m := cardinal(ptruint(s.zend)-ptruint(q));
          end;

          if (m = 0) then
          begin
            {FLUSH}
            s.write := q;
            r := inflate_flush(s,z,r);
            q := s.write;
            if ptruint(q) < ptruint(s.read) then
              m := cardinal(ptruint(s.read)-ptruint(q)-1)
            else
              m := cardinal(ptruint(s.zend)-ptruint(q));

            {WRAP}
            if (q = s.zend) and (s.read <> s.window) then
            begin
              q := s.window;
              if ptruint(q) < ptruint(s.read) then
                m := cardinal(ptruint(s.read)-ptruint(q)-1)
              else
                m := cardinal(ptruint(s.zend)-ptruint(q));
            end;

            if (m = 0) then
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_codes := inflate_flush(s,z,r);
              exit;
            end;
          end;
        end;
        r := Z_OK;

        {OUTBYTE( *f++)}
        q^ := f^;
        Inc(q);
        Inc(f);
        dec(m);

        if (f = s.zend) then
          f := s.window;
        dec(c^.len);
      end;
      c^.mode := START;
      { C-switch break; not needed }
    end;
  LIT:           { o: got literal, waiting for output space }
    begin
      {NEEDOUT}
      if (m = 0) then
      begin
        {WRAP}
        if (q = s.zend) and (s.read <> s.window) then
        begin
          q := s.window;
          if ptruint(q) < ptruint(s.read) then
            m := cardinal(ptruint(s.read)-ptruint(q)-1)
          else
            m := cardinal(ptruint(s.zend)-ptruint(q));
        end;

        if (m = 0) then
        begin
          {FLUSH}
          s.write := q;
          r := inflate_flush(s,z,r);
          q := s.write;
          if ptruint(q) < ptruint(s.read) then
            m := cardinal(ptruint(s.read)-ptruint(q)-1)
          else
            m := cardinal(ptruint(s.zend)-ptruint(q));

          {WRAP}
          if (q = s.zend) and (s.read <> s.window) then
          begin
            q := s.window;
            if ptruint(q) < ptruint(s.read) then
              m := cardinal(ptruint(s.read)-ptruint(q)-1)
            else
              m := cardinal(ptruint(s.zend)-ptruint(q));
          end;

          if (m = 0) then
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_codes := inflate_flush(s,z,r);
            exit;
          end;
        end;
      end;
      r := Z_OK;

      {OUTBYTE(c^.sub.lit);}
      q^ := c^.sub.lit;
      Inc(q);
      dec(m);

      c^.mode := START;
      {break;}
    end;
  WASH:          { o: got eob, possibly more output }
    begin
      {$ifdef patch112}
      if (k > 7) then           { return unused byte, if any }
      begin
        {$IFDEF ZLIB_DEBUG}
        Assert(k < 16, 'inflate_codes grabbed too many bytes');
        {$ENDIF}
        dec(k, 8);
        Inc(n);
        dec(p);                    { can always return one }
      end;
      {$endif}
      {FLUSH}
      s.write := q;
      r := inflate_flush(s,z,r);
      q := s.write;
      if ptruint(q) < ptruint(s.read) then
        m := cardinal(ptruint(s.read)-ptruint(q)-1)
      else
        m := cardinal(ptruint(s.zend)-ptruint(q));

      if (s.read <> s.write) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
        z.next_in := p;
        s.write := q;
        inflate_codes := inflate_flush(s,z,r);
        exit;
      end;
      c^.mode := ZEND;
      { falltrough }
    end;

  ZEND:
    begin
      r := Z_STREAM_END;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  BADCODE:       { x: got error }
    begin
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  else
    begin
      r := Z_STREAM_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptruint(p)-ptruint(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  end;
{NEED_DUMMY_RETURN - Delphi2+ dumb compilers complain without this }
  inflate_codes := Z_STREAM_ERROR;
end;


procedure inflate_codes_free(var c : pInflate_codes_state;
                             var z : z_stream);
begin
  dispose(c);
  c := nil;
  {$IFDEF ZLIB_DEBUG}  
  Tracev('inflate:       codes free');
  {$ENDIF}
end;

end.