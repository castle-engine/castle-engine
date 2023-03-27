unit inftrees;

{ inftrees.h -- header to use inftrees.c
  inftrees.c -- generate Huffman trees for efficient decoding
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


{ Maximum size of dynamic tree.  The maximum found in a long but non-
  exhaustive search was 1004 huft structures (850 for length/literals
  and 154 for distances, the latter actually the result of an
  exhaustive search).  The actual maximum is not known, but the
  value below is more than safe. }
const
  MANY = 1440;


{$ifdef ZLIB_DEBUG}
var
  inflate_hufts : cardinal;
{$endif}

function inflate_trees_bits(
  var c : array of cardinal;  { 19 code lengths }
  var bb : cardinal;          { bits tree desired/actual depth }
  var tb : pinflate_huft;  { bits tree result }
  var hp : array of Inflate_huft;      { space for trees }
  var z : z_stream         { for messages }
    ) : integer;

function inflate_trees_dynamic(
    nl : cardinal;                    { number of literal/length codes }
    nd : cardinal;                    { number of distance codes }
    var c : Array of cardinal;           { that many (total) code lengths }
    var bl : cardinal;               { literal desired/actual bit depth }
    var bd : cardinal;               { distance desired/actual bit depth }
var tl : pInflate_huft;           { literal/length tree result }
var td : pInflate_huft;           { distance tree result }
var hp : array of Inflate_huft;   { space for trees }
var z : z_stream                  { for messages }
     ) : integer;

function inflate_trees_fixed (
    var bl : cardinal;                { literal desired/actual bit depth }
    var bd : cardinal;                { distance desired/actual bit depth }
    var tl : pInflate_huft;       { literal/length tree result }
    var td : pInflate_huft;       { distance tree result }
    var z : z_stream              { for memory allocation }
     ) : integer;


implementation

const
 inflate_copyright = 'inflate 1.1.2 Copyright 1995-1998 Mark Adler';

{
  If you use the zlib library in a product, an acknowledgment is welcome
  in the documentation of your product. If for some reason you cannot
  include such an acknowledgment, I would appreciate that you keep this
  copyright string in the executable of your product.
}


const
{ Tables for deflate from PKZIP's appnote.txt. }
  cplens : Array [0..30] Of cardinal  { Copy lengths for literal codes 257..285 }
     = (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);
        { actually lengths - 2; also see note #13 above about 258 }

  invalid_code = 112;

  cplext : Array [0..30] Of cardinal  { Extra bits for literal codes 257..285 }
     = (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, invalid_code, invalid_code);

  cpdist : Array [0..29] Of cardinal { Copy offsets for distance codes 0..29 }
     = (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577);

  cpdext : Array [0..29] Of cardinal { Extra bits for distance codes }
     = (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13);

{  Huffman code decoding is performed using a multi-level table lookup.
   The fastest way to decode is to simply build a lookup table whose
   size is determined by the longest code.  However, the time it takes
   to build this table can also be a factor if the data being decoded
   is not very long.  The most common codes are necessarily the
   shortest codes, so those codes dominate the decoding time, and hence
   the speed.  The idea is you can have a shorter table that decodes the
   shorter, more probable codes, and then point to subsidiary tables for
   the longer codes.  The time it costs to decode the longer codes is
   then traded against the time it takes to make longer tables.

   This results of this trade are in the variables lbits and dbits
   below.  lbits is the number of bits the first level table for literal/
   length codes can decode in one step, and dbits is the same thing for
   the distance codes.  Subsequent tables are also less than or equal to
   those sizes.  These values may be adjusted either when all of the
   codes are shorter than that, in which case the longest code length in
   bits is used, or when the shortest code is *longer* than the requested
   table size, in which case the length of the shortest code in bits is
   used.

   There are two different values for the two tables, since they code a
   different number of possibilities each.  The literal/length table
   codes 286 possible values, or in a flat code, a little over eight
   bits.  The distance table codes 30 possible values, or a little less
   than five bits, flat.  The optimum values for speed end up being
   about one bit more than those, so lbits is 8+1 and dbits is 5+1.
   The optimum values may differ though from machine to machine, and
   possibly even between compilers.  Your mileage may vary. }


{ If BMAX needs to be larger than 16, then h and x[] should be uLong. }
const
  BMAX = 15;         { maximum bit length of any code }

{$DEFINE USE_PTR}

function huft_build(
var b : array of cardinal;    { code lengths in bits (all assumed <= BMAX) }
    n : cardinal;              { number of codes (assumed <= N_MAX) }
    s : cardinal;              { number of simple-valued codes (0..s-1) }
const d : array of cardinal;  { list of base values for non-simple codes }
{ array of word }
const e : array of cardinal;  { list of extra bits for non-simple codes }
{ array of byte }
  t : ppInflate_huft;     { result: starting table }
var m : cardinal;             { maximum lookup bits, returns actual }
var hp : array of inflate_huft;  { space for trees }
var hn : cardinal;             { hufts used in space }
var v : array of cardinal     { working area: values in order of bit length }
   ) : integer;
{ Given a list of code lengths and a maximum table size, make a set of
  tables to decode that set of codes.  Return Z_OK on success, Z_BUF_ERROR
  if the given code set is incomplete (the tables are still built in this
  case), Z_DATA_ERROR if the input is invalid (an over-subscribed set of
  lengths), or Z_MEM_ERROR if not enough memory. }
Var
  a : cardinal;                     { counter for codes of length k }
  c : Array [0..BMAX] Of cardinal;  { bit length count table }
  f : cardinal;                     { i repeats in table every f entries }
  g : integer;                      { maximum code length }
  h : integer;                      { table level }
  i : cardinal;  {register}         { counter, current code }
  j : cardinal;  {register}         { counter }
  k : integer;   {register}         { number of bits in current code }
  l : integer;			{ bits per table (returned in m) }
  mask : cardinal;                  { (1 shl w) - 1, to avoid cc -O bug on HP }
  p : ^cardinal; {register}        { pointer into c[], b[], or v[] }
  q : pInflate_huft;            { points to current table }
  r : inflate_huft;             { table entry for structure assignment }
  u : Array [0..BMAX-1] Of pInflate_huft; { table stack }
  w : integer;   {register}         { bits before this table = (l*h) }
  x : Array [0..BMAX] Of cardinal;  { bit offsets, then code stack }
  {$IFDEF USE_PTR}
  xp : Pcardinal;                  { pointer into x }
  {$ELSE}
  xp : cardinal;
  {$ENDIF}
  y : integer;                      { number of dummy codes added }
  z : cardinal;                     { number of entries in current table }
Begin
  { Generate counts for each bit length }
  FillChar(c,SizeOf(c),0) ;     { clear c[] }

  for i := 0 to n-1 do
    Inc (c[b[i]]);              { assume all entries <= BMAX }

  If (c[0] = n) Then            { null input--all zero length codes }
  Begin
    t^ := pInflate_huft(NIL);
    m := 0 ;
    huft_build := Z_OK ;
    Exit;
  End ;

  { Find minimum and maximum length, bound [m] by those }
  l := m;
  for j:=1 To BMAX do
    if (c[j] <> 0) then
      break;
  k := j ;                      { minimum code length }
  if (cardinal(l) < j) then
    l := j;
  for i := BMAX downto 1 do
    if (c[i] <> 0) then
      break ;
  g := i ;                      { maximum code length }
  if (cardinal(l) > i) then
     l := i;
  m := l;

  { Adjust last length count to fill out codes, if needed }
  y := 1 shl j ;
  while (j < i) do
  begin
    Dec(y, c[j]) ;
    if (y < 0) then
    begin
      huft_build := Z_DATA_ERROR;   { bad input: more codes than bits }
      exit;
    end ;
    Inc(j) ;
    y := y shl 1
  end;
  Dec (y, c[i]) ;
  if (y < 0) then
  begin
    huft_build := Z_DATA_ERROR;     { bad input: more codes than bits }
    exit;
  end;
  Inc(c[i], y);

  { Generate starting offsets into the value table FOR each length }
  {$IFDEF USE_PTR}
  x[1] := 0;
  j := 0;

  p := @c[1];
  xp := @x[2];

  dec(i);               { note that i = g from above }
  WHILE (i > 0) DO
  BEGIN
    inc(j, p^);
    xp^ := j;
    inc(p);
    inc(xp);
    dec(i);
  END;
  {$ELSE}
  x[1] := 0;
  j := 0 ;
  for i := 1 to g do
  begin
    x[i] := j;
    Inc(j, c[i]);
  end;
  {$ENDIF}

  { Make a table of values in order of bit lengths }
  for i := 0 to n-1 do
  begin
    j := b[i];
    if (j <> 0) then
    begin
      v[ x[j] ] := i;
      Inc(x[j]);
    end;
  end;
  n := x[g];                     { set n to length of v }

  { Generate the Huffman codes and for each, make the table entries }
  i := 0 ;
  x[0] := 0 ;                   { first Huffman code is zero }
  p := @v[0] ;                { grab values in bit order }
  h := -1 ;                     { no tables yet--level -1 }
  w := -l ;                     { bits decoded = (l*h) }

  u[0] := pInflate_huft(NIL);   { just to keep compilers happy }
  q := pInflate_huft(NIL);      { ditto }
  z := 0 ;                      { ditto }

  { go through the bit lengths (k already is bits in shortest code) }
  while (k <= g) Do
  begin
    a := c[k] ;
    while (a<>0) Do
    begin
      Dec (a) ;
      { here i is the Huffman code of length k bits for value p^ }
      { make tables up to required level }
      while (k > w + l) do
      begin

        Inc (h) ;
        Inc (w, l);              { add bits already decoded }
                                 { previous table always l bits }
        { compute minimum size table less than or equal to l bits }

        { table size upper limit }
        z := g - w;
        If (z > cardinal(l)) Then
          z := l;

        { try a k-w bit table }
        j := k - w;
        f := 1 shl j;
        if (f > a+1) Then        { too few codes for k-w bit table }
        begin
          Dec(f, a+1);           { deduct codes from patterns left }
          {$IFDEF USE_PTR}
          xp := Addr(c[k]);

          if (j < z) then
          begin
            Inc(j);
            while (j < z) do
            begin                { try smaller tables up to z bits }
              f := f shl 1;
              Inc (xp) ;
              If (f <= xp^) Then
                break;           { enough codes to use up j bits }
              Dec(f, xp^);       { else deduct codes from patterns }
              Inc(j);
            end;
          end;
          {$ELSE}
          xp := k;

          if (j < z) then
          begin
            Inc (j) ;
            While (j < z) Do
            begin                 { try smaller tables up to z bits }
              f := f * 2;
              Inc (xp) ;
              if (f <= c[xp]) then
                Break ;           { enough codes to use up j bits }
              Dec (f, c[xp]) ;      { else deduct codes from patterns }
              Inc (j);
            end;
          end;
          {$ENDIF}
        end;

        z := 1 shl j;            { table entries for j-bit table }

        { allocate new table }
        if (hn + z > MANY) then { (note: doesn't matter for fixed) }
        begin
          huft_build := Z_MEM_ERROR;     { not enough memory }
          exit;
        end;

        q := @hp[hn];
        u[h] := q;
        Inc(hn, z);

        { connect to last table, if there is one }
        if (h <> 0) then
        begin
          x[h] := i;             { save pattern for backing up }
          r.bits := byte(l);     { bits to dump before this table }
          r.exop := byte(j);     { bits in this table }
          j := i shr (w - l);
          {r.base := cardinal( q - u[h-1] -j);}   { offset to this table }
          r.base := (ptruint(q) - ptruint(u[h-1]) ) div sizeof(q^) - j;
          huft_Ptr(u[h-1])^[j] := r;  { connect to last table }
        end
        else
          t^ := q;               { first table is returned result }
      end;

      { set up table entry in r }
      r.bits := byte(k - w);

      { C-code: if (p >= v + n) - see ZUTIL.PAS for comments }

      if ptruint(p)>=ptruint(@(v[n])) then  { also works under DPMI ?? }
        r.exop := 128 + 64                  { out of values--invalid code }
      else
        if (p^ < s) then
        begin
          if (p^ < 256) then     { 256 is end-of-block code }
            r.exop := 0
          Else
            r.exop := 32 + 64;   { EOB_code; }
          r.base := p^;          { simple code is just the value }
          Inc(p);
        end
        Else
        begin
          r.exop := byte(e[p^-s] + 16 + 64);  { non-simple--look up in lists }
          r.base := d[p^-s];
          Inc (p);
        end ;

      { fill code-like entries with r }
      f := 1 shl (k - w);
      j := i shr w;
      while (j < z) do
      begin
        huft_Ptr(q)^[j] := r;
        Inc(j, f);
      end;

      { backwards increment the k-bit code i }
      j := 1 shl (k-1) ;
      while (i and j) <> 0 do
      begin
        i := i xor j;         { bitwise exclusive or }
        j := j shr 1
      end ;
      i := i xor j;

      { backup over finished tables }
      mask := (1 shl w) - 1;   { needed on HP, cc -O bug }
      while ((i and mask) <> x[h]) do
      begin
        Dec(h);                { don't need to update q }
        Dec(w, l);
        mask := (1 shl w) - 1;
      end;

    end;

    Inc(k);
  end;

  { Return Z_BUF_ERROR if we were given an incomplete table }
  if (y <> 0) And (g <> 1) then
    huft_build := Z_BUF_ERROR
  else
    huft_build := Z_OK;
end; { huft_build}


function inflate_trees_bits(
  var c : array of cardinal;  { 19 code lengths }
  var bb : cardinal;          { bits tree desired/actual depth }
  var tb : pinflate_huft;  { bits tree result }
  var hp : array of Inflate_huft;      { space for trees }
  var z : z_stream         { for messages }
    ) : integer;
var
  r : integer;
  hn : cardinal;          { hufts used in space }
  v : Pcardinalarray;     { work area for huft_build }
begin
  hn := 0;
  getmem(v,19*sizeof(cardinal));
  if (v = nil) then
  begin
    inflate_trees_bits := Z_MEM_ERROR;
    exit;
  end;

  r := huft_build(c, 19, 19, cplens, cplext,
                             {Pcardinal(nil), Pcardinal(nil),}
                  @tb, bb, hp, hn, v^);
  if (r = Z_DATA_ERROR) then
    z.msg := 'oversubscribed dynamic bit lengths tree'
  else
    if (r = Z_BUF_ERROR) or (bb = 0) then
    begin
      z.msg := 'incomplete dynamic bit lengths tree';
      r := Z_DATA_ERROR;
    end;
  freemem(v);
  inflate_trees_bits := r;
end;


function inflate_trees_dynamic(
    nl : cardinal;                    { number of literal/length codes }
    nd : cardinal;                    { number of distance codes }
    var c : Array of cardinal;           { that many (total) code lengths }
    var bl : cardinal;          { literal desired/actual bit depth }
    var bd : cardinal;          { distance desired/actual bit depth }
var tl : pInflate_huft;           { literal/length tree result }
var td : pInflate_huft;           { distance tree result }
var hp : array of Inflate_huft;   { space for trees }
var z : z_stream                  { for messages }
     ) : integer;
var
  r : integer;
  hn : cardinal;          { hufts used in space }
  v : Pcardinalarray;     { work area for huft_build }
begin
  hn := 0;
  { allocate work area }
  getmem(v,288*sizeof(cardinal));
  if (v = nil) then
  begin
    inflate_trees_dynamic := Z_MEM_ERROR;
    exit;
  end;

  { build literal/length tree }
  r := huft_build(c, nl, 257, cplens, cplext, @tl, bl, hp, hn, v^);
  if (r <> Z_OK) or (bl = 0) then
  begin
    if (r = Z_DATA_ERROR) then
      z.msg := 'oversubscribed literal/length tree'
    else
      if (r <> Z_MEM_ERROR) then
      begin
        z.msg := 'incomplete literal/length tree';
        r := Z_DATA_ERROR;
      end;

    freemem(v);
    inflate_trees_dynamic := r;
    exit;
  end;

  { build distance tree }
  r := huft_build(Pcardinalarray(@c[nl])^, nd, 0,
                  cpdist, cpdext, @td, bd, hp, hn, v^);
  if (r <> Z_OK) or ((bd = 0) and (nl > 257)) then
  begin
    if (r = Z_DATA_ERROR) then
      z.msg := 'oversubscribed literal/length tree'
    else
      if (r = Z_BUF_ERROR) then
      begin
{$ifdef PKZIP_BUG_WORKAROUND}
        r := Z_OK;
      end;
{$else}
        z.msg := 'incomplete literal/length tree';
        r := Z_DATA_ERROR;
      end
      else
        if (r <> Z_MEM_ERROR) then
        begin
          z.msg := 'empty distance tree with lengths';
          r := Z_DATA_ERROR;
        end;
    freemem(v);
    inflate_trees_dynamic := r;
    exit;
{$endif}
  end;

  { done }
  freemem(v);
  inflate_trees_dynamic := Z_OK;
end;

{$UNDEF BUILDFIXED}

{ build fixed tables only once--keep them here }
{$IFNDEF BUILDFIXED}
{ locals }
const
  fixed_built : Boolean = false;
  FIXEDH = 544;      { number of hufts used by fixed tables }
var
  fixed_mem : array[0..FIXEDH-1] of inflate_huft;
  fixed_bl : cardinal;
  fixed_bd : cardinal;
  fixed_tl : pInflate_huft;
  fixed_td : pInflate_huft;

{$ELSE}

{ inffixed.h -- table for decoding fixed codes }

{local}
const
  fixed_bl = 9;
{local}
const
  fixed_bd = 5;
{local}
const
  fixed_tl : array [0..288-1] of inflate_huft = (
    Exop,             { number of extra bits or operation }
    bits : byte;      { number of bits in this code or subcode }
    {pad : cardinal;}       { pad structure to a power of 2 (4 bytes for }
                      {  16-bit, 8 bytes for 32-bit integer's) }
    base : cardinal;      { literal, length base, or distance base }
                      { or table offset }

    ((96,7),256), ((0,8),80), ((0,8),16), ((84,8),115), ((82,7),31),
    ((0,8),112), ((0,8),48), ((0,9),192), ((80,7),10), ((0,8),96),
    ((0,8),32), ((0,9),160), ((0,8),0), ((0,8),128), ((0,8),64),
    ((0,9),224), ((80,7),6), ((0,8),88), ((0,8),24), ((0,9),144),
    ((83,7),59), ((0,8),120), ((0,8),56), ((0,9),208), ((81,7),17),
    ((0,8),104), ((0,8),40), ((0,9),176), ((0,8),8), ((0,8),136),
    ((0,8),72), ((0,9),240), ((80,7),4), ((0,8),84), ((0,8),20),
    ((85,8),227), ((83,7),43), ((0,8),116), ((0,8),52), ((0,9),200),
    ((81,7),13), ((0,8),100), ((0,8),36), ((0,9),168), ((0,8),4),
    ((0,8),132), ((0,8),68), ((0,9),232), ((80,7),8), ((0,8),92),
    ((0,8),28), ((0,9),152), ((84,7),83), ((0,8),124), ((0,8),60),
    ((0,9),216), ((82,7),23), ((0,8),108), ((0,8),44), ((0,9),184),
    ((0,8),12), ((0,8),140), ((0,8),76), ((0,9),248), ((80,7),3),
    ((0,8),82), ((0,8),18), ((85,8),163), ((83,7),35), ((0,8),114),
    ((0,8),50), ((0,9),196), ((81,7),11), ((0,8),98), ((0,8),34),
    ((0,9),164), ((0,8),2), ((0,8),130), ((0,8),66), ((0,9),228),
    ((80,7),7), ((0,8),90), ((0,8),26), ((0,9),148), ((84,7),67),
    ((0,8),122), ((0,8),58), ((0,9),212), ((82,7),19), ((0,8),106),
    ((0,8),42), ((0,9),180), ((0,8),10), ((0,8),138), ((0,8),74),
    ((0,9),244), ((80,7),5), ((0,8),86), ((0,8),22), ((192,8),0),
    ((83,7),51), ((0,8),118), ((0,8),54), ((0,9),204), ((81,7),15),
    ((0,8),102), ((0,8),38), ((0,9),172), ((0,8),6), ((0,8),134),
    ((0,8),70), ((0,9),236), ((80,7),9), ((0,8),94), ((0,8),30),
    ((0,9),156), ((84,7),99), ((0,8),126), ((0,8),62), ((0,9),220),
    ((82,7),27), ((0,8),110), ((0,8),46), ((0,9),188), ((0,8),14),
    ((0,8),142), ((0,8),78), ((0,9),252), ((96,7),256), ((0,8),81),
    ((0,8),17), ((85,8),131), ((82,7),31), ((0,8),113), ((0,8),49),
    ((0,9),194), ((80,7),10), ((0,8),97), ((0,8),33), ((0,9),162),
    ((0,8),1), ((0,8),129), ((0,8),65), ((0,9),226), ((80,7),6),
    ((0,8),89), ((0,8),25), ((0,9),146), ((83,7),59), ((0,8),121),
    ((0,8),57), ((0,9),210), ((81,7),17), ((0,8),105), ((0,8),41),
    ((0,9),178), ((0,8),9), ((0,8),137), ((0,8),73), ((0,9),242),
    ((80,7),4), ((0,8),85), ((0,8),21), ((80,8),258), ((83,7),43),
    ((0,8),117), ((0,8),53), ((0,9),202), ((81,7),13), ((0,8),101),
    ((0,8),37), ((0,9),170), ((0,8),5), ((0,8),133), ((0,8),69),
    ((0,9),234), ((80,7),8), ((0,8),93), ((0,8),29), ((0,9),154),
    ((84,7),83), ((0,8),125), ((0,8),61), ((0,9),218), ((82,7),23),
    ((0,8),109), ((0,8),45), ((0,9),186), ((0,8),13), ((0,8),141),
    ((0,8),77), ((0,9),250), ((80,7),3), ((0,8),83), ((0,8),19),
    ((85,8),195), ((83,7),35), ((0,8),115), ((0,8),51), ((0,9),198),
    ((81,7),11), ((0,8),99), ((0,8),35), ((0,9),166), ((0,8),3),
    ((0,8),131), ((0,8),67), ((0,9),230), ((80,7),7), ((0,8),91),
    ((0,8),27), ((0,9),150), ((84,7),67), ((0,8),123), ((0,8),59),
    ((0,9),214), ((82,7),19), ((0,8),107), ((0,8),43), ((0,9),182),
    ((0,8),11), ((0,8),139), ((0,8),75), ((0,9),246), ((80,7),5),
    ((0,8),87), ((0,8),23), ((192,8),0), ((83,7),51), ((0,8),119),
    ((0,8),55), ((0,9),206), ((81,7),15), ((0,8),103), ((0,8),39),
    ((0,9),174), ((0,8),7), ((0,8),135), ((0,8),71), ((0,9),238),
    ((80,7),9), ((0,8),95), ((0,8),31), ((0,9),158), ((84,7),99),
    ((0,8),127), ((0,8),63), ((0,9),222), ((82,7),27), ((0,8),111),
    ((0,8),47), ((0,9),190), ((0,8),15), ((0,8),143), ((0,8),79),
    ((0,9),254), ((96,7),256), ((0,8),80), ((0,8),16), ((84,8),115),
    ((82,7),31), ((0,8),112), ((0,8),48), ((0,9),193), ((80,7),10),
    ((0,8),96), ((0,8),32), ((0,9),161), ((0,8),0), ((0,8),128),
    ((0,8),64), ((0,9),225), ((80,7),6), ((0,8),88), ((0,8),24),
    ((0,9),145), ((83,7),59), ((0,8),120), ((0,8),56), ((0,9),209),
    ((81,7),17), ((0,8),104), ((0,8),40), ((0,9),177), ((0,8),8),
    ((0,8),136), ((0,8),72), ((0,9),241), ((80,7),4), ((0,8),84),
    ((0,8),20), ((85,8),227), ((83,7),43), ((0,8),116), ((0,8),52),
    ((0,9),201), ((81,7),13), ((0,8),100), ((0,8),36), ((0,9),169),
    ((0,8),4), ((0,8),132), ((0,8),68), ((0,9),233), ((80,7),8),
    ((0,8),92), ((0,8),28), ((0,9),153), ((84,7),83), ((0,8),124),
    ((0,8),60), ((0,9),217), ((82,7),23), ((0,8),108), ((0,8),44),
    ((0,9),185), ((0,8),12), ((0,8),140), ((0,8),76), ((0,9),249),
    ((80,7),3), ((0,8),82), ((0,8),18), ((85,8),163), ((83,7),35),
    ((0,8),114), ((0,8),50), ((0,9),197), ((81,7),11), ((0,8),98),
    ((0,8),34), ((0,9),165), ((0,8),2), ((0,8),130), ((0,8),66),
    ((0,9),229), ((80,7),7), ((0,8),90), ((0,8),26), ((0,9),149),
    ((84,7),67), ((0,8),122), ((0,8),58), ((0,9),213), ((82,7),19),
    ((0,8),106), ((0,8),42), ((0,9),181), ((0,8),10), ((0,8),138),
    ((0,8),74), ((0,9),245), ((80,7),5), ((0,8),86), ((0,8),22),
    ((192,8),0), ((83,7),51), ((0,8),118), ((0,8),54), ((0,9),205),
    ((81,7),15), ((0,8),102), ((0,8),38), ((0,9),173), ((0,8),6),
    ((0,8),134), ((0,8),70), ((0,9),237), ((80,7),9), ((0,8),94),
    ((0,8),30), ((0,9),157), ((84,7),99), ((0,8),126), ((0,8),62),
    ((0,9),221), ((82,7),27), ((0,8),110), ((0,8),46), ((0,9),189),
    ((0,8),14), ((0,8),142), ((0,8),78), ((0,9),253), ((96,7),256),
    ((0,8),81), ((0,8),17), ((85,8),131), ((82,7),31), ((0,8),113),
    ((0,8),49), ((0,9),195), ((80,7),10), ((0,8),97), ((0,8),33),
    ((0,9),163), ((0,8),1), ((0,8),129), ((0,8),65), ((0,9),227),
    ((80,7),6), ((0,8),89), ((0,8),25), ((0,9),147), ((83,7),59),
    ((0,8),121), ((0,8),57), ((0,9),211), ((81,7),17), ((0,8),105),
    ((0,8),41), ((0,9),179), ((0,8),9), ((0,8),137), ((0,8),73),
    ((0,9),243), ((80,7),4), ((0,8),85), ((0,8),21), ((80,8),258),
    ((83,7),43), ((0,8),117), ((0,8),53), ((0,9),203), ((81,7),13),
    ((0,8),101), ((0,8),37), ((0,9),171), ((0,8),5), ((0,8),133),
    ((0,8),69), ((0,9),235), ((80,7),8), ((0,8),93), ((0,8),29),
    ((0,9),155), ((84,7),83), ((0,8),125), ((0,8),61), ((0,9),219),
    ((82,7),23), ((0,8),109), ((0,8),45), ((0,9),187), ((0,8),13),
    ((0,8),141), ((0,8),77), ((0,9),251), ((80,7),3), ((0,8),83),
    ((0,8),19), ((85,8),195), ((83,7),35), ((0,8),115), ((0,8),51),
    ((0,9),199), ((81,7),11), ((0,8),99), ((0,8),35), ((0,9),167),
    ((0,8),3), ((0,8),131), ((0,8),67), ((0,9),231), ((80,7),7),
    ((0,8),91), ((0,8),27), ((0,9),151), ((84,7),67), ((0,8),123),
    ((0,8),59), ((0,9),215), ((82,7),19), ((0,8),107), ((0,8),43),
    ((0,9),183), ((0,8),11), ((0,8),139), ((0,8),75), ((0,9),247),
    ((80,7),5), ((0,8),87), ((0,8),23), ((192,8),0), ((83,7),51),
    ((0,8),119), ((0,8),55), ((0,9),207), ((81,7),15), ((0,8),103),
    ((0,8),39), ((0,9),175), ((0,8),7), ((0,8),135), ((0,8),71),
    ((0,9),239), ((80,7),9), ((0,8),95), ((0,8),31), ((0,9),159),
    ((84,7),99), ((0,8),127), ((0,8),63), ((0,9),223), ((82,7),27),
    ((0,8),111), ((0,8),47), ((0,9),191), ((0,8),15), ((0,8),143),
    ((0,8),79), ((0,9),255)
  );

{local}
const
  fixed_td : array[0..32-1] of inflate_huft = (
(Exop:80;bits:5;base:1),      (Exop:87;bits:5;base:257),   (Exop:83;bits:5;base:17),
(Exop:91;bits:5;base:4097),   (Exop:81;bits:5;base),       (Exop:89;bits:5;base:1025),
(Exop:85;bits:5;base:65),     (Exop:93;bits:5;base:16385), (Exop:80;bits:5;base:3),
(Exop:88;bits:5;base:513),    (Exop:84;bits:5;base:33),    (Exop:92;bits:5;base:8193),
(Exop:82;bits:5;base:9),      (Exop:90;bits:5;base:2049),  (Exop:86;bits:5;base:129),
(Exop:192;bits:5;base:24577), (Exop:80;bits:5;base:2),     (Exop:87;bits:5;base:385),
(Exop:83;bits:5;base:25),     (Exop:91;bits:5;base:6145),  (Exop:81;bits:5;base:7),
(Exop:89;bits:5;base:1537),   (Exop:85;bits:5;base:97),    (Exop:93;bits:5;base:24577),
(Exop:80;bits:5;base:4),      (Exop:88;bits:5;base:769),   (Exop:84;bits:5;base:49),
(Exop:92;bits:5;base:12289),  (Exop:82;bits:5;base:13),    (Exop:90;bits:5;base:3073),
(Exop:86;bits:5;base:193),    (Exop:192;bits:5;base:24577)
  );
{$ENDIF}

function inflate_trees_fixed(
var bl : cardinal;               { literal desired/actual bit depth }
var bd : cardinal;               { distance desired/actual bit depth }
var tl : pInflate_huft;      { literal/length tree result }
var td : pInflate_huft;      { distance tree result }
var  z : z_stream            { for memory allocation }
      ) : integer;
type
  pFixed_table = ^fixed_table;
  fixed_table = array[0..288-1] of cardinal;
var
  k : integer;                   { temporary variable }
  c : pFixed_table;              { length list for huft_build }
  v : Pcardinalarray;            { work area for huft_build }
var
  f : cardinal;                  { number of hufts used in fixed_mem }
begin
  { build fixed tables if not already (multiple overlapped executions ok) }
  if not fixed_built then
  begin
    f := 0;

    { allocate memory }
    getmem(c,288*sizeof(cardinal));
    if (c = nil) then
    begin
      inflate_trees_fixed := Z_MEM_ERROR;
      exit;
    end;
    getmem(v,288*sizeof(cardinal));
    if (v = nil) then
    begin
      freemem(c);
      inflate_trees_fixed := Z_MEM_ERROR;
      exit;
    end;

    { literal table }
    for k := 0 to Pred(144) do
      c^[k] := 8;
    for k := 144 to Pred(256) do
      c^[k] := 9;
    for k := 256 to Pred(280) do
      c^[k] := 7;
    for k := 280 to Pred(288) do
      c^[k] := 8;
    fixed_bl := 9;
    huft_build(c^, 288, 257, cplens, cplext, @fixed_tl, fixed_bl,
               fixed_mem, f, v^);

    { distance table }
    for k := 0 to Pred(30) do
      c^[k] := 5;
    fixed_bd := 5;
    huft_build(c^, 30, 0, cpdist, cpdext, @fixed_td, fixed_bd,
               fixed_mem, f, v^);

    { done }
    freemem(v);
    freemem(c);
    fixed_built := True;
  end;
  bl := fixed_bl;
  bd := fixed_bd;
  tl := fixed_tl;
  td := fixed_td;
  inflate_trees_fixed := Z_OK;
end; { inflate_trees_fixed }


end.
