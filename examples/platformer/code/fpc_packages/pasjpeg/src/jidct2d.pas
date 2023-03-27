Unit JIDct2D;

{ This file contains a fast, not so accurate integer implementation of the
  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
  must also perform dequantization of the input coefficients.


  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
  on each row (or vice versa, but it's more convenient to emit a row at
  a time).  Direct algorithms are also available, but they are much more
  complex and seem not to be any faster when reduced to code.

  The Feig direct 2D scaled Discrete Cosine Transform extends Arai, Agui
  and Nakajima fast scaled DCT to 2D (464 adds and 80 mult.) with further
  computational saving (462 adds, 54 mults and 6 shits).

  The forward DCT is described with flow diagrams from the Pennebaker&
  Mitchell JPEG book. The inverse DCT flow diagrams are obtained
  from the inverse matrices. Scaling must be done accordingly.

  Jacques NOMSSI NZALI, May 16th 1995 }


interface

uses
  jmorecfg,
  jinclude,
  jpeglib,
  jdct;                 { Private declarations for DCT subsystem }

{$I jconfig.inc}

{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_i2d (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
                         coef_block : JCOEFPTR;
                         output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);

implementation

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}

{ Scaling decisions are generally the same as in the LL&M algorithm;
  see jidctint.c for more details.  However, we choose to descale
  (right shift) multiplication products as soon as they are formed,
  rather than carrying additional fractional bits into subsequent additions.
  This compromises accuracy slightly, but it lets us save a few shifts.
  More importantly, 16-bit arithmetic is then adequate (for 8-bit samples)
  everywhere except in the multiplications proper; this saves a good deal
  of work on 16-bit-int machines.

  The dequantized coefficients are not integers because the AA&N scaling
  factors have been incorporated.  We represent them scaled up by PASS1_BITS,
  so that the first and second IDCT rounds have the same input scaling.
  For 8-bit JSAMPLEs, we choose IFAST_SCALE_BITS = PASS1_BITS so as to
  avoid a descaling shift; this compromises accuracy rather drastically
  for small quantization table entries, but it saves a lot of shifts.
  For 12-bit JSAMPLEs, there's no hope of using 16x16 multiplies anyway,
  so we use a much larger scaling factor to preserve accuracy.

  A final compromise is to represent the multiplicative constants to only
  8 fractional bits, rather than 13.  This saves some shifting work on some
  machines, and may also reduce the cost of multiplication (since there
  are fewer one-bits in the constants). }

{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  CONST_BITS = 8;
  PASS1_BITS = 2;
{$else}
  CONST_BITS = 8;
  PASS1_BITS = 1;       { lose a little precision to avoid overflow }
{$endif}


{ Convert a positive real constant to an integer scaled by CONST_SCALE. }
const
  CONST_SCALE = (INT32(1) shl CONST_BITS);
const
  FIX_1_082392200 = INT32(Round(CONST_SCALE*1.082392200));  {277}
  FIX_1_414213562 = INT32(Round(CONST_SCALE*1.414213562));  {362}
  FIX_1_847759065 = INT32(Round(CONST_SCALE*1.847759065));  {473}
  FIX_2_613125930 = INT32(Round(CONST_SCALE*2.613125930));  {669}


{ Descale and correctly round an INT32 value that's scaled by N bits.
  We assume RIGHT_SHIFT rounds towards minus infinity, so adding
  the fudge factor is correct for either sign of X. }

function DESCALE(x : INT32; n : int) : INT32;
var
  shift_temp : INT32;
begin
{$ifdef USE_ACCURATE_ROUNDING}
  shift_temp := x + (INT32(1) shl (n-1));
{$else}
{ We can gain a little more speed, with a further compromise in accuracy,
  by omitting the addition in a descaling shift.  This yields an incorrectly
  rounded result half the time... }
  shift_temp := x;
{$endif}

{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  if shift_temp < 0 then
    Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
  else
{$endif}
    Descale :=  (shift_temp shr n);

end;


{ Multiply a DCTELEM variable by an INT32 constant, and immediately
  descale to yield a DCTELEM result. }

  {(DCTELEM( DESCALE((var) * (const), CONST_BITS))}
  function Multiply(Avar, Aconst: Integer): DCTELEM;
  begin
    Multiply := DCTELEM( Avar*INT32(Aconst) div CONST_SCALE);
  end;



{ Dequantize a coefficient by multiplying it by the multiplier-table
  entry; produce a DCTELEM result.  For 8-bit data a 16x16->16
  multiplication will do.  For 12-bit data, the multiplier table is
  declared INT32, so a 32-bit multiply will be used. }

{$ifdef BITS_IN_JSAMPLE_IS_8}
  function DEQUANTIZE(coef,quantval : int) : int;
  begin
    Dequantize := ( IFAST_MULT_TYPE(coef) * quantval);
  end;

{$else}
#define DEQUANTIZE(coef,quantval)  \
        DESCALE((coef)*(quantval), IFAST_SCALE_BITS-PASS1_BITS)
{$endif}


{ Like DESCALE, but applies to a DCTELEM and produces an int.
  We assume that int right shift is unsigned if INT32 right shift is. }

function IDESCALE(x : DCTELEM; n : int) : int;
{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  DCTELEMBITS = 16;     { DCTELEM may be 16 or 32 bits }
{$else}
const
  DCTELEMBITS = 32;     { DCTELEM must be 32 bits }
{$endif}
var
  ishift_temp : DCTELEM;
begin
{$ifndef USE_ACCURATE_ROUNDING}
  ishift_temp := x + (INT32(1) shl (n-1));
{$else}
{ We can gain a little more speed, with a further compromise in accuracy,
  by omitting the addition in a descaling shift.  This yields an incorrectly
  rounded result half the time... }
  ishift_temp := x;
{$endif}

{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  if ishift_temp < 0 then
    IDescale :=  (ishift_temp shr n)
             or ((not DCTELEM(0)) shl (DCTELEMBITS-n))
  else
{$endif}
    IDescale :=  (ishift_temp shr n);
end;



{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_i2d (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
                         coef_block : JCOEFPTR;
                         output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);
Const
  CONST_IC4 = 1.414213562; { 1/0.707106781; }
  FP_IC4    = FIX_1_414213562;
  FP_I_C4_2 = FP_IC4;

type
  PWorkspace = ^TWorkspace;
  TWorkspace = coef_bits_field; { buffers data between passes }

  Procedure N1(var x, y : integer);   { rotator 1 }
  Const
    FP_a5 = FIX_1_847759065;
    FP_a4 = FIX_2_613125930;
    FP_a2 = FIX_1_082392200;
  var
    z5, tmp : integer;
  begin
    tmp := x;

    z5 := Multiply(tmp + y, FP_a5);  { c6 }
    x := Multiply(y, FP_a2) - z5;  { c2-c6 }
    y := Multiply(tmp, -FP_a4) + z5;  { c2+c6 }
  end;

  Procedure N2(var x, y : integer); { N1 scaled by c4 }
  Const
    FP_b5 = Integer(Round(CONST_SCALE*1.847759065*CONST_IC4));
    FP_b4 = Integer(Round(CONST_SCALE*2.613125930*CONST_IC4));
    FP_b2 = Integer(Round(CONST_SCALE*1.082392200*CONST_IC4));
  var
    z5, tmp : integer;
  begin
    tmp := x;

    z5 := Multiply(tmp + y, FP_b5);
    x := Multiply(y, FP_b2) - z5;
    y := Multiply(tmp,-FP_b4) + z5;
  end;

var
  column, row : byte;

var
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7 : DCTELEM;
  tmp10, tmp11, tmp12, tmp13 : DCTELEM;
  z10, z11, z12, z13 : DCTELEM;
  inptr : JCOEFPTR;

  quantptr : IFAST_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;       { buffers data between passes }
  {SHIFT_TEMPS                  { for DESCALE }
  {ISHIFT_TEMPS                 { for IDESCALE }
var
  dcval : int;
var
  dcval_ : JSAMPLE;
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));
  { Pass 1: process columns from input, store into work array. }

  inptr := coef_block;
  quantptr := IFAST_MULT_TYPE_FIELD_PTR(compptr^.dct_table);
  wsptr := @workspace;
  for ctr := pred(DCTSIZE) downto 0 do
  begin
    { short-circuiting is not easily done here }
  // bbo := @outptr;
  for num := 0 to Pred(count) do
  begin
    { R1 x R1 }
    for column := 7 downto 0 do
    BEGIN
      tmp5 := inptr^[1*RowSize + column];

      inptr^[1*RowSize + column] := inptr^[4*RowSize + column];

      tmp7 := inptr^[3*RowSize + column];

      a := inptr^[2*RowSize + column];
      b := inptr^[6*RowSize + column];
      inptr^[2*RowSize + column] := a - b;
      inptr^[3*RowSize + column] := a + b;

      a := inptr^[5*RowSize + column];
      inptr^[4*RowSize + column] := a - tmp7;
      z13 := a + tmp7;

      b := inptr^[7*RowSize + column];
      inptr^[6*RowSize + column] := tmp5 - b;
      z11 := tmp5 + b;

      inptr^[5*RowSize + column] := z11 - z13;
      inptr^[7*RowSize + column] := z11 + z13;
    END;

    { Even part }

    tmp0 := DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]);
    tmp1 := DEQUANTIZE(inptr^[DCTSIZE*2], quantptr^[DCTSIZE*2]);
    tmp2 := DEQUANTIZE(inptr^[DCTSIZE*4], quantptr^[DCTSIZE*4]);
    tmp3 := DEQUANTIZE(inptr^[DCTSIZE*6], quantptr^[DCTSIZE*6]);

    tmp10 := tmp0 + tmp2;       { phase 3 }
    tmp11 := tmp0 - tmp2;

    tmp13 := tmp1 + tmp3;       { phases 5-3 }
    tmp12 := MULTIPLY(tmp1 - tmp3, FIX_1_414213562) - tmp13; { 2*c4 }

    tmp0 := tmp10 + tmp13;      { phase 2 }
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    { Odd part }

    tmp4 := DEQUANTIZE(inptr^[DCTSIZE*1], quantptr^[DCTSIZE*1]);
    tmp5 := DEQUANTIZE(inptr^[DCTSIZE*3], quantptr^[DCTSIZE*3]);
    tmp6 := DEQUANTIZE(inptr^[DCTSIZE*5], quantptr^[DCTSIZE*5]);
    tmp7 := DEQUANTIZE(inptr^[DCTSIZE*7], quantptr^[DCTSIZE*7]);

    z13 := tmp6 + tmp5;         { phase 6 }
    z10 := tmp6 - tmp5;
    z11 := tmp4 + tmp7;
    z12 := tmp4 - tmp7;

    tmp7 := z11 + z13;          { phase 5 }
    tmp11 := MULTIPLY(z11 - z13, FIX_1_414213562); { 2*c4 }

    z5 := MULTIPLY(z10 + z12, FIX_1_847759065); { 2*c2 }
    tmp10 := MULTIPLY(z12, FIX_1_082392200) - z5; { 2*(c2-c6) }
    tmp12 := MULTIPLY(z10, - FIX_2_613125930) + z5; { -2*(c2+c6) }

    tmp6 := tmp12 - tmp7;       { phase 2 }
    tmp5 := tmp11 - tmp6;
    tmp4 := tmp10 + tmp5;

    wsptr^[DCTSIZE*0] := int (tmp0 + tmp7);
    wsptr^[DCTSIZE*7] := int (tmp0 - tmp7);
    wsptr^[DCTSIZE*1] := int (tmp1 + tmp6);
    wsptr^[DCTSIZE*6] := int (tmp1 - tmp6);
    wsptr^[DCTSIZE*2] := int (tmp2 + tmp5);
    wsptr^[DCTSIZE*5] := int (tmp2 - tmp5);
    wsptr^[DCTSIZE*4] := int (tmp3 + tmp4);
    wsptr^[DCTSIZE*3] := int (tmp3 - tmp4);

    Inc(JCOEF_PTR(inptr));              { advance pointers to next column }
    Inc(IFAST_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));
  end;

  { Pass 2: process rows from work array, store into output array. }
  { Note that we must descale the results by a factor of 8 == 2**3, }
  { and also undo the PASS1_BITS scaling. }

  wsptr := @workspace;
  for ctr := 0 to pred(DCTSIZE) do
  begin
    outptr := JSAMPROW(@output_buf^[ctr]^[output_col]);
    { Rows of zeroes can be exploited in the same way as we did with columns.
      However, the column calculation has created many nonzero AC terms, so
      the simplification applies less often (typically 5% to 10% of the time).
      On machines with very fast multiplication, it's possible that the
      test takes more time than it's worth.  In that case this section
      may be commented out. }

{$ifndef NO_ZERO_ROW_TEST}
    if ((wsptr^[1]) or (wsptr^[2]) or (wsptr^[3]) or (wsptr^[4]) or (wsptr^[5]) or
        (wsptr^[6]) or (wsptr^[7]) = 0) then
    begin
      { AC terms all zero }
      JSAMPLE(dcval_) := range_limit^[IDESCALE(wsptr^[0], PASS1_BITS+3)
                          and RANGE_MASK];

      outptr^[0] := dcval_;
      outptr^[1] := dcval_;
      outptr^[2] := dcval_;
      outptr^[3] := dcval_;
      outptr^[4] := dcval_;
      outptr^[5] := dcval_;
      outptr^[6] := dcval_;
      outptr^[7] := dcval_;

      Inc(int_ptr(wsptr), DCTSIZE);     { advance pointer to next row }
      continue;
    end;
{$endif}

    { Even part }

    tmp10 := (DCTELEM(wsptr^[0]) + DCTELEM(wsptr^[4]));
    tmp11 := (DCTELEM(wsptr^[0]) - DCTELEM(wsptr^[4]));

    tmp13 := (DCTELEM(wsptr^[2]) + DCTELEM(wsptr^[6]));
    tmp12 := MULTIPLY(DCTELEM(wsptr^[2]) - DCTELEM(wsptr^[6]), FIX_1_414213562)
            - tmp13;

    tmp0 := tmp10 + tmp13;
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    { Odd part }

    z13 := DCTELEM(wsptr^[5]) + DCTELEM(wsptr^[3]);
    z10 := DCTELEM(wsptr^[5]) - DCTELEM(wsptr^[3]);
    z11 := DCTELEM(wsptr^[1]) + DCTELEM(wsptr^[7]);
    z12 := DCTELEM(wsptr^[1]) - DCTELEM(wsptr^[7]);

    tmp7 := z11 + z13;          { phase 5 }
    tmp11 := MULTIPLY(z11 - z13, FIX_1_414213562); { 2*c4 }

    z5 := MULTIPLY(z10 + z12, FIX_1_847759065); { 2*c2 }
    tmp10 := MULTIPLY(z12, FIX_1_082392200) - z5; { 2*(c2-c6) }
    tmp12 := MULTIPLY(z10, - FIX_2_613125930) + z5; { -2*(c2+c6) }

    tmp6 := tmp12 - tmp7;       { phase 2 }
    tmp5 := tmp11 - tmp6;
    tmp4 := tmp10 + tmp5;

    { Final output stage: scale down by a factor of 8 and range-limit }

    outptr^[0] := range_limit^[IDESCALE(tmp0 + tmp7, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[7] := range_limit^[IDESCALE(tmp0 - tmp7, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[1] := range_limit^[IDESCALE(tmp1 + tmp6, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[6] := range_limit^[IDESCALE(tmp1 - tmp6, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[2] := range_limit^[IDESCALE(tmp2 + tmp5, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[5] := range_limit^[IDESCALE(tmp2 - tmp5, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[4] := range_limit^[IDESCALE(tmp3 + tmp4, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[3] := range_limit^[IDESCALE(tmp3 - tmp4, PASS1_BITS+3)
                            and RANGE_MASK];

    Inc(int_ptr(wsptr), DCTSIZE);       { advance pointer to next row }
  end;
end;

end.
----------------------------------------------------------
type
  matasm  = array[0..DCTSIZE2-1] of integer;
  bmatrix  = array[0..DCTSIZE2-1] of byte;
  bmatrixptr = ^bmatrix;
procedure ANN_IDCT(var coef_block :matasm;
                   var outptr :bmatrix);

                   var coeffs :matasm; = coef_block
                   var outptr :bmatrix); output_buf

Const
  CONST_IC4 = 1.414213562; { 1/0.707106781; }
  FP_IC4    = FIX_1_414213562;
  FP_I_C4_2 = FP_IC4;

  Function Descale(x : integer):byte;
  var y : integer;
  begin
    y := (x + (1 shl (16-1))+ (4 shl PASS_BITS)) div (8 shl PASS_BITS);
    { DeScale := x sar (3 + PASS_BITS);
      Borland Pascal SHR is unsigned }
    if y < 0 then
      descale := 0
    else
      if y > $ff then
        descale := $ff
      else
        descale := y;
  end;

  function Multiply(X, Y: Integer): integer; assembler;
  asm
    mov ax, X
    imul Y
    mov al, ah
    mov ah, dl
  end;


Const
  RowSize = 8;
var
  a, b : integer;

  inptr : JCOEFPTR;

  outptr : bmatrixptr;

  num : integer;
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));
  { Pass 1: process columns from input, store into work array. }

  inptr := @coef_block; + ctr*RowSize
  quantptr := IFAST_MULT_TYPE_FIELD_PTR(compptr^.dct_table);

    for ctr := pred(DCTSIZE) downto 0 do
    BEGIN
      tmp5 := inptr^[1];

      inptr^[1] := inptr^[4];

      tmp7 := inptr^[3];

      a := inptr^[2];
      b := inptr^[6];
      inptr^[2] := a - b;
      inptr^[3] := a + b;

      a := inptr^[5];
      inptr^[+ 4] := a - tmp7;
      z13 := a + tmp7;

      b := inptr^[7];
      inptr^[6] := tmp5 - b;
      z11 := tmp5 + b;

      inptr^[5] := z11 - z13;
      inptr^[7] := z11 + z13;
    END;

    { M x M tensor }
    for row := 0 to 7 do
    Case row of
    0,1,3,7: { M1 }
      begin
        inptr^[row*RowSize + 2] := Multiply(inptr^[row*RowSize + 2], FP_IC4);     { 2/c4 }
        inptr^[row*RowSize + 5] := Multiply(inptr^[row*RowSize + 5], FP_IC4);     { 2/c4 }

        N1(inptr^[row*RowSize +  4], inptr^[row*RowSize +  6]);
      end;
    2,5: { M2 }
      begin
        inptr^[row*RowSize + 0] := Multiply(inptr^[row*RowSize + 0], FP_IC4);
        inptr^[row*RowSize + 1] := Multiply(inptr^[row*RowSize + 1], FP_IC4);
        inptr^[row*RowSize + 3] := Multiply(inptr^[row*RowSize + 3], FP_IC4);
        inptr^[row*RowSize + 7] := Multiply(inptr^[row*RowSize + 7], FP_IC4);

        inptr^[row*RowSize + 2] := inptr^[row*RowSize + 2] * 2;  { shift }
        inptr^[row*RowSize + 5] := inptr^[row*RowSize + 5] * 2;

        N2(inptr^[row*RowSize + 4], inptr^[row*RowSize + 6]);
      end;
    end; { Case }

    { M x N tensor }
    { rows 4,6 }
    begin
      N1(inptr^[4*RowSize + 0], inptr^[6*RowSize + 0]);
      N1(inptr^[4*RowSize + 1], inptr^[6*RowSize + 1]);
      N1(inptr^[4*RowSize + 3], inptr^[6*RowSize + 3]);
      N1(inptr^[4*RowSize + 7], inptr^[6*RowSize + 7]);

      N2(inptr^[4*RowSize + 2], inptr^[6*RowSize + 2]);
      N2(inptr^[4*RowSize + 5], inptr^[6*RowSize + 5]);

      { N3 }
      { two inverse matrices => same as FDCT }
      tmp0 := inptr^[4*RowSize + 4];
      tmp3 := inptr^[6*RowSize + 6];
      tmp12 := (tmp0 + tmp3) * 2;
      z10 := tmp0 - tmp3;

      tmp1 := inptr^[6*RowSize + 4];
      tmp2 := inptr^[4*RowSize + 6];
      tmp13 :=-(tmp1 - tmp2)*2;
      z11 := tmp1 + tmp2;

      tmp0 := Multiply(z10 + z11, FP_I_C4_2);
      tmp1 := Multiply(z10 - z11, FP_I_C4_2);


      inptr^[4*RowSize + 4] := tmp12 + tmp0;
      inptr^[6*RowSize + 4] := tmp1 + tmp13;

      inptr^[4*RowSize + 6] := tmp1 - tmp13;
      inptr^[6*RowSize + 6] := tmp12 - tmp0;
    end;

    { R2 x R2 }

    for row := 0 to 7 do
    BEGIN
      { Odd part }
      tmp7 := inptr^[row*RowSize + 7];
      tmp6 := inptr^[row*RowSize + 6] - tmp7;
      tmp5 := inptr^[row*RowSize + 5] - tmp6;
      tmp4 :=-inptr^[row*RowSize + 4] - tmp5;

      { even part }
      tmp0 := inptr^[row*RowSize + 0];
      tmp1 := inptr^[row*RowSize + 1];
      tmp10 := tmp0 + tmp1;
      tmp11 := tmp0 - tmp1;

      tmp2 := inptr^[row*RowSize + 2];
      tmp13 := inptr^[row*RowSize + 3];
      tmp12 := tmp2 - tmp13;

      tmp0 := tmp10 + tmp13;
      tmp3 := tmp10 - tmp13;
      inptr^[row*RowSize + 0] := (tmp0 + tmp7);
      inptr^[row*RowSize + 7] := (tmp0 - tmp7);

      inptr^[row*RowSize + 3] := (tmp3 + tmp4);
      inptr^[row*RowSize + 4] := (tmp3 - tmp4);

      tmp1 := tmp11 + tmp12;
      tmp2 := tmp11 - tmp12;

      inptr^[row*RowSize + 1] := (tmp1 + tmp6);
      inptr^[row*RowSize + 6] := (tmp1 - tmp6);

      inptr^[row*RowSize + 2] := (tmp2 + tmp5);
      inptr^[row*RowSize + 5] := (tmp2 - tmp5);
    END;

    for ctr := 0 to pred(DCTSIZE) do
    BEGIN
      outptr := JSAMPROW(@output_buf^[ctr]^[output_col]);
      { even part }
      tmp0 := inptr^[0*RowSize + ctr];
      tmp1 := inptr^[1*RowSize + ctr];
      tmp2 := inptr^[2*RowSize + ctr];
      tmp3 := inptr^[3*RowSize + ctr];

      tmp10 := tmp0 + tmp1;
      tmp11 := tmp0 - tmp1;

      tmp13 := tmp3;
      tmp12 := tmp2 - tmp3;

      tmp0 := tmp10 + tmp13;
      tmp3 := tmp10 - tmp13;

      tmp1 := tmp11 + tmp12;
      tmp2 := tmp11 - tmp12;

      { Odd part }
      tmp4 := inptr^[4*RowSize + ctr];
      tmp5 := inptr^[5*RowSize + ctr];
      tmp6 := inptr^[6*RowSize + ctr];
      tmp7 := inptr^[7*RowSize + ctr];

      tmp6 := tmp6 - tmp7;
      tmp5 := tmp5 - tmp6;
      tmp4 :=-tmp4 - tmp5;

      outptr^[0*RowSize + ctr] := DeScale(tmp0 + tmp7);
      outptr^[7*RowSize + ctr] := DeScale(tmp0 - tmp7);

      outptr^[1*RowSize + ctr] := DeScale(tmp1 + tmp6);
      outptr^[6*RowSize + ctr] := DeScale(tmp1 - tmp6);

      outptr^[2*RowSize + ctr] := DeScale(tmp2 + tmp5);
      outptr^[5*RowSize + ctr] := DeScale(tmp2 - tmp5);

      outptr^[3*RowSize + ctr] := DeScale(tmp3 + tmp4);
      outptr^[4*RowSize + ctr] := DeScale(tmp3 - tmp4);


      { Final output stage: scale down by a factor of 8 and range-limit }

      outptr^[0] := range_limit^[IDESCALE(tmp0 + tmp7, PASS1_BITS+3)
                              and RANGE_MASK];
      outptr^[7] := range_limit^[IDESCALE(tmp0 - tmp7, PASS1_BITS+3)
                              and RANGE_MASK];
      outptr^[1] := range_limit^[IDESCALE(tmp1 + tmp6, PASS1_BITS+3)
                              and RANGE_MASK];
      outptr^[6] := range_limit^[IDESCALE(tmp1 - tmp6, PASS1_BITS+3)
                              and RANGE_MASK];
      outptr^[2] := range_limit^[IDESCALE(tmp2 + tmp5, PASS1_BITS+3)
                              and RANGE_MASK];
      outptr^[5] := range_limit^[IDESCALE(tmp2 - tmp5, PASS1_BITS+3)
                              and RANGE_MASK];
      outptr^[4] := range_limit^[IDESCALE(tmp3 + tmp4, PASS1_BITS+3)
                              and RANGE_MASK];
      outptr^[3] := range_limit^[IDESCALE(tmp3 - tmp4, PASS1_BITS+3)
                              and RANGE_MASK];
    END;

    Inc(bbo);
    Inc(inptr);
  End;
End; {----------------------------------------}


{GLOBAL}
procedure jpeg_idct_i2d (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
                         coef_block : JCOEFPTR;
                         output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);

procedure Feig_2D_IDCT(coef_block :imatrix;
                       output_buf : JSAMPARRAY);
Const
  CONST_IC4 = 1.414213562; { 1/0.707106781; }
  FP_IC4    = Integer(Round(IFX_CONST*CONST_IC4));
  FP_I_C4_2  = FP_IC4;

  Function Descale(x : integer):integer;
  begin
    DeScale := (x+ (4 shl PASS_BITS)) div (8 shl PASS_BITS);
    { DeScale := x sar (3 + PASS_BITS);
      Borland Pascal SHR is unsigned }
  end;
  {
  function Multiply(X, Y: Integer): integer;
  begin
    Multiply := Integer( X*LongInt(Y) div IFX_CONST);
  end;
  }
  function Multiply(X, Y: Integer): integer; assembler;
  asm
    mov ax, X
    imul Y
    mov al, ah
    mov ah, dl
  end;


var
  z10, z11, z12, z13,
  tmp0,tmp1,tmp2,tmp3,
  tmp4,tmp5,tmp6,tmp7,
  tmp10,tmp11,
  tmp12,tmp13 : integer;
  column, row : byte;

  Procedure N1(var x, y : integer);   { rotator 1 }
  Const
    FP_a5 = Integer(Round(IFX_CONST*1.847759065));
    FP_a4 = Integer(Round(IFX_CONST*2.613125930));
    FP_a2 = Integer(Round(IFX_CONST*1.082392200));
  var
    z5, tmp : integer;
  begin
    tmp := x;

    z5 := Multiply(tmp + y, FP_a5);  { c6 }
    x := Multiply(y, FP_a2) - z5;  { c2-c6 }
    y := Multiply(tmp, -FP_a4) + z5;  { c2+c6 }
  end;

  Procedure N2(var x, y : integer); { N1 scaled by c4 }
  Const
    FP_b5 = Integer(Round(IFX_CONST*1.847759065*CONST_IC4));
    FP_b4 = Integer(Round(IFX_CONST*2.613125930*CONST_IC4));
    FP_b2 = Integer(Round(IFX_CONST*1.082392200*CONST_IC4));
  var
    z5, tmp : integer;
  begin
    tmp := x;

    z5 := Multiply(tmp + y, FP_b5);
    x := Multiply(y, FP_b2) - z5;
    y := Multiply(tmp,-FP_b4) + z5;
  end;

var
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7 : DCTELEM;
  tmp10, tmp11, tmp12, tmp13 : DCTELEM;
  z10, z11, z12, z13 : DCTELEM;
  inptr : JCOEFPTR;

  quantptr : IFAST_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;       { buffers data between passes }
  {SHIFT_TEMPS                  { for DESCALE }
  {ISHIFT_TEMPS                 { for IDESCALE }
var
  dcval : int;
var
  dcval_ : JSAMPLE;
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));
  { Pass 1: process columns from input, store into work array. }

  inptr := coef_block;
  quantptr := IFAST_MULT_TYPE_FIELD_PTR(compptr^.dct_table);
  wsptr := @workspace;

  { R1 x R1 }
  for ctr := pred(DCTSIZE) downto 0 do
  BEGIN
    { even part }
    tmp1 := DEQUANTIZE(inptr^[DCTSIZE*2], quantptr^[DCTSIZE*2]);
    tmp3 := DEQUANTIZE(inptr^[DCTSIZE*6], quantptr^[DCTSIZE*6]);

    wsptr^[DCTSIZE*0] := int (DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]));
    wsptr^[DCTSIZE*1] := int (DEQUANTIZE(inptr^[DCTSIZE*4], quantptr^[DCTSIZE*4]);

    { Odd part }

    tmp6 := DEQUANTIZE(inptr^[DCTSIZE*5], quantptr^[DCTSIZE*5]);
    tmp4 := DEQUANTIZE(inptr^[DCTSIZE*1], quantptr^[DCTSIZE*1]);
    tmp7 := DEQUANTIZE(inptr^[DCTSIZE*7], quantptr^[DCTSIZE*7]);
    tmp5 := DEQUANTIZE(inptr^[DCTSIZE*3], quantptr^[DCTSIZE*3]);


    z13 := tmp6 + tmp5;
    wsptr^[DCTSIZE*4] := int (tmp6 - tmp5);

    z11 := tmp4 + tmp7;
    wsptr^[DCTSIZE*6] := int (tmp4 - tmp7);

    wsptr^[DCTSIZE*7] := int (z11 + z13);
    wsptr^[DCTSIZE*5] := int (z11 - z13);

    wsptr^[DCTSIZE*3] := int (tmp1 + tmp3);
    wsptr^[DCTSIZE*2] := int (tmp1 - tmp3);

    Inc(JCOEF_PTR(inptr));              { advance pointers to next column }
    Inc(IFAST_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));
  END;

  wsptr := @workspace[DCTSIZE*pred(DCTSIZE)];
  for row := pred(DCTSIZE) downto 0 do
  BEGIN
    { Odd part }
    tmp5 := DCTELEM(wsptr^[1]);
    tmp7 := DCTELEM(wsptr^[3]);

    { even part }

    {noop:
    tmp0 := DCTELEM(wsptr^[0]);
    wsptr^[0] := DCTELEM(tmp0);}

    {tmp2 := DCTELEM(wsptr^[4]);}
    wsptr^[1] := wsptr^[4];

    tmp1 := DCTELEM(wsptr^[2]);
    tmp3 := DCTELEM(wsptr^[6]);

    wsptr^[2] := DCTELEM(tmp1 - tmp3);
    wsptr^[3] := DCTELEM(tmp1 + tmp3);

    { Odd part }
    tmp4 := DCTELEM(wsptr^[5]);
    tmp6 := DCTELEM(wsptr^[7]);

    z13 := tmp4 + tmp7;
    wsptr^[4] := DCTELEM(tmp4 - tmp7);

    z11 := tmp5 + tmp6;
    wsptr^[6] := DCTELEM(tmp5 - tmp6);

    wsptr^[7] := DCTELEM(z11 + z13);
    wsptr^[5] := DCTELEM(z11 - z13);
    Dec(int_ptr(wsptr), DCTSIZE);       { advance pointer to previous row }
  END;

  { M x M tensor }
  wsptr := @workspace[DCTSIZE*0];
  for row := 0 to pred(DCTSIZE) do
  begin
    Case row of
    0,1,3,7: { M1 }
      begin
        wsptr^[2] := Multiply(wsptr^[2], FP_IC4);     { 2/c4 }
        wsptr^[5] := Multiply(wsptr^[5], FP_IC4);     { 2/c4 }

        N1(wsptr^[ 4], wsptr^[ 6]);
      end;
    2,5: { M2 }
      begin
        wsptr^[0] := Multiply(wsptr^[0], FP_IC4);
        wsptr^[1] := Multiply(wsptr^[1], FP_IC4);
        wsptr^[3] := Multiply(wsptr^[3], FP_IC4);
        wsptr^[7] := Multiply(wsptr^[7], FP_IC4);

        wsptr^[2] := wsptr^[2] * 2;  { shift }
        wsptr^[5] := wsptr^[5] * 2;

        N2(wsptr^[4], wsptr^[6]);
      end;
    end; { Case }
    Inc(int_ptr(wsptr), DCTSIZE);       { advance pointer to next row }
  end;

  { M x N tensor }
  { rows 4,6 }
  begin
    N1(workspace[DCTSIZE*4+0], workspace[DCTSIZE*6+0]);
    N1(workspace[DCTSIZE*4+1], workspace[DCTSIZE*6+1]);
    N1(workspace[DCTSIZE*4+3], workspace[DCTSIZE*6+3]);
    N1(workspace[DCTSIZE*4+7], workspace[DCTSIZE*6+7]);

    N2(workspace[DCTSIZE*4+2], workspace[DCTSIZE*6+2]);
    N2(workspace[DCTSIZE*4+5], workspace[DCTSIZE*6+5]);

    { N3 }
    tmp0 := workspace[DCTSIZE*4,4];
    tmp1 := workspace[DCTSIZE*6,4];
    tmp2 := workspace[DCTSIZE*4,6];
    tmp3 := workspace[DCTSIZE*6,6];

    { two inverse matrices => same as FDCT }
    z10 := tmp0 - tmp3;
    z11 := tmp1 + tmp2;

    z12 := tmp0 + tmp3;
    z13 := tmp1 - tmp2;

    tmp0 := Multiply(z10 + z11, FP_I_C4_2);
    tmp1 := Multiply(z10 - z11, FP_I_C4_2);

    tmp2 := z12 * 2;       { shifts }
    tmp3 := z13 * (-2);


    workspace[DCTSIZE*4,4] := tmp2 + tmp0;
    workspace[DCTSIZE*6,4] := tmp1 + tmp3;

    workspace[DCTSIZE*4,6] := tmp1 - tmp3;
    workspace[DCTSIZE*6,6] := tmp2 - tmp0;
  end;

  { R2 x R2 }

  wsptr := @workspace;
  for row := 0 to pred(DCTSIZE) do
  BEGIN
    { even part }
    tmp0 := wsptr^[0];
    tmp2 := wsptr^[1];
    tmp1 := wsptr^[2];
    tmp3 := wsptr^[3];

    tmp10 := tmp0 + tmp2;
    tmp11 := tmp0 - tmp2;

    tmp12 := tmp1 - tmp3;
    tmp13 := tmp3;

    tmp0 := tmp10 + tmp13;
    tmp3 := tmp10 - tmp13;

    tmp2 := tmp11 + tmp12;
    tmp1 := tmp11 - tmp12;

    { Odd part }
    tmp4 := wsptr^[4];
    tmp5 := wsptr^[5];
    tmp6 := wsptr^[6];
    tmp7 := wsptr^[7];

    tmp6 := tmp6 - tmp7;
    tmp5 := tmp5 - tmp6;
    tmp4 :=-tmp4 - tmp5;

    wsptr^[0] := (tmp0 + tmp7);
    wsptr^[7] := (tmp0 - tmp7);

    wsptr^[1] := (tmp2 + tmp6);
    wsptr^[6] := (tmp2 - tmp6);

    wsptr^[2] := (tmp1 + tmp5);
    wsptr^[5] := (tmp1 - tmp5);

    wsptr^[3] := (tmp3 + tmp4);
    wsptr^[4] := (tmp3 - tmp4);

    Inc(int_ptr(wsptr), DCTSIZE);       { advance pointer to next row }
  END;

  wsptr := @workspace;
  for ctr := 0 to pred(DCTSIZE) do
  BEGIN
    outptr := JSAMPROW(@output_buf^[ctr]^[output_col]);
    { even part }
    tmp0 := wsptr[0];
    tmp1 := wsptr[1];
    tmp2 := wsptr[2];
    tmp3 := wsptr[3];

    tmp10 := tmp0 + tmp1;
    tmp11 := tmp0 - tmp1;

    tmp13 := tmp3;
    tmp12 := tmp2 - tmp3;

    tmp0 := tmp10 + tmp13;
    tmp3 := tmp10 - tmp13;

    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    { Odd part }
    tmp4 := wsptr[4];
    tmp5 := wsptr[5];
    tmp6 := wsptr[6];
    tmp7 := wsptr[7];

    tmp6 := tmp6 - tmp7;
    tmp5 := tmp5 - tmp6;
    tmp4 :=-tmp4 - tmp5;

    { Final output stage: scale down by a factor of 8 and range-limit }

    outptr^[0] := range_limit^[IDESCALE(tmp0 + tmp7, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[7] := range_limit^[IDESCALE(tmp0 - tmp7, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[1] := range_limit^[IDESCALE(tmp1 + tmp6, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[6] := range_limit^[IDESCALE(tmp1 - tmp6, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[2] := range_limit^[IDESCALE(tmp2 + tmp5, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[5] := range_limit^[IDESCALE(tmp2 - tmp5, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[4] := range_limit^[IDESCALE(tmp3 + tmp4, PASS1_BITS+3)
                            and RANGE_MASK];
    outptr^[3] := range_limit^[IDESCALE(tmp3 - tmp4, PASS1_BITS+3)
                            and RANGE_MASK];
    Inc(int_ptr(wsptr));
  END;
End; {----------------------------------------}


{----------------------------------------------------------------------}

