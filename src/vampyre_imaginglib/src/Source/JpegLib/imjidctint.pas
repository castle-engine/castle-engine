unit imjidctint;
{$Q+}

{ This file contains a slow-but-accurate integer implementation of the
  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
  must also perform dequantization of the input coefficients.

  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
  on each row (or vice versa, but it's more convenient to emit a row at
  a time).  Direct algorithms are also available, but they are much more
  complex and seem not to be any faster when reduced to code.

  This implementation is based on an algorithm described in
    C. Loeffler, A. Ligtenberg and G. Moschytz, "Practical Fast 1-D DCT
    Algorithms with 11 Multiplications", Proc. Int'l. Conf. on Acoustics,
    Speech, and Signal Processing 1989 (ICASSP '89), pp. 988-991.
  The primary algorithm described there uses 11 multiplies and 29 adds.
  We use their alternate method with 12 multiplies and 32 adds.
  The advantage of this method is that no data path contains more than one
  multiplication; this allows a very simple and accurate implementation in
  scaled fixed-point arithmetic, with a minimal number of shifts. }

{ Original : jidctint.c ;  Copyright (C) 1991-1998, Thomas G. Lane. }


interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjpeglib,
  imjdct;         { Private declarations for DCT subsystem }

{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_islow (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);

implementation

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}

{ The poop on this scaling stuff is as follows:

  Each 1-D IDCT step produces outputs which are a factor of sqrt(N)
  larger than the true IDCT outputs.  The final outputs are therefore
  a factor of N larger than desired; since N=8 this can be cured by
  a simple right shift at the end of the algorithm.  The advantage of
  this arrangement is that we save two multiplications per 1-D IDCT,
  because the y0 and y4 inputs need not be divided by sqrt(N).

  We have to do addition and subtraction of the integer inputs, which
  is no problem, and multiplication by fractional constants, which is
  a problem to do in integer arithmetic.  We multiply all the constants
  by CONST_SCALE and convert them to integer constants (thus retaining
  CONST_BITS bits of precision in the constants).  After doing a
  multiplication we have to divide the product by CONST_SCALE, with proper
  rounding, to produce the correct output.  This division can be done
  cheaply as a right shift of CONST_BITS bits.  We postpone shifting
  as long as possible so that partial sums can be added together with
  full fractional precision.

  The outputs of the first pass are scaled up by PASS1_BITS bits so that
  they are represented to better-than-integral precision.  These outputs
  require BITS_IN_JSAMPLE + PASS1_BITS + 3 bits; this fits in a 16-bit word
  with the recommended scaling.  (To scale up 12-bit sample data further, an
  intermediate INT32 array would be needed.)

  To avoid overflow of the 32-bit intermediate results in pass 2, we must
  have BITS_IN_JSAMPLE + CONST_BITS + PASS1_BITS <= 26.  Error analysis
  shows that the values given below are the most effective. }

{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  CONST_BITS = 13;
  PASS1_BITS = 2;
{$else}
const
  CONST_BITS = 13;
  PASS1_BITS = 1;	{ lose a little precision to avoid overflow }
{$endif}

const
  CONST_SCALE = (INT32(1) shl CONST_BITS);

const
  FIX_0_298631336 = INT32(Round(CONST_SCALE * 0.298631336));  {2446}
  FIX_0_390180644 = INT32(Round(CONST_SCALE * 0.390180644));  {3196}
  FIX_0_541196100 = INT32(Round(CONST_SCALE * 0.541196100));  {4433}
  FIX_0_765366865 = INT32(Round(CONST_SCALE * 0.765366865));  {6270}
  FIX_0_899976223 = INT32(Round(CONST_SCALE * 0.899976223));  {7373}
  FIX_1_175875602 = INT32(Round(CONST_SCALE * 1.175875602));  {9633}
  FIX_1_501321110 = INT32(Round(CONST_SCALE * 1.501321110));  {12299}
  FIX_1_847759065 = INT32(Round(CONST_SCALE * 1.847759065));  {15137}
  FIX_1_961570560 = INT32(Round(CONST_SCALE * 1.961570560));  {16069}
  FIX_2_053119869 = INT32(Round(CONST_SCALE * 2.053119869));  {16819}
  FIX_2_562915447 = INT32(Round(CONST_SCALE * 2.562915447));  {20995}
  FIX_3_072711026 = INT32(Round(CONST_SCALE * 3.072711026));  {25172}



{ Multiply an INT32 variable by an INT32 constant to yield an INT32 result.
  For 8-bit samples with the recommended scaling, all the variable
  and constant values involved are no more than 16 bits wide, so a
  16x16->32 bit multiply can be used instead of a full 32x32 multiply.
  For 12-bit samples, a full 32-bit multiplication will be needed. }

{$ifdef BITS_IN_JSAMPLE_IS_8}

   {$IFDEF BASM16}
     {$IFNDEF WIN32}
   {MULTIPLY16C16(var,const)}
   function Multiply(X, Y: Integer): integer; assembler;
   asm
     mov ax, X
     imul Y
     mov al, ah
     mov ah, dl
   end;
     {$ENDIF}
   {$ENDIF}

   function Multiply(X, Y: INT32): INT32;
   begin
     Multiply := INT32(X) * INT32(Y);
   end;


{$else}
  {#define MULTIPLY(var,const)  ((var) * (const))}
   function Multiply(X, Y: INT32): INT32;
   begin
     Multiply := INT32(X) * INT32(Y);
   end;
{$endif}


{ Dequantize a coefficient by multiplying it by the multiplier-table
  entry; produce an int result.  In this module, both inputs and result
  are 16 bits or less, so either int or short multiply will work. }

function DEQUANTIZE(coef,quantval : int) : int;
begin
  Dequantize := ( ISLOW_MULT_TYPE(coef) * quantval);
end;

{ Descale and correctly round an INT32 value that's scaled by N bits.
  We assume RIGHT_SHIFT rounds towards minus infinity, so adding
  the fudge factor is correct for either sign of X. }

function DESCALE(x : INT32; n : int) : INT32;
var
  shift_temp : INT32;
begin
{$ifdef RIGHT_SHIFT_IS_UNSIGNED}
  shift_temp := x + (INT32(1) shl (n-1));
  if shift_temp < 0 then
    Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
  else
    Descale :=  (shift_temp shr n);
{$else}
  Descale := (x + (INT32(1) shl (n-1)) shr n;
{$endif}
end;

{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_islow (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = coef_bits_field; { buffers data between passes }
var
  tmp0, tmp1, tmp2, tmp3 : INT32;
  tmp10, tmp11, tmp12, tmp13 : INT32;
  z1, z2, z3, z4, z5 : INT32;
  inptr : JCOEFPTR;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;
  {SHIFT_TEMPS}
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
  { Note results are scaled up by sqrt(8) compared to a true IDCT; }
  { furthermore, we scale the results by 2**PASS1_BITS. }

  inptr := coef_block;
  quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  wsptr := PWorkspace(@workspace);
  for ctr := pred(DCTSIZE) downto 0 do
  begin
    { Due to quantization, we will usually find that many of the input
      coefficients are zero, especially the AC terms.  We can exploit this
      by short-circuiting the IDCT calculation for any column in which all
      the AC terms are zero.  In that case each output is equal to the
      DC coefficient (with scale factor as needed).
      With typical images and quantization tables, half or more of the
      column DCT calculations can be simplified this way. }

    if ((inptr^[DCTSIZE*1]=0) and (inptr^[DCTSIZE*2]=0) and
        (inptr^[DCTSIZE*3]=0) and (inptr^[DCTSIZE*4]=0) and
        (inptr^[DCTSIZE*5]=0) and (inptr^[DCTSIZE*6]=0) and
	(inptr^[DCTSIZE*7]=0)) then
    begin
      { AC terms all zero }
      dcval := DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]) shl PASS1_BITS;

      wsptr^[DCTSIZE*0] := dcval;
      wsptr^[DCTSIZE*1] := dcval;
      wsptr^[DCTSIZE*2] := dcval;
      wsptr^[DCTSIZE*3] := dcval;
      wsptr^[DCTSIZE*4] := dcval;
      wsptr^[DCTSIZE*5] := dcval;
      wsptr^[DCTSIZE*6] := dcval;
      wsptr^[DCTSIZE*7] := dcval;

      Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));
      continue;
    end;

    { Even part: reverse the even part of the forward DCT. }
    { The rotator is sqrt(2)*c(-6). }

    z2 := DEQUANTIZE(inptr^[DCTSIZE*2], quantptr^[DCTSIZE*2]);
    z3 := DEQUANTIZE(inptr^[DCTSIZE*6], quantptr^[DCTSIZE*6]);

    z1 := MULTIPLY(z2 + z3, FIX_0_541196100);
    tmp2 := z1 + MULTIPLY(z3, - FIX_1_847759065);
    tmp3 := z1 + MULTIPLY(z2, FIX_0_765366865);

    z2 := DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]);
    z3 := DEQUANTIZE(inptr^[DCTSIZE*4], quantptr^[DCTSIZE*4]);

    tmp0 := (z2 + z3) shl CONST_BITS;
    tmp1 := (z2 - z3) shl CONST_BITS;

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    { Odd part per figure 8; the matrix is unitary and hence its
      transpose is its inverse.  i0..i3 are y7,y5,y3,y1 respectively. }

    tmp0 := DEQUANTIZE(inptr^[DCTSIZE*7], quantptr^[DCTSIZE*7]);
    tmp1 := DEQUANTIZE(inptr^[DCTSIZE*5], quantptr^[DCTSIZE*5]);
    tmp2 := DEQUANTIZE(inptr^[DCTSIZE*3], quantptr^[DCTSIZE*3]);
    tmp3 := DEQUANTIZE(inptr^[DCTSIZE*1], quantptr^[DCTSIZE*1]);

    z1 := tmp0 + tmp3;
    z2 := tmp1 + tmp2;
    z3 := tmp0 + tmp2;
    z4 := tmp1 + tmp3;
    z5 := MULTIPLY(z3 + z4, FIX_1_175875602); { sqrt(2) * c3 }

    tmp0 := MULTIPLY(tmp0, FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
    tmp1 := MULTIPLY(tmp1, FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
    tmp2 := MULTIPLY(tmp2, FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    tmp3 := MULTIPLY(tmp3, FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    z1 := MULTIPLY(z1, - FIX_0_899976223); { sqrt(2) * (c7-c3) }
    z2 := MULTIPLY(z2, - FIX_2_562915447); { sqrt(2) * (-c1-c3) }
    z3 := MULTIPLY(z3, - FIX_1_961570560); { sqrt(2) * (-c3-c5) }
    z4 := MULTIPLY(z4, - FIX_0_390180644); { sqrt(2) * (c5-c3) }

    Inc(z3, z5);
    Inc(z4, z5);

    Inc(tmp0, z1 + z3);
    Inc(tmp1, z2 + z4);
    Inc(tmp2, z2 + z3);
    Inc(tmp3, z1 + z4);

    { Final output stage: inputs are tmp10..tmp13, tmp0..tmp3 }

    wsptr^[DCTSIZE*0] := int (DESCALE(tmp10 + tmp3, CONST_BITS-PASS1_BITS));
    wsptr^[DCTSIZE*7] := int (DESCALE(tmp10 - tmp3, CONST_BITS-PASS1_BITS));
    wsptr^[DCTSIZE*1] := int (DESCALE(tmp11 + tmp2, CONST_BITS-PASS1_BITS));
    wsptr^[DCTSIZE*6] := int (DESCALE(tmp11 - tmp2, CONST_BITS-PASS1_BITS));
    wsptr^[DCTSIZE*2] := int (DESCALE(tmp12 + tmp1, CONST_BITS-PASS1_BITS));
    wsptr^[DCTSIZE*5] := int (DESCALE(tmp12 - tmp1, CONST_BITS-PASS1_BITS));
    wsptr^[DCTSIZE*3] := int (DESCALE(tmp13 + tmp0, CONST_BITS-PASS1_BITS));
    wsptr^[DCTSIZE*4] := int (DESCALE(tmp13 - tmp0, CONST_BITS-PASS1_BITS));

    Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
    Inc(ISLOW_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));
  end;

  { Pass 2: process rows from work array, store into output array. }
  { Note that we must descale the results by a factor of 8 == 2**3, }
  { and also undo the PASS1_BITS scaling. }

  wsptr := @workspace;
  for ctr := 0 to pred(DCTSIZE) do
  begin
    outptr := output_buf^[ctr];
    Inc(JSAMPLE_PTR(outptr), output_col);
    { Rows of zeroes can be exploited in the same way as we did with columns.
      However, the column calculation has created many nonzero AC terms, so
      the simplification applies less often (typically 5% to 10% of the time).
      On machines with very fast multiplication, it's possible that the
      test takes more time than it's worth.  In that case this section
      may be commented out. }

{$ifndef NO_ZERO_ROW_TEST}
    if ((wsptr^[1]=0) and (wsptr^[2]=0) and (wsptr^[3]=0) and (wsptr^[4]=0)
       and (wsptr^[5]=0) and (wsptr^[6]=0) and (wsptr^[7]=0)) then
    begin
      { AC terms all zero }
      JSAMPLE(dcval_) := range_limit^[int(DESCALE(INT32(wsptr^[0]),
                          PASS1_BITS+3)) and RANGE_MASK];

      outptr^[0] := dcval_;
      outptr^[1] := dcval_;
      outptr^[2] := dcval_;
      outptr^[3] := dcval_;
      outptr^[4] := dcval_;
      outptr^[5] := dcval_;
      outptr^[6] := dcval_;
      outptr^[7] := dcval_;

      Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
      continue;
    end;
{$endif}

    { Even part: reverse the even part of the forward DCT. }
    { The rotator is sqrt(2)*c(-6). }

    z2 := INT32 (wsptr^[2]);
    z3 := INT32 (wsptr^[6]);

    z1 := MULTIPLY(z2 + z3, FIX_0_541196100);
    tmp2 := z1 + MULTIPLY(z3, - FIX_1_847759065);
    tmp3 := z1 + MULTIPLY(z2, FIX_0_765366865);

    tmp0 := (INT32(wsptr^[0]) + INT32(wsptr^[4])) shl CONST_BITS;
    tmp1 := (INT32(wsptr^[0]) - INT32(wsptr^[4])) shl CONST_BITS;

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    { Odd part per figure 8; the matrix is unitary and hence its
      transpose is its inverse.  i0..i3 are y7,y5,y3,y1 respectively. }

    tmp0 := INT32(wsptr^[7]);
    tmp1 := INT32(wsptr^[5]);
    tmp2 := INT32(wsptr^[3]);
    tmp3 := INT32(wsptr^[1]);

    z1 := tmp0 + tmp3;
    z2 := tmp1 + tmp2;
    z3 := tmp0 + tmp2;
    z4 := tmp1 + tmp3;
    z5 := MULTIPLY(z3 + z4, FIX_1_175875602); { sqrt(2) * c3 }

    tmp0 := MULTIPLY(tmp0, FIX_0_298631336); { sqrt(2) * (-c1+c3+c5-c7) }
    tmp1 := MULTIPLY(tmp1, FIX_2_053119869); { sqrt(2) * ( c1+c3-c5+c7) }
    tmp2 := MULTIPLY(tmp2, FIX_3_072711026); { sqrt(2) * ( c1+c3+c5-c7) }
    tmp3 := MULTIPLY(tmp3, FIX_1_501321110); { sqrt(2) * ( c1+c3-c5-c7) }
    z1 := MULTIPLY(z1, - FIX_0_899976223); { sqrt(2) * (c7-c3) }
    z2 := MULTIPLY(z2, - FIX_2_562915447); { sqrt(2) * (-c1-c3) }
    z3 := MULTIPLY(z3, - FIX_1_961570560); { sqrt(2) * (-c3-c5) }
    z4 := MULTIPLY(z4, - FIX_0_390180644); { sqrt(2) * (c5-c3) }

    Inc(z3, z5);
    Inc(z4, z5);

    Inc(tmp0, z1 + z3);
    Inc(tmp1, z2 + z4);
    Inc(tmp2, z2 + z3);
    Inc(tmp3, z1 + z4);

    { Final output stage: inputs are tmp10..tmp13, tmp0..tmp3 }

    outptr^[0] := range_limit^[ int(DESCALE(tmp10 + tmp3,
					  CONST_BITS+PASS1_BITS+3))
			    and RANGE_MASK];
    outptr^[7] := range_limit^[ int(DESCALE(tmp10 - tmp3,
					  CONST_BITS+PASS1_BITS+3))
			    and RANGE_MASK];
    outptr^[1] := range_limit^[ int(DESCALE(tmp11 + tmp2,
					  CONST_BITS+PASS1_BITS+3))
			    and RANGE_MASK];
    outptr^[6] := range_limit^[ int(DESCALE(tmp11 - tmp2,
                                          CONST_BITS+PASS1_BITS+3))
                            and RANGE_MASK];
    outptr^[2] := range_limit^[ int(DESCALE(tmp12 + tmp1,
                                          CONST_BITS+PASS1_BITS+3))
                            and RANGE_MASK];
    outptr^[5] := range_limit^[ int(DESCALE(tmp12 - tmp1,
                                          CONST_BITS+PASS1_BITS+3))
                            and RANGE_MASK];
    outptr^[3] := range_limit^[ int(DESCALE(tmp13 + tmp0,
                                          CONST_BITS+PASS1_BITS+3))
                            and RANGE_MASK];
    outptr^[4] := range_limit^[ int(DESCALE(tmp13 - tmp0,
                                          CONST_BITS+PASS1_BITS+3))
                            and RANGE_MASK];

    Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
  end;
end;

end.
