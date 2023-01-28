unit imjidctred;


{ This file contains inverse-DCT routines that produce reduced-size output:
  either 4x4, 2x2, or 1x1 pixels from an 8x8 DCT block.

  The implementation is based on the Loeffler, Ligtenberg and Moschytz (LL&M)
  algorithm used in jidctint.c.  We simply replace each 8-to-8 1-D IDCT step
  with an 8-to-4 step that produces the four averages of two adjacent outputs
  (or an 8-to-2 step producing two averages of four outputs, for 2x2 output).
  These steps were derived by computing the corresponding values at the end
  of the normal LL&M code, then simplifying as much as possible.

  1x1 is trivial: just take the DC coefficient divided by 8.

  See jidctint.c for additional comments. }


{ Original : jidctred.c ; Copyright (C) 1994-1998, Thomas G. Lane. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjpeglib,
  imjdct;         	{ Private declarations for DCT subsystem }

{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 1x1 output block. }

{GLOBAL}
procedure jpeg_idct_1x1 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
	                 output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);

{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 2x2 output block. }

{GLOBAL}
procedure jpeg_idct_2x2 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
                         output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);

{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 4x4 output block. }

{GLOBAL}
procedure jpeg_idct_4x4 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
	                 output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);

implementation

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}


{ Scaling is the same as in jidctint.c. }

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
  FIX_0_211164243 = INT32(Round((INT32(1) shl CONST_BITS) * 0.211164243)); {1730}
  FIX_0_509795579 = INT32(Round((INT32(1) shl CONST_BITS) * 0.509795579)); {4176}
  FIX_0_601344887 = INT32(Round((INT32(1) shl CONST_BITS) * 0.601344887)); {4926}
  FIX_0_720959822 = INT32(Round((INT32(1) shl CONST_BITS) * 0.720959822)); {5906}
  FIX_0_765366865 = INT32(Round((INT32(1) shl CONST_BITS) * 0.765366865)); {6270}
  FIX_0_850430095 = INT32(Round((INT32(1) shl CONST_BITS) * 0.850430095)); {6967}
  FIX_0_899976223 = INT32(Round((INT32(1) shl CONST_BITS) * 0.899976223)); {7373}
  FIX_1_061594337 = INT32(Round((INT32(1) shl CONST_BITS) * 1.061594337)); {8697}
  FIX_1_272758580 = INT32(Round((INT32(1) shl CONST_BITS) * 1.272758580)); {10426}
  FIX_1_451774981 = INT32(Round((INT32(1) shl CONST_BITS) * 1.451774981)); {11893}
  FIX_1_847759065 = INT32(Round((INT32(1) shl CONST_BITS) * 1.847759065)); {15137}
  FIX_2_172734803 = INT32(Round((INT32(1) shl CONST_BITS) * 2.172734803)); {17799}
  FIX_2_562915447 = INT32(Round((INT32(1) shl CONST_BITS) * 2.562915447)); {20995}
  FIX_3_624509785 = INT32(Round((INT32(1) shl CONST_BITS) * 3.624509785)); {29692}


{ Multiply an INT32 variable by an INT32 constant to yield an INT32 result.
  For 8-bit samples with the recommended scaling, all the variable
  and constant values involved are no more than 16 bits wide, so a
  16x16->32 bit multiply can be used instead of a full 32x32 multiply.
  For 12-bit samples, a full 32-bit multiplication will be needed. }

{$ifdef BITS_IN_JSAMPLE_IS_8}

   {function Multiply(X, Y: Integer): integer; assembler;
   asm
     mov ax, X
     imul Y
     mov al, ah
     mov ah, dl
   end;}

   {MULTIPLY16C16(var,const)}
   function Multiply(X, Y: Integer): INT32;
   begin
     Multiply := X*INT32(Y);
   end;


{$else}
   function Multiply(X, Y: INT32): INT32;
   begin
     Multiply := X*Y;
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

{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 4x4 output block. }

{GLOBAL}
procedure jpeg_idct_4x4 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
	                 output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array[0..(DCTSIZE*4)-1] of int; { buffers data between passes }
var
  tmp0, tmp2, tmp10, tmp12 : INT32;
  z1, z2, z3, z4 : INT32;
  inptr : JCOEFPTR;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;	{ buffers data between passes }
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

  inptr := coef_block;
  quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  wsptr := @workspace;
  for ctr := DCTSIZE downto 1 do
  begin
    { Don't bother to process column 4, because second pass won't use it }
    if (ctr = DCTSIZE-4) then
    begin
      Inc(JCOEF_PTR(inptr));
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));

      continue;
    end;
    if (inptr^[DCTSIZE*1]=0) and (inptr^[DCTSIZE*2]=0) and (inptr^[DCTSIZE*3]=0) and
       (inptr^[DCTSIZE*5]=0) and (inptr^[DCTSIZE*6]=0) and (inptr^[DCTSIZE*7]=0) then
    begin
      { AC terms all zero; we need not examine term 4 for 4x4 output }
      dcval := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) *
                      quantptr^[DCTSIZE*0]) shl PASS1_BITS;

      wsptr^[DCTSIZE*0] := dcval;
      wsptr^[DCTSIZE*1] := dcval;
      wsptr^[DCTSIZE*2] := dcval;
      wsptr^[DCTSIZE*3] := dcval;

      Inc(JCOEF_PTR(inptr));
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));

      continue;
    end;

    { Even part }

    tmp0 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) * quantptr^[DCTSIZE*0]);

    tmp0 := tmp0 shl (CONST_BITS+1);

    z2 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*2]) * quantptr^[DCTSIZE*2]);
    z3 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*6]) * quantptr^[DCTSIZE*6]);

    tmp2 := MULTIPLY(z2, FIX_1_847759065) + MULTIPLY(z3, - FIX_0_765366865);

    tmp10 := tmp0 + tmp2;
    tmp12 := tmp0 - tmp2;

    { Odd part }

    z1 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*7]) * quantptr^[DCTSIZE*7];
    z2 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*5]) * quantptr^[DCTSIZE*5];
    z3 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*3]) * quantptr^[DCTSIZE*3];
    z4 := ISLOW_MULT_TYPE(inptr^[DCTSIZE*1]) * quantptr^[DCTSIZE*1];

    tmp0 := MULTIPLY(z1, - FIX_0_211164243) { sqrt(2) * (c3-c1) }
	  + MULTIPLY(z2, FIX_1_451774981) { sqrt(2) * (c3+c7) }
	  + MULTIPLY(z3, - FIX_2_172734803) { sqrt(2) * (-c1-c5) }
	  + MULTIPLY(z4, FIX_1_061594337); { sqrt(2) * (c5+c7) }

    tmp2 := MULTIPLY(z1, - FIX_0_509795579) { sqrt(2) * (c7-c5) }
	  + MULTIPLY(z2, - FIX_0_601344887) { sqrt(2) * (c5-c1) }
	  + MULTIPLY(z3, FIX_0_899976223) { sqrt(2) * (c3-c7) }
	  + MULTIPLY(z4, FIX_2_562915447); { sqrt(2) * (c1+c3) }

    { Final output stage }

    wsptr^[DCTSIZE*0] := int(DESCALE(tmp10 + tmp2, CONST_BITS-PASS1_BITS+1));
    wsptr^[DCTSIZE*3] := int(DESCALE(tmp10 - tmp2, CONST_BITS-PASS1_BITS+1));
    wsptr^[DCTSIZE*1] := int(DESCALE(tmp12 + tmp0, CONST_BITS-PASS1_BITS+1));
    wsptr^[DCTSIZE*2] := int(DESCALE(tmp12 - tmp0, CONST_BITS-PASS1_BITS+1));

    Inc(JCOEF_PTR(inptr));
    Inc(ISLOW_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));
  end;

  { Pass 2: process 4 rows from work array, store into output array. }

  wsptr := @workspace;
  for ctr := 0 to pred(4) do
  begin
    outptr := JSAMPROW(@ output_buf^[ctr]^[output_col]);
    { It's not clear whether a zero row test is worthwhile here ... }

{$ifndef NO_ZERO_ROW_TEST}
    if (wsptr^[1]=0) and (wsptr^[2]=0) and (wsptr^[3]=0) and
       (wsptr^[5]=0) and (wsptr^[6]=0) and (wsptr^[7]=0) then
    begin
      { AC terms all zero }
      dcval_ := range_limit^[int(DESCALE(INT32(wsptr^[0]), PASS1_BITS+3))
				  and RANGE_MASK];

      outptr^[0] := dcval_;
      outptr^[1] := dcval_;
      outptr^[2] := dcval_;
      outptr^[3] := dcval_;

      Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
      continue;
    end;
{$endif}

    { Even part }

    tmp0 := (INT32(wsptr^[0])) shl (CONST_BITS+1);

    tmp2 := MULTIPLY(INT32(wsptr^[2]), FIX_1_847759065)
	  + MULTIPLY(INT32(wsptr^[6]), - FIX_0_765366865);

    tmp10 := tmp0 + tmp2;
    tmp12 := tmp0 - tmp2;

    { Odd part }

    z1 := INT32(wsptr^[7]);
    z2 := INT32(wsptr^[5]);
    z3 := INT32(wsptr^[3]);
    z4 := INT32(wsptr^[1]);

    tmp0 := MULTIPLY(z1, - FIX_0_211164243) { sqrt(2) * (c3-c1) }
	  + MULTIPLY(z2, FIX_1_451774981) { sqrt(2) * (c3+c7) }
	  + MULTIPLY(z3, - FIX_2_172734803) { sqrt(2) * (-c1-c5) }
	  + MULTIPLY(z4, FIX_1_061594337); { sqrt(2) * (c5+c7) }

    tmp2 := MULTIPLY(z1, - FIX_0_509795579) { sqrt(2) * (c7-c5) }
	  + MULTIPLY(z2, - FIX_0_601344887) { sqrt(2) * (c5-c1) }
	  + MULTIPLY(z3, FIX_0_899976223) { sqrt(2) * (c3-c7) }
	  + MULTIPLY(z4, FIX_2_562915447); { sqrt(2) * (c1+c3) }

    { Final output stage }

    outptr^[0] := range_limit^[ int(DESCALE(tmp10 + tmp2,
					  CONST_BITS+PASS1_BITS+3+1))
			    and RANGE_MASK];
    outptr^[3] := range_limit^[ int(DESCALE(tmp10 - tmp2,
					  CONST_BITS+PASS1_BITS+3+1))
			    and RANGE_MASK];
    outptr^[1] := range_limit^[ int(DESCALE(tmp12 + tmp0,
					  CONST_BITS+PASS1_BITS+3+1))
			    and RANGE_MASK];
    outptr^[2] := range_limit^[ int(DESCALE(tmp12 - tmp0,
				 	  CONST_BITS+PASS1_BITS+3+1))
			    and RANGE_MASK];

    Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
  end;
end;


{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 2x2 output block. }

{GLOBAL}
procedure jpeg_idct_2x2 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
                         output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array[0..(DCTSIZE*2)-1] of int; { buffers data between passes }
var
  tmp0, tmp10, z1 : INT32;
  inptr : JCOEFPTR;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkspace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace;  { buffers data between passes }
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

  inptr := coef_block;
  quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  wsptr := @workspace;
  for ctr := DCTSIZE downto 1 do
  begin
    { Don't bother to process columns 2,4,6 }
    if (ctr = DCTSIZE-2) or (ctr = DCTSIZE-4) or (ctr = DCTSIZE-6) then
    begin
      Inc(JCOEF_PTR(inptr));
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));

      continue;
    end;
    if (inptr^[DCTSIZE*1]=0) and (inptr^[DCTSIZE*3]=0) and
       (inptr^[DCTSIZE*5]=0) and (inptr^[DCTSIZE*7]=0) then
    begin
      { AC terms all zero; we need not examine terms 2,4,6 for 2x2 output }
      dcval := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) *
                 quantptr^[DCTSIZE*0]) shl PASS1_BITS;

      wsptr^[DCTSIZE*0] := dcval;
      wsptr^[DCTSIZE*1] := dcval;

      Inc(JCOEF_PTR(inptr));
      Inc(ISLOW_MULT_TYPE_PTR(quantptr));
      Inc(int_ptr(wsptr));

      continue;
    end;

    { Even part }

    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*0]) * quantptr^[DCTSIZE*0]);

    tmp10 := z1 shl (CONST_BITS+2);

    { Odd part }

    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*7]) * quantptr^[DCTSIZE*7]);
    tmp0 := MULTIPLY(z1, - FIX_0_720959822); { sqrt(2) * (c7-c5+c3-c1) }
    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*5]) * quantptr^[DCTSIZE*5]);
    Inc(tmp0, MULTIPLY(z1, FIX_0_850430095)); { sqrt(2) * (-c1+c3+c5+c7) }
    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*3]) * quantptr^[DCTSIZE*3]);
    Inc(tmp0, MULTIPLY(z1, - FIX_1_272758580)); { sqrt(2) * (-c1+c3-c5-c7) }
    z1 := (ISLOW_MULT_TYPE(inptr^[DCTSIZE*1]) * quantptr^[DCTSIZE*1]);
    Inc(tmp0, MULTIPLY(z1, FIX_3_624509785)); { sqrt(2) * (c1+c3+c5+c7) }

    { Final output stage }

    wsptr^[DCTSIZE*0] := int (DESCALE(tmp10 + tmp0, CONST_BITS-PASS1_BITS+2));
    wsptr^[DCTSIZE*1] := int (DESCALE(tmp10 - tmp0, CONST_BITS-PASS1_BITS+2));

    Inc(JCOEF_PTR(inptr));
    Inc(ISLOW_MULT_TYPE_PTR(quantptr));
    Inc(int_ptr(wsptr));
  end;

  { Pass 2: process 2 rows from work array, store into output array. }

  wsptr := @workspace;
  for ctr := 0 to pred(2) do
  begin
    outptr := JSAMPROW(@ output_buf^[ctr]^[output_col]);
    { It's not clear whether a zero row test is worthwhile here ... }

{$ifndef NO_ZERO_ROW_TEST}
    if (wsptr^[1]=0) and (wsptr^[3]=0) and (wsptr^[5]=0) and (wsptr^[7]= 0) then
    begin
      { AC terms all zero }
      dcval_ := range_limit^[ int(DESCALE(INT32(wsptr^[0]), PASS1_BITS+3))
				  and RANGE_MASK];

      outptr^[0] := dcval_;
      outptr^[1] := dcval_;

      Inc(int_ptr(wsptr), DCTSIZE);	{ advance pointer to next row }
      continue;
    end;
{$endif}

    { Even part }

    tmp10 := (INT32 (wsptr^[0])) shl (CONST_BITS+2);

    { Odd part }

    tmp0 := MULTIPLY( INT32(wsptr^[7]), - FIX_0_720959822) { sqrt(2) * (c7-c5+c3-c1) }
	  + MULTIPLY( INT32(wsptr^[5]), FIX_0_850430095) { sqrt(2) * (-c1+c3+c5+c7) }
	  + MULTIPLY( INT32(wsptr^[3]), - FIX_1_272758580) { sqrt(2) * (-c1+c3-c5-c7) }
	  + MULTIPLY( INT32(wsptr^[1]), FIX_3_624509785); { sqrt(2) * (c1+c3+c5+c7) }

    { Final output stage }

    outptr^[0] := range_limit^[ int(DESCALE(tmp10 + tmp0,
					  CONST_BITS+PASS1_BITS+3+2))
			    and RANGE_MASK];
    outptr^[1] := range_limit^[ int(DESCALE(tmp10 - tmp0,
					  CONST_BITS+PASS1_BITS+3+2))
			    and RANGE_MASK];

    Inc(int_ptr(wsptr), DCTSIZE);		{ advance pointer to next row }
  end;
end;


{ Perform dequantization and inverse DCT on one block of coefficients,
  producing a reduced-size 1x1 output block. }

{GLOBAL}
procedure jpeg_idct_1x1 (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
	                 coef_block : JCOEFPTR;
	                 output_buf : JSAMPARRAY;
                         output_col : JDIMENSION);
var
  dcval : int;
  quantptr : ISLOW_MULT_TYPE_FIELD_PTR;
  range_limit : JSAMPROW;
  {SHIFT_TEMPS}
begin
{ Each IDCT routine is responsible for range-limiting its results and
  converting them to unsigned form (0..MAXJSAMPLE).  The raw outputs could
  be quite far out of range if the input data is corrupt, so a bulletproof
  range-limiting step is required.  We use a mask-and-table-lookup method
  to do the combined operations quickly.  See the comments with
  prepare_range_limit_table (in jdmaster.c) for more info. }

  range_limit := JSAMPROW(@(cinfo^.sample_range_limit^[CENTERJSAMPLE]));
  { Pass 1: process columns from input, store into work array. }

  { We hardly need an inverse DCT routine for this: just take the
    average pixel value, which is one-eighth of the DC coefficient. }

  quantptr := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  dcval := (ISLOW_MULT_TYPE(coef_block^[0]) * quantptr^[0]);
  dcval := int (DESCALE( INT32(dcval), 3));

  output_buf^[0]^[output_col] := range_limit^[dcval and RANGE_MASK];
end;

end.
