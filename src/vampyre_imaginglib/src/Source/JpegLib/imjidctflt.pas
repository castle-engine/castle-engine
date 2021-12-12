unit imjidctflt;

{ This file contains a floating-point implementation of the
  inverse DCT (Discrete Cosine Transform).  In the IJG code, this routine
  must also perform dequantization of the input coefficients.

  This implementation should be more accurate than either of the integer
  IDCT implementations.  However, it may not give the same results on all
  machines because of differences in roundoff behavior.  Speed will depend
  on the hardware's floating point capacity.

  A 2-D IDCT can be done by 1-D IDCT on each column followed by 1-D IDCT
  on each row (or vice versa, but it's more convenient to emit a row at
  a time).  Direct algorithms are also available, but they are much more
  complex and seem not to be any faster when reduced to code.

  This implementation is based on Arai, Agui, and Nakajima's algorithm for
  scaled DCT.  Their original paper (Trans. IEICE E-71(11):1095) is in
  Japanese, but the algorithm is described in the Pennebaker & Mitchell
  JPEG textbook (see REFERENCES section in file README).  The following code
  is based directly on figure 4-8 in P&M.
  While an 8-point DCT cannot be done in less than 11 multiplies, it is
  possible to arrange the computation so that many of the multiplies are
  simple scalings of the final outputs.  These multiplies can then be
  folded into the multiplications or divisions by the JPEG quantization
  table entries.  The AA&N method leaves only 5 multiplies and 29 adds
  to be done in the DCT itself.
  The primary disadvantage of this method is that with a fixed-point
  implementation, accuracy is lost due to imprecise representation of the
  scaled quantization values.  However, that problem does not arise if
  we use floating point arithmetic. }

{ Original: jidctflt.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjpeglib,
  imjdct;         	{ Private declarations for DCT subsystem }

{ Perform dequantization and inverse DCT on one block of coefficients. }

{GLOBAL}
procedure jpeg_idct_float (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);

implementation

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}


{ Dequantize a coefficient by multiplying it by the multiplier-table
  entry; produce a float result. }

function DEQUANTIZE(coef : int; quantval : FAST_FLOAT) : FAST_FLOAT;
begin
  Dequantize := ( (coef) * quantval);
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
procedure jpeg_idct_float (cinfo : j_decompress_ptr;
                           compptr : jpeg_component_info_ptr;
		           coef_block : JCOEFPTR;
		           output_buf : JSAMPARRAY;
                           output_col : JDIMENSION);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array[0..DCTSIZE2-1] of FAST_FLOAT;
var
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7 : FAST_FLOAT;
  tmp10, tmp11, tmp12, tmp13 : FAST_FLOAT;
  z5, z10, z11, z12, z13 : FAST_FLOAT;
  inptr : JCOEFPTR;
  quantptr : FLOAT_MULT_TYPE_FIELD_PTR;
  wsptr : PWorkSpace;
  outptr : JSAMPROW;
  range_limit : JSAMPROW;
  ctr : int;
  workspace : TWorkspace; { buffers data between passes }
  {SHIFT_TEMPS}
var
  dcval : FAST_FLOAT;
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
  quantptr := FLOAT_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
  wsptr := @workspace;
  for ctr := pred(DCTSIZE) downto 0 do
  begin
    { Due to quantization, we will usually find that many of the input
      coefficients are zero, especially the AC terms.  We can exploit this
      by short-circuiting the IDCT calculation for any column in which all
      the AC terms are zero.  In that case each output is equal to the
      DC coefficient (with scale factor as needed).
      With typical images and quantization tables, half or more of the
      column DCT calculations can be simplified this way. }

    if (inptr^[DCTSIZE*1]=0) and (inptr^[DCTSIZE*2]=0) and
       (inptr^[DCTSIZE*3]=0) and (inptr^[DCTSIZE*4]=0) and
       (inptr^[DCTSIZE*5]=0) and (inptr^[DCTSIZE*6]=0) and
       (inptr^[DCTSIZE*7]=0) then
    begin
      { AC terms all zero }
      FAST_FLOAT(dcval) := DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]);

      wsptr^[DCTSIZE*0] := dcval;
      wsptr^[DCTSIZE*1] := dcval;
      wsptr^[DCTSIZE*2] := dcval;
      wsptr^[DCTSIZE*3] := dcval;
      wsptr^[DCTSIZE*4] := dcval;
      wsptr^[DCTSIZE*5] := dcval;
      wsptr^[DCTSIZE*6] := dcval;
      wsptr^[DCTSIZE*7] := dcval;

      Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
      Inc(FLOAT_MULT_TYPE_PTR(quantptr));
      Inc(FAST_FLOAT_PTR(wsptr));
      continue;
    end;

    { Even part }

    tmp0 := DEQUANTIZE(inptr^[DCTSIZE*0], quantptr^[DCTSIZE*0]);
    tmp1 := DEQUANTIZE(inptr^[DCTSIZE*2], quantptr^[DCTSIZE*2]);
    tmp2 := DEQUANTIZE(inptr^[DCTSIZE*4], quantptr^[DCTSIZE*4]);
    tmp3 := DEQUANTIZE(inptr^[DCTSIZE*6], quantptr^[DCTSIZE*6]);

    tmp10 := tmp0 + tmp2;	{ phase 3 }
    tmp11 := tmp0 - tmp2;

    tmp13 := tmp1 + tmp3;	{ phases 5-3 }
    tmp12 := (tmp1 - tmp3) * ({FAST_FLOAT}(1.414213562)) - tmp13; { 2*c4 }

    tmp0 := tmp10 + tmp13;	{ phase 2 }
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    { Odd part }

    tmp4 := DEQUANTIZE(inptr^[DCTSIZE*1], quantptr^[DCTSIZE*1]);
    tmp5 := DEQUANTIZE(inptr^[DCTSIZE*3], quantptr^[DCTSIZE*3]);
    tmp6 := DEQUANTIZE(inptr^[DCTSIZE*5], quantptr^[DCTSIZE*5]);
    tmp7 := DEQUANTIZE(inptr^[DCTSIZE*7], quantptr^[DCTSIZE*7]);

    z13 := tmp6 + tmp5;		{ phase 6 }
    z10 := tmp6 - tmp5;
    z11 := tmp4 + tmp7;
    z12 := tmp4 - tmp7;

    tmp7 := z11 + z13;		{ phase 5 }
    tmp11 := (z11 - z13) * ({FAST_FLOAT}(1.414213562)); { 2*c4 }

    z5 := (z10 + z12) * ({FAST_FLOAT}(1.847759065)); { 2*c2 }
    tmp10 := ({FAST_FLOAT}(1.082392200)) * z12 - z5; { 2*(c2-c6) }
    tmp12 := ({FAST_FLOAT}(-2.613125930)) * z10 + z5; { -2*(c2+c6) }

    tmp6 := tmp12 - tmp7;	{ phase 2 }
    tmp5 := tmp11 - tmp6;
    tmp4 := tmp10 + tmp5;

    wsptr^[DCTSIZE*0] := tmp0 + tmp7;
    wsptr^[DCTSIZE*7] := tmp0 - tmp7;
    wsptr^[DCTSIZE*1] := tmp1 + tmp6;
    wsptr^[DCTSIZE*6] := tmp1 - tmp6;
    wsptr^[DCTSIZE*2] := tmp2 + tmp5;
    wsptr^[DCTSIZE*5] := tmp2 - tmp5;
    wsptr^[DCTSIZE*4] := tmp3 + tmp4;
    wsptr^[DCTSIZE*3] := tmp3 - tmp4;

    Inc(JCOEF_PTR(inptr));		{ advance pointers to next column }
    Inc(FLOAT_MULT_TYPE_PTR(quantptr));
    Inc(FAST_FLOAT_PTR(wsptr));
  end;

  { Pass 2: process rows from work array, store into output array. }
  { Note that we must descale the results by a factor of 8 = 2**3. }

  wsptr := @workspace;
  for ctr := 0 to pred(DCTSIZE) do
  begin
    outptr := JSAMPROW(@(output_buf^[ctr]^[output_col]));
    { Rows of zeroes can be exploited in the same way as we did with columns.
      However, the column calculation has created many nonzero AC terms, so
      the simplification applies less often (typically 5% to 10% of the time).
      And testing floats for zero is relatively expensive, so we don't bother. }

    { Even part }

    tmp10 := wsptr^[0] + wsptr^[4];
    tmp11 := wsptr^[0] - wsptr^[4];

    tmp13 := wsptr^[2] + wsptr^[6];
    tmp12 := (wsptr^[2] - wsptr^[6]) * ({FAST_FLOAT}(1.414213562)) - tmp13;

    tmp0 := tmp10 + tmp13;
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    { Odd part }

    z13 := wsptr^[5] + wsptr^[3];
    z10 := wsptr^[5] - wsptr^[3];
    z11 := wsptr^[1] + wsptr^[7];
    z12 := wsptr^[1] - wsptr^[7];

    tmp7 := z11 + z13;
    tmp11 := (z11 - z13) * ({FAST_FLOAT}(1.414213562));

    z5 := (z10 + z12) * ({FAST_FLOAT}(1.847759065)); { 2*c2 }
    tmp10 := ({FAST_FLOAT}(1.082392200)) * z12 - z5; { 2*(c2-c6) }
    tmp12 := ({FAST_FLOAT}(-2.613125930)) * z10 + z5; { -2*(c2+c6) }

    tmp6 := tmp12 - tmp7;
    tmp5 := tmp11 - tmp6;
    tmp4 := tmp10 + tmp5;

    { Final output stage: scale down by a factor of 8 and range-limit }

    outptr^[0] := range_limit^[ int(DESCALE( INT32(Round((tmp0 + tmp7))), 3))
			    and RANGE_MASK];
    outptr^[7] := range_limit^[ int(DESCALE( INT32(Round((tmp0 - tmp7))), 3))
			    and RANGE_MASK];
    outptr^[1] := range_limit^[ int(DESCALE( INT32(Round((tmp1 + tmp6))), 3))
			    and RANGE_MASK];
    outptr^[6] := range_limit^[ int(DESCALE( INT32(Round((tmp1 - tmp6))), 3))
			    and RANGE_MASK];
    outptr^[2] := range_limit^[ int(DESCALE( INT32(Round((tmp2 + tmp5))), 3))
			    and RANGE_MASK];
    outptr^[5] := range_limit^[ int(DESCALE( INT32(Round((tmp2 - tmp5))), 3))
			    and RANGE_MASK];
    outptr^[4] := range_limit^[ int(DESCALE( INT32(Round((tmp3 + tmp4))), 3))
			    and RANGE_MASK];
    outptr^[3] := range_limit^[ int(DESCALE( INT32(Round((tmp3 - tmp4))), 3))
			    and RANGE_MASK];

    Inc(FAST_FLOAT_PTR(wsptr), DCTSIZE);	{ advance pointer to next row }
  end;
end;

end.
