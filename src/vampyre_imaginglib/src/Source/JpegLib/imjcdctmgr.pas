unit imjcdctmgr;

{ Original : jcdctmgr.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains the forward-DCT management logic.
  This code selects a particular DCT implementation to be used,
  and it performs related housekeeping chores including coefficient
  quantization. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjdeferr,
  imjerror,
  imjpeglib,
  imjdct,                 { Private declarations for DCT subsystem }
  imjfdctint, imjfdctfst, imjfdctflt;

{ Initialize FDCT manager. }

{GLOBAL}
procedure jinit_forward_dct (cinfo : j_compress_ptr);

implementation


{ Private subobject for this module }

type
  my_fdct_ptr = ^my_fdct_controller;
  my_fdct_controller = record
    pub : jpeg_forward_dct;	{ public fields }

    { Pointer to the DCT routine actually in use }
    do_dct : forward_DCT_method_ptr;

    { The actual post-DCT divisors --- not identical to the quant table
      entries, because of scaling (especially for an unnormalized DCT).
      Each table is given in normal array order. }

    divisors : array[0..NUM_QUANT_TBLS-1] of DCTELEM_FIELD_PTR;

  {$ifdef DCT_FLOAT_SUPPORTED}
    { Same as above for the floating-point case. }
    do_float_dct : float_DCT_method_ptr;
    float_divisors : array[0..NUM_QUANT_TBLS-1] of FAST_FLOAT_FIELD_PTR;
  {$endif}
  end;


{ Initialize for a processing pass.
  Verify that all referenced Q-tables are present, and set up
  the divisor table for each one.
  In the current implementation, DCT of all components is done during
  the first pass, even if only some components will be output in the
  first scan.  Hence all components should be examined here. }

{METHODDEF}
procedure start_pass_fdctmgr (cinfo : j_compress_ptr);  
var
  fdct : my_fdct_ptr;
  ci, qtblno, i : int;
  compptr : jpeg_component_info_ptr;
  qtbl : JQUANT_TBL_PTR;
  dtbl : DCTELEM_FIELD_PTR;
{$ifdef DCT_IFAST_SUPPORTED}
const
  CONST_BITS = 14;
  aanscales : array[0..DCTSIZE2-1] of INT16 =
         ({ precomputed values scaled up by 14 bits }
	  16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
	  22725, 31521, 29692, 26722, 22725, 17855, 12299,  6270,
	  21407, 29692, 27969, 25172, 21407, 16819, 11585,  5906,
	  19266, 26722, 25172, 22654, 19266, 15137, 10426,  5315,
	  16384, 22725, 21407, 19266, 16384, 12873,  8867,  4520,
	  12873, 17855, 16819, 15137, 12873, 10114,  6967,  3552,
	   8867, 12299, 11585, 10426,  8867,  6967,  4799,  2446,
	   4520,  6270,  5906,  5315,  4520,  3552,  2446,  1247);
  {SHIFT_TEMPS}

  { Descale and correctly round an INT32 value that's scaled by N bits.
    We assume RIGHT_SHIFT rounds towards minus infinity, so adding
    the fudge factor is correct for either sign of X. }

  function DESCALE(x : INT32; n : int) : INT32;
  var
    shift_temp : INT32;
  begin
    shift_temp := x + (INT32(1) shl (n-1));
  {$ifdef RIGHT_SHIFT_IS_UNSIGNED}
    if shift_temp < 0 then
      Descale :=  (shift_temp shr n) or ((not INT32(0)) shl (32-n))
    else
  {$endif}
      Descale :=  (shift_temp shr n);
  end;

{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
var
  fdtbl : FAST_FLOAT_FIELD_PTR;
  row, col : int;
const
  aanscalefactor : array[0..DCTSIZE-1] of double =
    (1.0, 1.387039845, 1.306562965, 1.175875602,
     1.0, 0.785694958, 0.541196100, 0.275899379);
{$endif}
begin
  fdct := my_fdct_ptr (cinfo^.fdct);
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    qtblno := compptr^.quant_tbl_no;
    { Make sure specified quantization table is present }
    if (qtblno < 0) or (qtblno >= NUM_QUANT_TBLS) or
       (cinfo^.quant_tbl_ptrs[qtblno] = NIL) then
      ERREXIT1(j_common_ptr(cinfo), JERR_NO_QUANT_TABLE, qtblno);
    qtbl := cinfo^.quant_tbl_ptrs[qtblno];
    { Compute divisors for this quant table }
    { We may do this more than once for same table, but it's not a big deal }
    case (cinfo^.dct_method) of
{$ifdef DCT_ISLOW_SUPPORTED}
    JDCT_ISLOW:
    begin
      { For LL&M IDCT method, divisors are equal to raw quantization
        coefficients multiplied by 8 (to counteract scaling). }

      if (fdct^.divisors[qtblno] = NIL) then
      begin
	fdct^.divisors[qtblno] := DCTELEM_FIELD_PTR(
	  cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				      DCTSIZE2 * SIZEOF(DCTELEM)) );
      end;
      dtbl := fdct^.divisors[qtblno];
      for i := 0 to pred(DCTSIZE2) do
      begin
	dtbl^[i] := (DCTELEM(qtbl^.quantval[i])) shl 3;
      end;
    end;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
    JDCT_IFAST:
      begin
        { For AA&N IDCT method, divisors are equal to quantization
          coefficients scaled by scalefactor[row]*scalefactor[col], where
            scalefactor[0] := 1
            scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7
          We apply a further scale factor of 8. }


	if (fdct^.divisors[qtblno] = NIL) then
        begin
	  fdct^.divisors[qtblno] := DCTELEM_FIELD_PTR(
	    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
					DCTSIZE2 * SIZEOF(DCTELEM)) );
	end;
	dtbl := fdct^.divisors[qtblno];
	for i := 0 to pred(DCTSIZE2) do
        begin
	  dtbl^[i] := DCTELEM(
                     {MULTIPLY16V16}
	    DESCALE( INT32(qtbl^.quantval[i]) * INT32 (aanscales[i]),
		     CONST_BITS-3) );
	end;
      end;
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}

    JDCT_FLOAT:
      begin
	{ For float AA&N IDCT method, divisors are equal to quantization
	  coefficients scaled by scalefactor[row]*scalefactor[col], where
	    scalefactor[0] := 1
	    scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7
	  We apply a further scale factor of 8.
	  What's actually stored is 1/divisor so that the inner loop can
	  use a multiplication rather than a division. }

	if (fdct^.float_divisors[qtblno] = NIL) then
        begin
	  fdct^.float_divisors[qtblno] := FAST_FLOAT_FIELD_PTR(
	    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
					DCTSIZE2 * SIZEOF(FAST_FLOAT)) );
	end;
	fdtbl := fdct^.float_divisors[qtblno];
	i := 0;
	for row := 0 to pred(DCTSIZE) do
        begin
	  for col := 0 to pred(DCTSIZE) do
          begin
	    fdtbl^[i] := {FAST_FLOAT}
	      (1.0 / (( {double}(qtbl^.quantval[i]) *
		       aanscalefactor[row] * aanscalefactor[col] * 8.0)));
	    Inc(i);
	  end;
	end;
      end;
{$endif}
    else
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
    end;
    Inc(compptr);
  end;
end;


{ Perform forward DCT on one or more blocks of a component.

  The input samples are taken from the sample_data[] array starting at
  position start_row/start_col, and moving to the right for any additional
  blocks. The quantized coefficients are returned in coef_blocks[]. }

{METHODDEF}
procedure forward_DCT (cinfo : j_compress_ptr;
                       compptr : jpeg_component_info_ptr;
	               sample_data : JSAMPARRAY;
                       coef_blocks : JBLOCKROW;
	               start_row : JDIMENSION;
                       start_col : JDIMENSION;
	               num_blocks : JDIMENSION);  
{ This version is used for integer DCT implementations. }
var
  { This routine is heavily used, so it's worth coding it tightly. }
  fdct : my_fdct_ptr;
  do_dct : forward_DCT_method_ptr;
  divisors : DCTELEM_FIELD_PTR;
  workspace : array[0..DCTSIZE2-1] of DCTELEM;	{ work area for FDCT subroutine }
  bi : JDIMENSION;
var
  {register} workspaceptr : DCTELEMPTR;
  {register} elemptr : JSAMPLE_PTR;
  {register} elemr : int;
{$ifndef DCTSIZE_IS_8}
var
  {register} elemc : int;
{$endif}
var
  {register} temp, qval : DCTELEM;
  {register} i : int;
  {register} output_ptr : JCOEFPTR;
begin
  fdct := my_fdct_ptr (cinfo^.fdct);
  do_dct := fdct^.do_dct;
  divisors := fdct^.divisors[compptr^.quant_tbl_no];

  Inc(JSAMPROW_PTR(sample_data), start_row);	{ fold in the vertical offset once }

  for bi := 0 to pred(num_blocks) do
  begin

    { Load data into workspace, applying unsigned->signed conversion }

    workspaceptr := @workspace[0];
    for elemr := 0 to pred(DCTSIZE) do
    begin
      elemptr := @sample_data^[elemr]^[start_col];
{$ifdef DCTSIZE_IS_8}		{ unroll the inner loop }
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
      Inc(workspaceptr);
      {Inc(elemptr);       - Value never used }
{$else}
      for elemc := pred(DCTSIZE) downto 0 do
      begin
        workspaceptr^ := GETJSAMPLE(elemptr^) - CENTERJSAMPLE;
        Inc(workspaceptr);
        Inc(elemptr);
      end;
{$endif}
    end;

    { Perform the DCT }
    do_dct (workspace);

    { Quantize/descale the coefficients, and store into coef_blocks[] }

    output_ptr := JCOEFPTR(@coef_blocks^[bi]);
    for i := 0 to pred(DCTSIZE2) do
    begin
      qval := divisors^[i];
      temp := workspace[i];
      { Divide the coefficient value by qval, ensuring proper rounding.
	Since C does not specify the direction of rounding for negative
	quotients, we have to force the dividend positive for portability.

	In most files, at least half of the output values will be zero
	(at default quantization settings, more like three-quarters...)
	so we should ensure that this case is fast.  On many machines,
	a comparison is enough cheaper than a divide to make a special test
	a win.  Since both inputs will be nonnegative, we need only test
	for a < b to discover whether a/b is 0.
	If your machine's division is fast enough, define FAST_DIVIDE. }

      if (temp < 0) then
      begin
	temp := -temp;
	Inc(temp, qval shr 1);	{ for rounding }
        {DIVIDE_BY(temp, qval);}
        {$ifdef FAST_DIVIDE}
          temp := temp div qval;
        {$else}
          if (temp >= qval) then
            temp := temp div qval
          else
            temp := 0;
        {$endif}
	temp := -temp;
      end
      else
      begin
	Inc(temp, qval shr 1);	{ for rounding }
        {DIVIDE_BY(temp, qval);}
        {$ifdef FAST_DIVIDE}
          temp := temp div qval;
        {$else}
          if (temp >= qval) then
            temp := temp div qval
          else
            temp := 0;
        {$endif}
      end;
      output_ptr^[i] := JCOEF (temp);
    end;
    Inc(start_col, DCTSIZE);
  end;
end;


{$ifdef DCT_FLOAT_SUPPORTED}

{METHODDEF}
procedure forward_DCT_float (cinfo : j_compress_ptr;
                             compptr : jpeg_component_info_ptr;
		             sample_data : JSAMPARRAY;
                             coef_blocks : JBLOCKROW;
		             start_row : JDIMENSION;
                             start_col : JDIMENSION;
		             num_blocks : JDIMENSION);  
{ This version is used for floating-point DCT implementations. }
var
  { This routine is heavily used, so it's worth coding it tightly. }
  fdct : my_fdct_ptr;
  do_dct : float_DCT_method_ptr;
  divisors : FAST_FLOAT_FIELD_PTR;
  workspace : array[0..DCTSIZE2-1] of FAST_FLOAT; { work area for FDCT subroutine }
  bi : JDIMENSION;
var
  {register} workspaceptr : FAST_FLOAT_PTR;
  {register} elemptr : JSAMPLE_PTR;
  {register} elemr : int;
{$ifndef DCTSIZE_IS_8}
var
  {register} elemc : int;
{$endif}
var
  {register} temp : FAST_FLOAT;
  {register} i : int;
  {register} output_ptr : JCOEFPTR;
begin
  fdct := my_fdct_ptr (cinfo^.fdct);
  do_dct := fdct^.do_float_dct;
  divisors := fdct^.float_divisors[compptr^.quant_tbl_no];

  Inc(JSAMPROW_PTR(sample_data), start_row);	{ fold in the vertical offset once }

  for bi := 0 to pred(num_blocks) do
  begin
    { Load data into workspace, applying unsigned->signed conversion }

    workspaceptr := @workspace[0];
    for elemr := 0 to pred(DCTSIZE) do
    begin
      elemptr := @(sample_data^[elemr]^[start_col]);
{$ifdef DCTSIZE_IS_8}		{ unroll the inner loop }
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      Inc(elemptr);
      workspaceptr^ := {FAST_FLOAT}(GETJSAMPLE(elemptr^) - CENTERJSAMPLE);
      Inc(workspaceptr);
      {Inc(elemptr);         - value never used }
{$else}
      for elemc := pred(DCTSIZE) downto 0 do
      begin
	workspaceptr^ := {FAST_FLOAT}(
	  (GETJSAMPLE(elemptr^) - CENTERJSAMPLE) );
        Inc(workspaceptr);
        Inc(elemptr);
      end;
{$endif}
    end;


    { Perform the DCT }
    do_dct (workspace);

    { Quantize/descale the coefficients, and store into coef_blocks[] }

    output_ptr := JCOEFPTR(@(coef_blocks^[bi]));

    for i := 0 to pred(DCTSIZE2) do
    begin
      { Apply the quantization and scaling factor }
      temp := workspace[i] * divisors^[i];
      { Round to nearest integer.
	Since C does not specify the direction of rounding for negative
	quotients, we have to force the dividend positive for portability.
	The maximum coefficient size is +-16K (for 12-bit data), so this
	code should work for either 16-bit or 32-bit ints. }
      output_ptr^[i] := JCOEF ( int(Trunc (temp + {FAST_FLOAT}(16384.5))) - 16384);
    end;
    Inc(start_col, DCTSIZE);
  end;
end;

{$endif} { DCT_FLOAT_SUPPORTED }


{ Initialize FDCT manager. }

{GLOBAL}
procedure jinit_forward_dct (cinfo : j_compress_ptr);
var
  fdct : my_fdct_ptr;
  i : int;
begin
  fdct := my_fdct_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_fdct_controller)) );
  cinfo^.fdct := jpeg_forward_dct_ptr (fdct);
  fdct^.pub.start_pass := start_pass_fdctmgr;

  case (cinfo^.dct_method) of
{$ifdef DCT_ISLOW_SUPPORTED}
  JDCT_ISLOW:
    begin
      fdct^.pub.forward_DCT := forward_DCT;
      fdct^.do_dct := jpeg_fdct_islow;
    end;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
  JDCT_IFAST:
    begin
      fdct^.pub.forward_DCT := forward_DCT;
      fdct^.do_dct := jpeg_fdct_ifast;
    end;
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
  JDCT_FLOAT:
    begin
      fdct^.pub.forward_DCT := forward_DCT_float;
      fdct^.do_float_dct := jpeg_fdct_float;
    end;
{$endif}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
  end;

  { Mark divisor tables unallocated }
  for i := 0 to pred(NUM_QUANT_TBLS) do
  begin
    fdct^.divisors[i] := NIL;
{$ifdef DCT_FLOAT_SUPPORTED}
    fdct^.float_divisors[i] := NIL;
{$endif}
  end;
end;

end.
