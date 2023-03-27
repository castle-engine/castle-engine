Unit JdDctMgr;

{ Original : jddctmgr.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file contains the inverse-DCT management logic.
  This code selects a particular IDCT implementation to be used,
  and it performs related housekeeping chores.  No code in this file
  is executed per IDCT step, only during output pass setup.

  Note that the IDCT routines are responsible for performing coefficient
  dequantization as well as the IDCT proper.  This module sets up the
  dequantization multiplier table needed by the IDCT routine. }

interface

{$I jconfig.inc}

{$N+}

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
  jpeglib,
  jdct,         { Private declarations for DCT subsystem }
  jidctfst,
  {$IFDEF BASM}
  jidctasm,
  {$ELSE}
  jidctint,
  {$ENDIF}
  jidctflt, JIDctRed;



{ Initialize IDCT manager. }

{GLOBAL}
procedure jinit_inverse_dct (cinfo : j_decompress_ptr);


implementation

{ The decompressor input side (jdinput.c) saves away the appropriate
  quantization table for each component at the start of the first scan
  involving that component.  (This is necessary in order to correctly
  decode files that reuse Q-table slots.)
  When we are ready to make an output pass, the saved Q-table is converted
  to a multiplier table that will actually be used by the IDCT routine.
  The multiplier table contents are IDCT-method-dependent.  To support
  application changes in IDCT method between scans, we can remake the
  multiplier tables if necessary.
  In buffered-image mode, the first output pass may occur before any data
  has been seen for some components, and thus before their Q-tables have
  been saved away.  To handle this case, multiplier tables are preset
  to zeroes; the result of the IDCT will be a neutral gray level. }


{ Private subobject for this module }

type
  my_idct_ptr = ^my_idct_controller;
  my_idct_controller = record
    pub : jpeg_inverse_dct;     { public fields }

    { This array contains the IDCT method code that each multiplier table
      is currently set up for, or -1 if it's not yet set up.
      The actual multiplier tables are pointed to by dct_table in the
      per-component comp_info structures. }

    cur_method : array[0..MAX_COMPONENTS-1] of int;
  end; {my_idct_controller;}


{ Allocated multiplier tables: big enough for any supported variant }

type
  multiplier_table = record
  case byte of
    0:(islow_array : array[0..DCTSIZE2-1] of ISLOW_MULT_TYPE);
  {$ifdef DCT_IFAST_SUPPORTED}
    1:(ifast_array : array[0..DCTSIZE2-1] of IFAST_MULT_TYPE);
  {$endif}
  {$ifdef DCT_FLOAT_SUPPORTED}
    2:(float_array : array[0..DCTSIZE2-1] of FLOAT_MULT_TYPE);
  {$endif}
  end;


{ The current scaled-IDCT routines require ISLOW-style multiplier tables,
  so be sure to compile that code if either ISLOW or SCALING is requested. }

{$ifdef DCT_ISLOW_SUPPORTED}
  {$define PROVIDE_ISLOW_TABLES}
{$else}
  {$ifdef IDCT_SCALING_SUPPORTED}
    {$define PROVIDE_ISLOW_TABLES}
  {$endif}
{$endif}


{ Prepare for an output pass.
  Here we select the proper IDCT routine for each component and build
  a matching multiplier table. }

{METHODDEF}
procedure start_pass (cinfo : j_decompress_ptr); far;
var
  idct : my_idct_ptr;
  ci, i : int;
  compptr : jpeg_component_info_ptr;
  method : J_DCT_METHOD;
  method_ptr : inverse_DCT_method_ptr;
  qtbl : JQUANT_TBL_PTR;
{$ifdef PROVIDE_ISLOW_TABLES}
var
  ismtbl : ISLOW_MULT_TYPE_FIELD_PTR;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
const
  CONST_BITS = 14;
const
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
var
  ifmtbl : IFAST_MULT_TYPE_FIELD_PTR;
  {SHIFT_TEMPS}

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

{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
const
  aanscalefactor : array[0..DCTSIZE-1] of double =
  (1.0, 1.387039845, 1.306562965, 1.175875602,
    1.0, 0.785694958, 0.541196100, 0.275899379);
var
  fmtbl : FLOAT_MULT_TYPE_FIELD_PTR;
  row, col : int;
{$endif}
begin
  idct := my_idct_ptr (cinfo^.idct);
  method := J_DCT_METHOD(0);
  method_ptr := NIL;
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);

  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Select the proper IDCT routine for this component's scaling }
    case (compptr^.DCT_scaled_size) of
{$ifdef IDCT_SCALING_SUPPORTED}
    1:begin
        method_ptr := jpeg_idct_1x1;
        method := JDCT_ISLOW;   { jidctred uses islow-style table }
      end;
    2:begin
        method_ptr := jpeg_idct_2x2;
        method := JDCT_ISLOW;   { jidctred uses islow-style table }
      end;
    4:begin
        method_ptr := jpeg_idct_4x4;
        method := JDCT_ISLOW;   { jidctred uses islow-style table }
      end;
{$endif}
    DCTSIZE:
      case (cinfo^.dct_method) of
{$ifdef DCT_ISLOW_SUPPORTED}
      JDCT_ISLOW:
        begin
          method_ptr := jpeg_idct_islow;
          method := JDCT_ISLOW;
        end;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
      JDCT_IFAST:
        begin
          method_ptr := jpeg_idct_ifast;
          method := JDCT_IFAST;
        end;
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
      JDCT_FLOAT:
        begin
          method_ptr := jpeg_idct_float;
          method := JDCT_FLOAT;
        end;
{$endif}
      else
        ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
      end;
    else
      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_DCTSIZE, compptr^.DCT_scaled_size);
    end;
    idct^.pub.inverse_DCT[ci] := method_ptr;
    { Create multiplier table from quant table.
      However, we can skip this if the component is uninteresting
      or if we already built the table.  Also, if no quant table
      has yet been saved for the component, we leave the
      multiplier table all-zero; we'll be reading zeroes from the
      coefficient controller's buffer anyway. }

    if (not compptr^.component_needed) or (idct^.cur_method[ci] = int(method)) then
      continue;
    qtbl := compptr^.quant_table;
    if (qtbl = NIL) then        { happens if no data yet for component }
      continue;
    idct^.cur_method[ci] := int(method);
    case (method) of
{$ifdef PROVIDE_ISLOW_TABLES}
    JDCT_ISLOW:
      begin
        { For LL&M IDCT method, multipliers are equal to raw quantization
          coefficients, but are stored as ints to ensure access efficiency. }

        ismtbl := ISLOW_MULT_TYPE_FIELD_PTR (compptr^.dct_table);
        for i := 0 to pred(DCTSIZE2) do
        begin
          ismtbl^[i] := ISLOW_MULT_TYPE (qtbl^.quantval[i]);
        end;
      end;
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
    JDCT_IFAST:
      begin
        { For AA&N IDCT method, multipliers are equal to quantization
          coefficients scaled by scalefactor[row]*scalefactor[col], where
            scalefactor[0] := 1
            scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7
          For integer operation, the multiplier table is to be scaled by
          IFAST_SCALE_BITS. }

        ifmtbl := IFAST_MULT_TYPE_FIELD_PTR (compptr^.dct_table);

        for i := 0 to pred(DCTSIZE2) do
        begin
          ifmtbl^[i] := IFAST_MULT_TYPE(
            DESCALE(  INT32 (qtbl^.quantval[i]) * INT32 (aanscales[i]),
                    CONST_BITS-IFAST_SCALE_BITS) );
        end;
      end;
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
    JDCT_FLOAT:
      begin
        { For float AA&N IDCT method, multipliers are equal to quantization
          coefficients scaled by scalefactor[row]*scalefactor[col], where
            scalefactor[0] := 1
            scalefactor[k] := cos(k*PI/16) * sqrt(2)    for k=1..7 }

        fmtbl := FLOAT_MULT_TYPE_FIELD_PTR(compptr^.dct_table);

        i := 0;
        for row := 0 to pred(DCTSIZE) do
        begin
          for col := 0 to pred(DCTSIZE) do
          begin
            fmtbl^[i] := {FLOAT_MULT_TYPE} (
               {double} qtbl^.quantval[i] *
               aanscalefactor[row] * aanscalefactor[col] );
            Inc(i);
          end;
        end;
      end;
{$endif}
    else
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
      break;
    end;
    Inc(compptr);
  end;
end;


{ Initialize IDCT manager. }

{GLOBAL}
procedure jinit_inverse_dct (cinfo : j_decompress_ptr);
var
  idct : my_idct_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  idct := my_idct_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(my_idct_controller)) );
  cinfo^.idct := jpeg_inverse_dct_ptr (idct);
  idct^.pub.start_pass := start_pass;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Allocate and pre-zero a multiplier table for each component }
    compptr^.dct_table :=
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(multiplier_table));
    MEMZERO(compptr^.dct_table, SIZEOF(multiplier_table));
    { Mark multiplier table not yet set up for any method }
    idct^.cur_method[ci] := -1;
    Inc(compptr);
  end;
end;

end.
