Unit JdMerge;

{  This file contains code for merged upsampling/color conversion.

  This file combines functions from jdsample.c and jdcolor.c;
  read those files first to understand what's going on.

  When the chroma components are to be upsampled by simple replication
  (ie, box filtering), we can save some work in color conversion by
  calculating all the output pixels corresponding to a pair of chroma
  samples at one time.  In the conversion equations
        R := Y           + K1 * Cr
        G := Y + K2 * Cb + K3 * Cr
        B := Y + K4 * Cb
  only the Y term varies among the group of pixels corresponding to a pair
  of chroma samples, so the rest of the terms can be calculated just once.
  At typical sampling ratios, this eliminates half or three-quarters of the
  multiplications needed for color conversion.

  This file currently provides implementations for the following cases:
        YCbCr => RGB color conversion only.
        Sampling ratios of 2h1v or 2h2v.
        No scaling needed at upsample time.
        Corner-aligned (non-CCIR601) sampling alignment.
  Other special cases could be added, but in most applications these are
  the only common cases.  (For uncommon cases we fall back on the more
  general code in jdsample.c and jdcolor.c.) }

{ Original: jdmerge.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jpeglib,
  jutils;

{ Module initialization routine for merged upsampling/color conversion.

  NB: this is called under the conditions determined by use_merged_upsample()
  in jdmaster.c.  That routine MUST correspond to the actual capabilities
  of this module; no safety checks are made here. }

{GLOBAL}
procedure jinit_merged_upsampler (cinfo : j_decompress_ptr);

implementation


{ Private subobject }

type  { the same definition as in JdColor }
  int_Color_Table = array[0..MAXJSAMPLE+1-1] of int;
  int_CConvertPtr = ^int_Color_Table;
  INT32_Color_Table = array[0..MAXJSAMPLE+1-1] of INT32;
  INT32_CConvertPtr = ^INT32_Color_Table;

type
  my_upsample_ptr = ^my_upsampler;
    my_upsampler = record
    pub : jpeg_upsampler;       { public fields }

    { Pointer to routine to do actual upsampling/conversion of one row group }
    upmethod : procedure (cinfo : j_decompress_ptr;
                          input_buf : JSAMPIMAGE;
                          in_row_group_ctr : JDIMENSION;
                          output_buf : JSAMPARRAY);

    { Private state for YCC->RGB conversion }
    Cr_r_tab : int_CConvertPtr;         { => table for Cr to R conversion }
    Cb_b_tab : int_CConvertPtr;         { => table for Cb to B conversion }
    Cr_g_tab : INT32_CConvertPtr;       { => table for Cr to G conversion }
    Cb_g_tab : INT32_CConvertPtr;       { => table for Cb to G conversion }

    { For 2:1 vertical sampling, we produce two output rows at a time.
      We need a "spare" row buffer to hold the second output row if the
      application provides just a one-row buffer; we also use the spare
      to discard the dummy last row if the image height is odd. }

    spare_row : JSAMPROW;
    spare_full : boolean;               { TRUE if spare buffer is occupied }

    out_row_width : JDIMENSION; { samples per output row }
    rows_to_go : JDIMENSION;    { counts rows remaining in image }
  end; {my_upsampler;}


const
  SCALEBITS = 16;       { speediest right-shift on some machines }
  ONE_HALF  = (INT32(1) shl (SCALEBITS-1));


{ Initialize tables for YCC->RGB colorspace conversion.
  This is taken directly from jdcolor.c; see that file for more info. }

{LOCAL}
procedure build_ycc_rgb_table (cinfo : j_decompress_ptr);
const
  FIX_1_40200 = INT32( Round(1.40200 * (INT32(1) shl SCALEBITS)) );
  FIX_1_77200 = INT32( Round(1.77200 * (INT32(1) shl SCALEBITS)) );
  FIX_0_71414 = INT32( Round(0.71414 * (INT32(1) shl SCALEBITS)) );
  FIX_0_34414 = INT32( Round(0.34414 * (INT32(1) shl SCALEBITS)) );
var
  upsample : my_upsample_ptr;
  i : int;
  x : INT32;
var
  shift_temp : INT32;
begin
  upsample := my_upsample_ptr (cinfo^.upsample);

  upsample^.Cr_r_tab := int_CConvertPtr (
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(int)) );
  upsample^.Cb_b_tab := int_CConvertPtr (
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(int)) );
  upsample^.Cr_g_tab := INT32_CConvertPtr (
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(INT32)) );
  upsample^.Cb_g_tab := INT32_CConvertPtr (
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(INT32)) );

  x := -CENTERJSAMPLE;
  for i := 0 to pred(MAXJSAMPLE) do
  begin
    { i is the actual input pixel value, in the range 0..MAXJSAMPLE }
    { The Cb or Cr value we are thinking of is x := i - CENTERJSAMPLE }
    { Cr=>R value is nearest int to 1.40200 * x }
    {upsample^.Cr_r_tab^[i] := int(
                    RIGHT_SHIFT(FIX_1_40200 * x + ONE_HALF, SCALEBITS) );}
    shift_temp := FIX_1_40200  * x + ONE_HALF;
    if shift_temp < 0 then  { SHIFT arithmetic RIGHT }
      upsample^.Cr_r_tab^[i] := int((shift_temp shr SCALEBITS)
                             or ( (not INT32(0)) shl (32-SCALEBITS)))
    else
      upsample^.Cr_r_tab^[i] := int(shift_temp shr SCALEBITS);


    { Cb=>B value is nearest int to 1.77200 * x }
    {upsample^.Cb_b_tab^[i] := int(
                    RIGHT_SHIFT(FIX_1_77200 * x + ONE_HALF, SCALEBITS) );}
    shift_temp := FIX_1_77200 * x + ONE_HALF;
    if shift_temp < 0 then  { SHIFT arithmetic RIGHT }
      upsample^.Cb_b_tab^[i] := int((shift_temp shr SCALEBITS)
                             or ( (not INT32(0)) shl (32-SCALEBITS)))
    else
      upsample^.Cb_b_tab^[i] := int(shift_temp shr SCALEBITS);

    { Cr=>G value is scaled-up -0.71414 * x }
    upsample^.Cr_g_tab^[i] := (- FIX_0_71414) * x;
    { Cb=>G value is scaled-up -0.34414 * x }
    { We also add in ONE_HALF so that need not do it in inner loop }
    upsample^.Cb_g_tab^[i] := (- FIX_0_34414) * x + ONE_HALF;
    Inc(x);
  end;
end;


{ Initialize for an upsampling pass. }

{METHODDEF}
procedure start_pass_merged_upsample (cinfo : j_decompress_ptr); far;
var
  upsample : my_upsample_ptr;
begin
  upsample := my_upsample_ptr (cinfo^.upsample);

  { Mark the spare buffer empty }
  upsample^.spare_full := FALSE;
  { Initialize total-height counter for detecting bottom of image }
  upsample^.rows_to_go := cinfo^.output_height;
end;


{ Control routine to do upsampling (and color conversion).

  The control routine just handles the row buffering considerations. }

{METHODDEF}
procedure merged_2v_upsample (cinfo : j_decompress_ptr;
                              input_buf : JSAMPIMAGE;
                              var in_row_group_ctr : JDIMENSION;
                              in_row_groups_avail : JDIMENSION;
                              output_buf : JSAMPARRAY;
                              var out_row_ctr : JDIMENSION;
                              out_rows_avail : JDIMENSION); far;
{ 2:1 vertical sampling case: may need a spare row. }
var
  upsample : my_upsample_ptr;
  work_ptrs : array[0..2-1] of JSAMPROW;
  num_rows : JDIMENSION;                { number of rows returned to caller }
begin
  upsample := my_upsample_ptr (cinfo^.upsample);

  if (upsample^.spare_full) then
  begin
    { If we have a spare row saved from a previous cycle, just return it. }
    jcopy_sample_rows(JSAMPARRAY(@upsample^.spare_row),
                      0,
                      JSAMPARRAY(@ output_buf^[out_row_ctr]),
                      0, 1, upsample^.out_row_width);
    num_rows := 1;
    upsample^.spare_full := FALSE;
  end
  else
  begin
    { Figure number of rows to return to caller. }
    num_rows := 2;
    { Not more than the distance to the end of the image. }
    if (num_rows > upsample^.rows_to_go) then
      num_rows := upsample^.rows_to_go;
    { And not more than what the client can accept: }
    Dec(out_rows_avail, {var} out_row_ctr);
    if (num_rows > out_rows_avail) then
      num_rows := out_rows_avail;
    { Create output pointer array for upsampler. }
    work_ptrs[0] := output_buf^[out_row_ctr];
    if (num_rows > 1) then
    begin
      work_ptrs[1] := output_buf^[out_row_ctr + 1];
    end
    else
    begin
      work_ptrs[1] := upsample^.spare_row;
      upsample^.spare_full := TRUE;
    end;
    { Now do the upsampling. }
    upsample^.upmethod (cinfo, input_buf, {var}in_row_group_ctr,
                        JSAMPARRAY(@work_ptrs));
  end;

  { Adjust counts }
  Inc(out_row_ctr, num_rows);
  Dec(upsample^.rows_to_go, num_rows);
  { When the buffer is emptied, declare this input row group consumed }
  if (not upsample^.spare_full) then
    Inc(in_row_group_ctr);
end;


{METHODDEF}
procedure merged_1v_upsample (cinfo : j_decompress_ptr;
                             input_buf : JSAMPIMAGE;
                             var in_row_group_ctr : JDIMENSION;
                             in_row_groups_avail : JDIMENSION;
                             output_buf : JSAMPARRAY;
                             var out_row_ctr : JDIMENSION;
                             out_rows_avail : JDIMENSION); far;
{ 1:1 vertical sampling case: much easier, never need a spare row. }
var
  upsample : my_upsample_ptr;
begin
  upsample := my_upsample_ptr (cinfo^.upsample);

  { Just do the upsampling. }
  upsample^.upmethod (cinfo, input_buf, in_row_group_ctr,
                         JSAMPARRAY(@ output_buf^[out_row_ctr]));
  { Adjust counts }
  Inc(out_row_ctr);
  Inc(in_row_group_ctr);
end;


{ These are the routines invoked by the control routines to do
  the actual upsampling/conversion.  One row group is processed per call.

  Note: since we may be writing directly into application-supplied buffers,
  we have to be honest about the output width; we can't assume the buffer
  has been rounded up to an even width. }


{ Upsample and color convert for the case of 2:1 horizontal and 1:1 vertical. }

{METHODDEF}
procedure h2v1_merged_upsample (cinfo : j_decompress_ptr;
                                input_buf : JSAMPIMAGE;
                                in_row_group_ctr : JDIMENSION;
                                output_buf : JSAMPARRAY); far;
var
  upsample : my_upsample_ptr;
  {register} y, cred, cgreen, cblue : int;
  cb, cr : int;
  {register}  outptr : JSAMPROW;
  inptr0, inptr1, inptr2 : JSAMPLE_PTR;
  col : JDIMENSION;
  { copy these pointers into registers if possible }
  {register} range_limit : range_limit_table_ptr;
  Crrtab : int_CConvertPtr;
  Cbbtab : int_CConvertPtr;
  Crgtab : INT32_CConvertPtr;
  Cbgtab : INT32_CConvertPtr;
var
  shift_temp : INT32;
begin
  upsample := my_upsample_ptr (cinfo^.upsample);
  range_limit := cinfo^.sample_range_limit;
  Crrtab := upsample^.Cr_r_tab;
  Cbbtab := upsample^.Cb_b_tab;
  Crgtab := upsample^.Cr_g_tab;
  Cbgtab := upsample^.Cb_g_tab;

  inptr0 := JSAMPLE_PTR(input_buf^[0]^[in_row_group_ctr]);
  inptr1 := JSAMPLE_PTR(input_buf^[1]^[in_row_group_ctr]);
  inptr2 := JSAMPLE_PTR(input_buf^[2]^[in_row_group_ctr]);
  outptr := output_buf^[0];
  { Loop for each pair of output pixels }
  for col := pred(cinfo^.output_width shr 1) downto 0 do
  begin
    { Do the chroma part of the calculation }
    cb := GETJSAMPLE(inptr1^);
    Inc(inptr1);
    cr := GETJSAMPLE(inptr2^);
    Inc(inptr2);
    cred := Crrtab^[cr];
    {cgreen := int( RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr], SCALEBITS) );}
    shift_temp := Cbgtab^[cb] + Crgtab^[cr];
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cgreen := int((shift_temp shr SCALEBITS)
                            or ( (not INT32(0)) shl (32-SCALEBITS)))
    else
      cgreen := int(shift_temp shr SCALEBITS);

    cblue := Cbbtab^[cb];
    { Fetch 2 Y values and emit 2 pixels }
    y  := GETJSAMPLE(inptr0^);
    Inc(inptr0);
    outptr^[RGB_RED] :=   range_limit^[y + cred];
    outptr^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr), RGB_PIXELSIZE);
    y  := GETJSAMPLE(inptr0^);
    Inc(inptr0);
    outptr^[RGB_RED] :=   range_limit^[y + cred];
    outptr^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr), RGB_PIXELSIZE);
  end;
  { If image width is odd, do the last output column separately }
  if Odd(cinfo^.output_width) then
  begin
    cb := GETJSAMPLE(inptr1^);
    cr := GETJSAMPLE(inptr2^);
    cred := Crrtab^[cr];
    {cgreen := int ( RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr], SCALEBITS) );}
    shift_temp := Cbgtab^[cb] + Crgtab^[cr];
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cgreen := int((shift_temp shr SCALEBITS)
                            or ( (not INT32(0)) shl (32-SCALEBITS)))
    else
      cgreen := int(shift_temp shr SCALEBITS);

    cblue := Cbbtab^[cb];
    y  := GETJSAMPLE(inptr0^);
    outptr^[RGB_RED] :=   range_limit^[y + cred];
    outptr^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr^[RGB_BLUE] :=  range_limit^[y + cblue];
  end;
end;


{ Upsample and color convert for the case of 2:1 horizontal and 2:1 vertical. }

{METHODDEF}
procedure h2v2_merged_upsample (cinfo : j_decompress_ptr;
                                input_buf : JSAMPIMAGE;
                                in_row_group_ctr : JDIMENSION;
                                output_buf : JSAMPARRAY); far;
var
  upsample : my_upsample_ptr;
  {register} y, cred, cgreen, cblue : int;
  cb, cr : int;
  {register} outptr0, outptr1 : JSAMPROW;
  inptr00, inptr01, inptr1, inptr2 : JSAMPLE_PTR;
  col : JDIMENSION;
  { copy these pointers into registers if possible }
  {register} range_limit : range_limit_table_ptr;
  Crrtab : int_CConvertPtr;
  Cbbtab : int_CConvertPtr;
  Crgtab : INT32_CConvertPtr;
  Cbgtab : INT32_CConvertPtr;
var
  shift_temp : INT32;
begin
  upsample := my_upsample_ptr (cinfo^.upsample);
  range_limit := cinfo^.sample_range_limit;
  Crrtab := upsample^.Cr_r_tab;
  Cbbtab := upsample^.Cb_b_tab;
  Crgtab := upsample^.Cr_g_tab;
  Cbgtab := upsample^.Cb_g_tab;

  inptr00 := JSAMPLE_PTR(input_buf^[0]^[in_row_group_ctr*2]);
  inptr01 := JSAMPLE_PTR(input_buf^[0]^[in_row_group_ctr*2 + 1]);
  inptr1 := JSAMPLE_PTR(input_buf^[1]^[in_row_group_ctr]);
  inptr2 := JSAMPLE_PTR(input_buf^[2]^[in_row_group_ctr]);
  outptr0 := output_buf^[0];
  outptr1 := output_buf^[1];
  { Loop for each group of output pixels }
  for col := pred(cinfo^.output_width shr 1) downto 0 do
  begin
    { Do the chroma part of the calculation }
    cb := GETJSAMPLE(inptr1^);
    Inc(inptr1);
    cr := GETJSAMPLE(inptr2^);
    Inc(inptr2);
    cred := Crrtab^[cr];
    {cgreen := int( RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr], SCALEBITS) );}
    shift_temp := Cbgtab^[cb] + Crgtab^[cr];
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cgreen := int((shift_temp shr SCALEBITS)
                            or ( (not INT32(0)) shl (32-SCALEBITS)))
    else
      cgreen := int(shift_temp shr SCALEBITS);

    cblue := Cbbtab^[cb];
    { Fetch 4 Y values and emit 4 pixels }
    y  := GETJSAMPLE(inptr00^);
    Inc(inptr00);
    outptr0^[RGB_RED] :=   range_limit^[y + cred];
    outptr0^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr0^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr0), RGB_PIXELSIZE);
    y  := GETJSAMPLE(inptr00^);
    Inc(inptr00);
    outptr0^[RGB_RED] :=   range_limit^[y + cred];
    outptr0^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr0^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr0), RGB_PIXELSIZE);
    y  := GETJSAMPLE(inptr01^);
    Inc(inptr01);
    outptr1^[RGB_RED] :=   range_limit^[y + cred];
    outptr1^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr1^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr1), RGB_PIXELSIZE);
    y  := GETJSAMPLE(inptr01^);
    Inc(inptr01);
    outptr1^[RGB_RED] :=   range_limit^[y + cred];
    outptr1^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr1^[RGB_BLUE] :=  range_limit^[y + cblue];
    Inc(JSAMPLE_PTR(outptr1), RGB_PIXELSIZE);
  end;
  { If image width is odd, do the last output column separately }
  if Odd(cinfo^.output_width) then
  begin
    cb := GETJSAMPLE(inptr1^);
    cr := GETJSAMPLE(inptr2^);
    cred := Crrtab^[cr];
    {cgreen := int (RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr], SCALEBITS));}
    shift_temp := Cbgtab^[cb] + Crgtab^[cr];
    if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
      cgreen := int((shift_temp shr SCALEBITS)
                            or ( (not INT32(0)) shl (32-SCALEBITS)))
    else
      cgreen := int(shift_temp shr SCALEBITS);

    cblue := Cbbtab^[cb];
    y  := GETJSAMPLE(inptr00^);
    outptr0^[RGB_RED] :=   range_limit^[y + cred];
    outptr0^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr0^[RGB_BLUE] :=  range_limit^[y + cblue];
    y  := GETJSAMPLE(inptr01^);
    outptr1^[RGB_RED] :=   range_limit^[y + cred];
    outptr1^[RGB_GREEN] := range_limit^[y + cgreen];
    outptr1^[RGB_BLUE] :=  range_limit^[y + cblue];
  end;
end;


{ Module initialization routine for merged upsampling/color conversion.

  NB: this is called under the conditions determined by use_merged_upsample()
  in jdmaster.c.  That routine MUST correspond to the actual capabilities
  of this module; no safety checks are made here. }


{GLOBAL}
procedure jinit_merged_upsampler (cinfo : j_decompress_ptr);
var
  upsample : my_upsample_ptr;
begin
  upsample := my_upsample_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(my_upsampler)) );
  cinfo^.upsample := jpeg_upsampler_ptr (upsample);
  upsample^.pub.start_pass := start_pass_merged_upsample;
  upsample^.pub.need_context_rows := FALSE;

  upsample^.out_row_width := cinfo^.output_width * cinfo^.out_color_components;

  if (cinfo^.max_v_samp_factor = 2) then
  begin
    upsample^.pub.upsample := merged_2v_upsample;
    upsample^.upmethod := h2v2_merged_upsample;
    { Allocate a spare row buffer }
    upsample^.spare_row := JSAMPROW(
      cinfo^.mem^.alloc_large ( j_common_ptr(cinfo), JPOOL_IMAGE,
                size_t (upsample^.out_row_width * SIZEOF(JSAMPLE))) );
  end
  else
  begin
    upsample^.pub.upsample := merged_1v_upsample;
    upsample^.upmethod := h2v1_merged_upsample;
    { No spare row needed }
    upsample^.spare_row := NIL;
  end;

  build_ycc_rgb_table(cinfo);
end;

end.
