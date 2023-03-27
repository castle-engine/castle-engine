Unit JdSample;

{ Original: jdsample.c; Copyright (C) 1991-1996, Thomas G. Lane. }

{ This file contains upsampling routines.

  Upsampling input data is counted in "row groups".  A row group
  is defined to be (v_samp_factor * DCT_scaled_size / min_DCT_scaled_size)
  sample rows of each component.  Upsampling will normally produce
  max_v_samp_factor pixel rows from each row group (but this could vary
  if the upsampler is applying a scale factor of its own).

  An excellent reference for image resampling is
    Digital Image Warping, George Wolberg, 1990.
    Pub. by IEEE Computer Society Press, Los Alamitos, CA. ISBN 0-8186-8944-7.}

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jutils,
  jpeglib,
  jdeferr,
  jerror;


{ Pointer to routine to upsample a single component }
type
  upsample1_ptr = procedure (cinfo : j_decompress_ptr;
                             compptr : jpeg_component_info_ptr;
                             input_data : JSAMPARRAY;
                             var output_data_ptr : JSAMPARRAY);

{ Module initialization routine for upsampling. }

{GLOBAL}
procedure jinit_upsampler (cinfo : j_decompress_ptr);

implementation

{ Private subobject }

type
  my_upsample_ptr = ^my_upsampler;
  my_upsampler = record
    pub : jpeg_upsampler;       { public fields }

    { Color conversion buffer.  When using separate upsampling and color
      conversion steps, this buffer holds one upsampled row group until it
      has been color converted and output.
      Note: we do not allocate any storage for component(s) which are full-size,
      ie do not need rescaling.  The corresponding entry of color_buf[] is
      simply set to point to the input data array, thereby avoiding copying.}

    color_buf : array[0..MAX_COMPONENTS-1] of JSAMPARRAY;

    { Per-component upsampling method pointers }
    methods : array[0..MAX_COMPONENTS-1] of upsample1_ptr;

    next_row_out : int;         { counts rows emitted from color_buf }
    rows_to_go : JDIMENSION;    { counts rows remaining in image }

    { Height of an input row group for each component. }
    rowgroup_height : array[0..MAX_COMPONENTS-1] of int;

    { These arrays save pixel expansion factors so that int_expand need not
      recompute them each time.  They are unused for other upsampling methods.}
    h_expand : array[0..MAX_COMPONENTS-1] of UINT8 ;
    v_expand : array[0..MAX_COMPONENTS-1] of UINT8 ;
  end;


{ Initialize for an upsampling pass. }

{METHODDEF}
procedure start_pass_upsample (cinfo : j_decompress_ptr); far;
var
  upsample : my_upsample_ptr;
begin
  upsample := my_upsample_ptr (cinfo^.upsample);

  { Mark the conversion buffer empty }
  upsample^.next_row_out := cinfo^.max_v_samp_factor;
  { Initialize total-height counter for detecting bottom of image }
  upsample^.rows_to_go := cinfo^.output_height;
end;


{ Control routine to do upsampling (and color conversion).

  In this version we upsample each component independently.
  We upsample one row group into the conversion buffer, then apply
  color conversion a row at a time. }

{METHODDEF}
procedure sep_upsample (cinfo : j_decompress_ptr;
                        input_buf : JSAMPIMAGE;
                        var in_row_group_ctr : JDIMENSION;
                        in_row_groups_avail : JDIMENSION;
                        output_buf : JSAMPARRAY;
                        var out_row_ctr : JDIMENSION;
                        out_rows_avail : JDIMENSION); far;
var
  upsample : my_upsample_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  num_rows : JDIMENSION;
begin
  upsample := my_upsample_ptr (cinfo^.upsample);

  { Fill the conversion buffer, if it's empty }
  if (upsample^.next_row_out >= cinfo^.max_v_samp_factor) then
  begin
    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      { Invoke per-component upsample method.  Notice we pass a POINTER
        to color_buf[ci], so that fullsize_upsample can change it. }

      upsample^.methods[ci] (cinfo, compptr,
        JSAMPARRAY(@ input_buf^[ci]^
           [in_row_group_ctr * upsample^.rowgroup_height[ci]]),
        upsample^.color_buf[ci]);

      Inc(compptr);
    end;
    upsample^.next_row_out := 0;
  end;

  { Color-convert and emit rows }

  { How many we have in the buffer: }
  num_rows := JDIMENSION (cinfo^.max_v_samp_factor - upsample^.next_row_out);
  { Not more than the distance to the end of the image.  Need this test
    in case the image height is not a multiple of max_v_samp_factor: }

  if (num_rows > upsample^.rows_to_go) then
    num_rows := upsample^.rows_to_go;
  { And not more than what the client can accept: }
  Dec(out_rows_avail, out_row_ctr);
  if (num_rows > out_rows_avail) then
    num_rows := out_rows_avail;

  cinfo^.cconvert^.color_convert (cinfo,
                                 JSAMPIMAGE(@(upsample^.color_buf)),
                                 JDIMENSION (upsample^.next_row_out),
                                 JSAMPARRAY(@(output_buf^[out_row_ctr])),
                                 int (num_rows));

  { Adjust counts }
  Inc(out_row_ctr, num_rows);
  Dec(upsample^.rows_to_go, num_rows);
  Inc(upsample^.next_row_out, num_rows);
  { When the buffer is emptied, declare this input row group consumed }
  if (upsample^.next_row_out >= cinfo^.max_v_samp_factor) then
    Inc(in_row_group_ctr);
end;


{ These are the routines invoked by sep_upsample to upsample pixel values
  of a single component.  One row group is processed per call. }


{ For full-size components, we just make color_buf[ci] point at the
  input buffer, and thus avoid copying any data.  Note that this is
  safe only because sep_upsample doesn't declare the input row group
  "consumed" until we are done color converting and emitting it. }

{METHODDEF}
procedure fullsize_upsample (cinfo : j_decompress_ptr;
                             compptr : jpeg_component_info_ptr;
                             input_data : JSAMPARRAY;
                             var output_data_ptr : JSAMPARRAY); far;
begin
  output_data_ptr := input_data;
end;


{ This is a no-op version used for "uninteresting" components.
  These components will not be referenced by color conversion. }

{METHODDEF}
procedure noop_upsample (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
                         input_data : JSAMPARRAY;
                         var output_data_ptr : JSAMPARRAY); far;
begin
  output_data_ptr := NIL;       { safety check }
end;


{ This version handles any integral sampling ratios.
  This is not used for typical JPEG files, so it need not be fast.
  Nor, for that matter, is it particularly accurate: the algorithm is
  simple replication of the input pixel onto the corresponding output
  pixels.  The hi-falutin sampling literature refers to this as a
  "box filter".  A box filter tends to introduce visible artifacts,
  so if you are actually going to use 3:1 or 4:1 sampling ratios
  you would be well advised to improve this code. }

{METHODDEF}
procedure int_upsample (cinfo : j_decompress_ptr;
                        compptr : jpeg_component_info_ptr;
                        input_data : JSAMPARRAY;
                        var output_data_ptr : JSAMPARRAY); far;
var
  upsample : my_upsample_ptr;
  output_data : JSAMPARRAY;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} invalue : JSAMPLE;
  {register} h : int;
  {outend}
  h_expand, v_expand : int;
  inrow, outrow : int;
var
  outcount : int;  { Nomssi: avoid pointer arithmetic }
begin
  upsample := my_upsample_ptr (cinfo^.upsample);
  output_data := output_data_ptr;

  h_expand := upsample^.h_expand[compptr^.component_index];
  v_expand := upsample^.v_expand[compptr^.component_index];

  inrow := 0;
  outrow := 0;
  while (outrow < cinfo^.max_v_samp_factor) do
  begin
    { Generate one output row with proper horizontal expansion }
    inptr := JSAMPLE_PTR(input_data^[inrow]);
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    outcount := cinfo^.output_width;
    while (outcount > 0) do     { Nomssi }
    begin
      invalue := inptr^;        { don't need GETJSAMPLE() here }
      Inc(inptr);
      for h := pred(h_expand) downto 0 do
      begin
        outptr^ := invalue;
        inc(outptr);       { <-- fix: this was left out in PasJpeg 1.0 }
        Dec(outcount);        { thanks to Jannie Gerber for the report }
      end;
    end;

    { Generate any additional output rows by duplicating the first one }
    if (v_expand > 1) then
    begin
      jcopy_sample_rows(output_data, outrow, output_data, outrow+1,
                        v_expand-1, cinfo^.output_width);
    end;
    Inc(inrow);
    Inc(outrow, v_expand);
  end;
end;


{ Fast processing for the common case of 2:1 horizontal and 1:1 vertical.
  It's still a box filter. }

{METHODDEF}
procedure h2v1_upsample (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
                         input_data : JSAMPARRAY;
                         var output_data_ptr : JSAMPARRAY); far;
var
  output_data : JSAMPARRAY;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} invalue : JSAMPLE;
  {outend : JSAMPROW;}
  outcount : int;
  inrow : int;
begin
  output_data := output_data_ptr;

  for inrow := 0 to pred(cinfo^.max_v_samp_factor) do
  begin
    inptr := JSAMPLE_PTR(input_data^[inrow]);
    outptr := JSAMPLE_PTR(output_data^[inrow]);
    {outend := outptr + cinfo^.output_width;}
    outcount := cinfo^.output_width;
    while (outcount > 0) do
    begin
      invalue := inptr^;        { don't need GETJSAMPLE() here }
      Inc(inptr);
      outptr^ := invalue;
      Inc(outptr);
      outptr^ := invalue;
      Inc(outptr);
      Dec(outcount, 2);         { Nomssi: to avoid pointer arithmetic }
    end;
  end;
end;


{ Fast processing for the common case of 2:1 horizontal and 2:1 vertical.
  It's still a box filter. }

{METHODDEF}
procedure h2v2_upsample (cinfo : j_decompress_ptr;
                         compptr : jpeg_component_info_ptr;
                         input_data : JSAMPARRAY;
                         var output_data_ptr : JSAMPARRAY); far;
var
  output_data : JSAMPARRAY;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} invalue : JSAMPLE;
  {outend : JSAMPROW;}
  outcount : int;
  inrow, outrow : int;
begin
  output_data := output_data_ptr;

  inrow := 0;
  outrow := 0;
  while (outrow < cinfo^.max_v_samp_factor) do
  begin
    inptr := JSAMPLE_PTR(input_data^[inrow]);
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    {outend := outptr + cinfo^.output_width;}
    outcount := cinfo^.output_width;
    while (outcount > 0) do
    begin
      invalue := inptr^;        { don't need GETJSAMPLE() here }
      Inc(inptr);
      outptr^ := invalue;
      Inc(outptr);
      outptr^ := invalue;
      Inc(outptr);
      Dec(outcount, 2);
    end;
    jcopy_sample_rows(output_data, outrow, output_data, outrow+1,
                      1, cinfo^.output_width);
    Inc(inrow);
    Inc(outrow, 2);
  end;
end;


{ Fancy processing for the common case of 2:1 horizontal and 1:1 vertical.

  The upsampling algorithm is linear interpolation between pixel centers,
  also known as a "triangle filter".  This is a good compromise between
  speed and visual quality.  The centers of the output pixels are 1/4 and 3/4
  of the way between input pixel centers.

  A note about the "bias" calculations: when rounding fractional values to
  integer, we do not want to always round 0.5 up to the next integer.
  If we did that, we'd introduce a noticeable bias towards larger values.
  Instead, this code is arranged so that 0.5 will be rounded up or down at
  alternate pixel locations (a simple ordered dither pattern). }

{METHODDEF}
procedure h2v1_fancy_upsample (cinfo : j_decompress_ptr;
                               compptr : jpeg_component_info_ptr;
                               input_data : JSAMPARRAY;
                               var output_data_ptr : JSAMPARRAY); far;
var
  output_data : JSAMPARRAY;
  {register} pre_inptr, inptr, outptr : JSAMPLE_PTR;
  {register} invalue : int;
  {register} colctr : JDIMENSION;
  inrow : int;
begin
  output_data := output_data_ptr;

  for inrow := 0 to pred(cinfo^.max_v_samp_factor) do
  begin
    inptr := JSAMPLE_PTR(input_data^[inrow]);
    outptr := JSAMPLE_PTR(output_data^[inrow]);
    { Special case for first column }
    pre_inptr := inptr;
    invalue := GETJSAMPLE(inptr^);
    Inc(inptr);
    outptr^ := JSAMPLE (invalue);
    Inc(outptr);
    outptr^ := JSAMPLE ((invalue * 3 + GETJSAMPLE(inptr^) + 2) shr 2);
    Inc(outptr);

    for colctr := pred(compptr^.downsampled_width - 2) downto 0 do
    begin
      { General case: 3/4 * nearer pixel + 1/4 * further pixel }
      invalue := GETJSAMPLE(inptr^) * 3;
      Inc(inptr);
      outptr^ := JSAMPLE ((invalue + GETJSAMPLE(pre_inptr^) + 1) shr 2);
      Inc(pre_inptr);
      Inc(outptr);
      outptr^ := JSAMPLE ((invalue + GETJSAMPLE(inptr^) + 2) shr 2);
      Inc(outptr);
    end;

    { Special case for last column }
    invalue := GETJSAMPLE(inptr^);
    outptr^ := JSAMPLE ((invalue * 3 + GETJSAMPLE(pre_inptr^) + 1) shr 2);
    Inc(outptr);
    outptr^ := JSAMPLE (invalue);
    {Inc(outptr);                        - value never used }
  end;
end;


{ Fancy processing for the common case of 2:1 horizontal and 2:1 vertical.
  Again a triangle filter; see comments for h2v1 case, above.

  It is OK for us to reference the adjacent input rows because we demanded
  context from the main buffer controller (see initialization code). }

{METHODDEF}
procedure h2v2_fancy_upsample (cinfo : j_decompress_ptr;
                               compptr : jpeg_component_info_ptr;
                               input_data : JSAMPARRAY;
                               var output_data_ptr : JSAMPARRAY); far;
var
  output_data : JSAMPARRAY;
  {register} inptr0, inptr1, outptr : JSAMPLE_PTR;
{$ifdef BITS_IN_JSAMPLE_IS_8}
  {register} thiscolsum, lastcolsum, nextcolsum : int;
{$else}
  {register} thiscolsum, lastcolsum, nextcolsum : INT32;
{$endif}
  {register} colctr : JDIMENSION;
  inrow, outrow, v : int;
var
  prev_input_data : JSAMPARRAY;  { Nomssi work around }
begin
  output_data := output_data_ptr;

  outrow := 0;
  inrow := 0;
  while (outrow < cinfo^.max_v_samp_factor) do
  begin
    for v := 0 to pred(2) do
    begin
      { inptr0 points to nearest input row, inptr1 points to next nearest }
      inptr0 := JSAMPLE_PTR(input_data^[inrow]);
      if (v = 0) then         { next nearest is row above }
      begin
        {inptr1 := JSAMPLE_PTR(input_data^[inrow-1]);}
        prev_input_data := input_data;       { work around }
        Dec(JSAMPROW_PTR(prev_input_data));  { negative offsets }
        inptr1 := JSAMPLE_PTR(prev_input_data^[inrow]);
      end
      else                    { next nearest is row below }
        inptr1 := JSAMPLE_PTR(input_data^[inrow+1]);
      outptr := JSAMPLE_PTR(output_data^[outrow]);
      Inc(outrow);

      { Special case for first column }
      thiscolsum := GETJSAMPLE(inptr0^) * 3 + GETJSAMPLE(inptr1^);
      Inc(inptr0);
      Inc(inptr1);
      nextcolsum := GETJSAMPLE(inptr0^) * 3 + GETJSAMPLE(inptr1^);
      Inc(inptr0);
      Inc(inptr1);

      outptr^ := JSAMPLE ((thiscolsum * 4 + 8) shr 4);
      Inc(outptr);
      outptr^ := JSAMPLE ((thiscolsum * 3 + nextcolsum + 7) shr 4);
      Inc(outptr);
      lastcolsum := thiscolsum; thiscolsum := nextcolsum;

      for colctr := pred(compptr^.downsampled_width - 2) downto 0 do
      begin
        { General case: 3/4 * nearer pixel + 1/4 * further pixel in each }
        { dimension, thus 9/16, 3/16, 3/16, 1/16 overall }
        nextcolsum := GETJSAMPLE(inptr0^) * 3 + GETJSAMPLE(inptr1^);
        Inc(inptr0);
        Inc(inptr1);
        outptr^ := JSAMPLE ((thiscolsum * 3 + lastcolsum + 8) shr 4);
        Inc(outptr);
        outptr^ := JSAMPLE ((thiscolsum * 3 + nextcolsum + 7) shr 4);
        Inc(outptr);
        lastcolsum := thiscolsum;
        thiscolsum := nextcolsum;
      end;

      { Special case for last column }
      outptr^ := JSAMPLE ((thiscolsum * 3 + lastcolsum + 8) shr 4);
      Inc(outptr);
      outptr^ := JSAMPLE ((thiscolsum * 4 + 7) shr 4);
      {Inc(outptr);                     - value never used }
    end;
    Inc(inrow);
  end;
end;


{ Module initialization routine for upsampling. }

{GLOBAL}
procedure jinit_upsampler (cinfo : j_decompress_ptr);
var
  upsample : my_upsample_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  need_buffer, do_fancy : boolean;
  h_in_group, v_in_group, h_out_group, v_out_group : int;
begin
  upsample := my_upsample_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(my_upsampler)) );
  cinfo^.upsample := jpeg_upsampler_ptr (upsample);
  upsample^.pub.start_pass := start_pass_upsample;
  upsample^.pub.upsample := sep_upsample;
  upsample^.pub.need_context_rows := FALSE; { until we find out differently }

  if (cinfo^.CCIR601_sampling)  then        { this isn't supported }
    ERREXIT(j_common_ptr(cinfo), JERR_CCIR601_NOTIMPL);

  { jdmainct.c doesn't support context rows when min_DCT_scaled_size := 1,
    so don't ask for it. }

  do_fancy := cinfo^.do_fancy_upsampling and (cinfo^.min_DCT_scaled_size > 1);

  { Verify we can handle the sampling factors, select per-component methods,
    and create storage as needed. }

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Compute size of an "input group" after IDCT scaling.  This many samples
      are to be converted to max_h_samp_factor * max_v_samp_factor pixels. }

    h_in_group := (compptr^.h_samp_factor * compptr^.DCT_scaled_size) div
                 cinfo^.min_DCT_scaled_size;
    v_in_group := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
                 cinfo^.min_DCT_scaled_size;
    h_out_group := cinfo^.max_h_samp_factor;
    v_out_group := cinfo^.max_v_samp_factor;
    upsample^.rowgroup_height[ci] := v_in_group; { save for use later }
    need_buffer := TRUE;
    if (not compptr^.component_needed) then
    begin
      { Don't bother to upsample an uninteresting component. }
      upsample^.methods[ci] := noop_upsample;
      need_buffer := FALSE;
    end
    else
      if (h_in_group = h_out_group) and (v_in_group = v_out_group) then
      begin
        { Fullsize components can be processed without any work. }
        upsample^.methods[ci] := fullsize_upsample;
        need_buffer := FALSE;
      end
      else
        if (h_in_group * 2 = h_out_group) and
                 (v_in_group = v_out_group) then
        begin
        { Special cases for 2h1v upsampling }
          if (do_fancy) and (compptr^.downsampled_width > 2) then
            upsample^.methods[ci] := h2v1_fancy_upsample
          else
            upsample^.methods[ci] := h2v1_upsample;
        end
        else
          if (h_in_group * 2 = h_out_group) and
                   (v_in_group * 2 = v_out_group) then
          begin
            { Special cases for 2h2v upsampling }
            if (do_fancy) and (compptr^.downsampled_width > 2) then
            begin
              upsample^.methods[ci] := h2v2_fancy_upsample;
              upsample^.pub.need_context_rows := TRUE;
            end
            else
              upsample^.methods[ci] := h2v2_upsample;
          end
          else
            if ((h_out_group mod h_in_group) = 0) and
                     ((v_out_group mod v_in_group) = 0) then
            begin
              { Generic integral-factors upsampling method }
              upsample^.methods[ci] := int_upsample;
              upsample^.h_expand[ci] := UINT8 (h_out_group div h_in_group);
              upsample^.v_expand[ci] := UINT8 (v_out_group div v_in_group);
            end
            else
              ERREXIT(j_common_ptr(cinfo), JERR_FRACT_SAMPLE_NOTIMPL);
    if (need_buffer) then
    begin
      upsample^.color_buf[ci] := cinfo^.mem^.alloc_sarray
        (j_common_ptr(cinfo), JPOOL_IMAGE,
         JDIMENSION (jround_up( long (cinfo^.output_width),
                                long (cinfo^.max_h_samp_factor))),
         JDIMENSION (cinfo^.max_v_samp_factor));
    end;
    Inc(compptr);
  end;
end;

end.
