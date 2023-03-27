Unit JcSample;

{ This file contains downsampling routines.

  Downsampling input data is counted in "row groups".  A row group
  is defined to be max_v_samp_factor pixel rows of each component,
  from which the downsampler produces v_samp_factor sample rows.
  A single row group is processed in each call to the downsampler module.

  The downsampler is responsible for edge-expansion of its output data
  to fill an integral number of DCT blocks horizontally.  The source buffer
  may be modified if it is helpful for this purpose (the source buffer is
  allocated wide enough to correspond to the desired output width).
  The caller (the prep controller) is responsible for vertical padding.

  The downsampler may request "context rows" by setting need_context_rows
  during startup.  In this case, the input arrays will contain at least
  one row group's worth of pixels above and below the passed-in data;
  the caller will create dummy rows at image top and bottom by replicating
  the first or last real pixel row.

  An excellent reference for image resampling is
    Digital Image Warping, George Wolberg, 1990.
    Pub. by IEEE Computer Society Press, Los Alamitos, CA. ISBN 0-8186-8944-7.

  The downsampling algorithm used here is a simple average of the source
  pixels covered by the output pixel.  The hi-falutin sampling literature
  refers to this as a "box filter".  In general the characteristics of a box
  filter are not very good, but for the specific cases we normally use (1:1
  and 2:1 ratios) the box is equivalent to a "triangle filter" which is not
  nearly so bad.  If you intend to use other sampling ratios, you'd be well
  advised to improve this code.

  A simple input-smoothing capability is provided.  This is mainly intended
  for cleaning up color-dithered GIF input files (if you find it inadequate,
  we suggest using an external filtering program such as pnmconvol).  When
  enabled, each input pixel P is replaced by a weighted sum of itself and its
  eight neighbors.  P's weight is 1-8*SF and each neighbor's weight is SF,
  where SF := (smoothing_factor / 1024).
  Currently, smoothing is only supported for 2h2v sampling factors. }

{ Original: jcsample.c ; Copyright (C) 1991-1996, Thomas G. Lane. }


interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jutils,
  jdeferr,
  jerror,
  jpeglib;


{ Module initialization routine for downsampling.
  Note that we must select a routine for each component. }

{GLOBAL}
procedure jinit_downsampler (cinfo : j_compress_ptr);

implementation

{ Pointer to routine to downsample a single component }
type
  downsample1_ptr = procedure(cinfo : j_compress_ptr;
                              compptr : jpeg_component_info_ptr;
                              input_data : JSAMPARRAY;
                              output_data : JSAMPARRAY);

{ Private subobject }

type
  my_downsample_ptr = ^my_downsampler;
  my_downsampler = record
    pub : jpeg_downsampler;     { public fields }

    { Downsampling method pointers, one per component }
    methods : array[0..MAX_COMPONENTS-1] of downsample1_ptr;
  end;

{ Initialize for a downsampling pass. }

{METHODDEF}
procedure start_pass_downsample (cinfo : j_compress_ptr); far;
begin
  { no work for now }
end;


{ Expand a component horizontally from width input_cols to width output_cols,
  by duplicating the rightmost samples. }

{LOCAL}
procedure expand_right_edge (image_data : JSAMPARRAY;
                             num_rows : int;
                             input_cols : JDIMENSION;
                             output_cols : JDIMENSION);
var
  {register} ptr : JSAMPLE_PTR;
  {register} pixval : JSAMPLE;
  {register} count : int;
  row : int;
  numcols : int;
begin
  numcols := int (output_cols - input_cols);

  if (numcols > 0) then
  begin
    for row := 0 to pred(num_rows) do
    begin
      ptr := JSAMPLE_PTR(@(image_data^[row]^[input_cols-1]));
      pixval := ptr^;           { don't need GETJSAMPLE() here }
      for count := pred(numcols) downto 0 do
      begin
        Inc(ptr);
        ptr^ := pixval;
      end;
    end;
  end;
end;


{ Do downsampling for a whole row group (all components).

  In this version we simply downsample each component independently. }

{METHODDEF}
procedure sep_downsample (cinfo : j_compress_ptr;
                          input_buf : JSAMPIMAGE;
                          in_row_index : JDIMENSION;
                          output_buf : JSAMPIMAGE;
                          out_row_group_index : JDIMENSION); far;
var
  downsample : my_downsample_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  in_ptr, out_ptr : JSAMPARRAY;
begin
  downsample := my_downsample_ptr (cinfo^.downsample);

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    in_ptr := JSAMPARRAY(@ input_buf^[ci]^[in_row_index]);
    out_ptr := JSAMPARRAY(@ output_buf^[ci]^
                     [out_row_group_index * compptr^.v_samp_factor]);
    downsample^.methods[ci] (cinfo, compptr, in_ptr, out_ptr);
    Inc(compptr);
  end;
end;


{ Downsample pixel values of a single component.
  One row group is processed per call.
  This version handles arbitrary integral sampling ratios, without smoothing.
  Note that this version is not actually used for customary sampling ratios. }

{METHODDEF}
procedure int_downsample (cinfo : j_compress_ptr;
                          compptr : jpeg_component_info_ptr;
                          input_data : JSAMPARRAY;
                          output_data : JSAMPARRAY); far;
var
  inrow, outrow, h_expand, v_expand, numpix, numpix2, h, v : int;
  outcol, outcol_h :  JDIMENSION;       { outcol_h = outcol*h_expand }
  output_cols : JDIMENSION;
  inptr,
  outptr : JSAMPLE_PTR;
  outvalue : INT32;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  h_expand := cinfo^.max_h_samp_factor div compptr^.h_samp_factor;
  v_expand := cinfo^.max_v_samp_factor div compptr^.v_samp_factor;
  numpix := h_expand * v_expand;
  numpix2 := numpix div 2;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  expand_right_edge(input_data, cinfo^.max_v_samp_factor,
                    cinfo^.image_width, output_cols * h_expand);

  inrow := 0;
  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    outcol_h := 0;
    for outcol := 0 to pred(output_cols) do
    begin
      outvalue := 0;
      for v := 0 to pred(v_expand) do
      begin
        inptr := @(input_data^[inrow+v]^[outcol_h]);
        for h := 0 to pred(h_expand) do
        begin
          Inc(outvalue, INT32 (GETJSAMPLE(inptr^)) );
          Inc(inptr);
        end;
      end;
      outptr^ := JSAMPLE ((outvalue + numpix2) div numpix);
      Inc(outptr);
      Inc(outcol_h, h_expand);
    end;
    Inc(inrow, v_expand);
  end;
end;


{ Downsample pixel values of a single component.
  This version handles the special case of a full-size component,
  without smoothing. }

{METHODDEF}
procedure fullsize_downsample (cinfo : j_compress_ptr;
                               compptr : jpeg_component_info_ptr;
                               input_data : JSAMPARRAY;
                               output_data : JSAMPARRAY); far;
begin
  { Copy the data }
  jcopy_sample_rows(input_data, 0, output_data, 0,
                    cinfo^.max_v_samp_factor, cinfo^.image_width);
  { Edge-expand }
  expand_right_edge(output_data, cinfo^.max_v_samp_factor,
                    cinfo^.image_width, compptr^.width_in_blocks * DCTSIZE);
end;


{ Downsample pixel values of a single component.
  This version handles the common case of 2:1 horizontal and 1:1 vertical,
  without smoothing.

  A note about the "bias" calculations: when rounding fractional values to
  integer, we do not want to always round 0.5 up to the next integer.
  If we did that, we'd introduce a noticeable bias towards larger values.
  Instead, this code is arranged so that 0.5 will be rounded up or down at
  alternate pixel locations (a simple ordered dither pattern). }

{METHODDEF}
procedure h2v1_downsample (cinfo : j_compress_ptr;
                           compptr : jpeg_component_info_ptr;
                           input_data : JSAMPARRAY;
                           output_data : JSAMPARRAY); far;
var
  outrow : int;
  outcol : JDIMENSION;
  output_cols : JDIMENSION;
  {register} inptr, outptr : JSAMPLE_PTR;
  {register} bias : int;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  expand_right_edge(input_data, cinfo^.max_v_samp_factor,
                    cinfo^.image_width, output_cols * 2);

  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    inptr := JSAMPLE_PTR(input_data^[outrow]);
    bias := 0;               { bias := 0,1,0,1,... for successive samples }
    for outcol := 0 to pred(output_cols) do
    begin
      outptr^ := JSAMPLE ((GETJSAMPLE(inptr^) +
                           GETJSAMPLE(JSAMPROW(inptr)^[1]) + bias) shr 1);
      Inc(outptr);
      bias := bias xor 1;    { 0=>1, 1=>0 }
      Inc(inptr, 2);
    end;
  end;
end;


{ Downsample pixel values of a single component.
  This version handles the standard case of 2:1 horizontal and 2:1 vertical,
  without smoothing. }

{METHODDEF}
procedure h2v2_downsample (cinfo : j_compress_ptr;
                           compptr : jpeg_component_info_ptr;
                           input_data : JSAMPARRAY;
                           output_data : JSAMPARRAY); far;
var
  inrow, outrow : int;
  outcol : JDIMENSION;
  output_cols : JDIMENSION;
  {register} inptr0, inptr1, outptr : JSAMPLE_PTR;
  {register} bias : int;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  expand_right_edge(input_data, cinfo^.max_v_samp_factor,
                    cinfo^.image_width, output_cols * 2);

  inrow := 0;
  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    inptr0 := JSAMPLE_PTR(input_data^[inrow]);
    inptr1 := JSAMPLE_PTR(input_data^[inrow+1]);
    bias := 1;                  { bias := 1,2,1,2,... for successive samples }
    for outcol := 0 to pred(output_cols) do
    begin
      outptr^ := JSAMPLE ((GETJSAMPLE(inptr0^) +
                           GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
                           GETJSAMPLE(inptr1^) +
                           GETJSAMPLE(JSAMPROW(inptr1)^[1]) + bias) shr 2);
      Inc(outptr);
      bias := bias xor 3;       { 1=>2, 2=>1 }
      Inc(inptr0, 2);
      Inc(inptr1, 2);
    end;
    Inc(inrow, 2);
  end;
end;


{$ifdef INPUT_SMOOTHING_SUPPORTED}

{ Downsample pixel values of a single component.
  This version handles the standard case of 2:1 horizontal and 2:1 vertical,
  with smoothing.  One row of context is required. }

{METHODDEF}
procedure h2v2_smooth_downsample (cinfo : j_compress_ptr;
                                  compptr : jpeg_component_info_ptr;
                                  input_data : JSAMPARRAY;
                                  output_data : JSAMPARRAY); far;
var
  inrow, outrow : int;
  colctr : JDIMENSION;
  output_cols : JDIMENSION;
  {register} inptr0, inptr1, above_ptr, below_ptr, outptr : JSAMPLE_PTR;
  membersum, neighsum, memberscale, neighscale : INT32;
var
  prev_input_data : JSAMPARRAY;
  prev_inptr0, prev_inptr1, prev_above_ptr, prev_below_ptr : JSAMPLE_PTR;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  prev_input_data := input_data;
  Dec(JSAMPROW_PTR(prev_input_data));
  expand_right_edge(prev_input_data, cinfo^.max_v_samp_factor + 2,
                    cinfo^.image_width, output_cols * 2);

  { We don't bother to form the individual "smoothed" input pixel values;
    we can directly compute the output which is the average of the four
    smoothed values.  Each of the four member pixels contributes a fraction
    (1-8*SF) to its own smoothed image and a fraction SF to each of the three
    other smoothed pixels, therefore a total fraction (1-5*SF)/4 to the final
    output.  The four corner-adjacent neighbor pixels contribute a fraction
    SF to just one smoothed pixel, or SF/4 to the final output; while the
    eight edge-adjacent neighbors contribute SF to each of two smoothed
    pixels, or SF/2 overall.  In order to use integer arithmetic, these
    factors are scaled by 2^16 := 65536.
    Also recall that SF := smoothing_factor / 1024. }

  memberscale := 16384 - cinfo^.smoothing_factor * 80; { scaled (1-5*SF)/4 }
  neighscale := cinfo^.smoothing_factor * 16; { scaled SF/4 }

  inrow := 0;
  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    inptr0 := JSAMPLE_PTR(input_data^[inrow]);
    inptr1 := JSAMPLE_PTR(input_data^[inrow+1]);
    above_ptr := JSAMPLE_PTR(input_data^[inrow-1]);
    below_ptr := JSAMPLE_PTR(input_data^[inrow+2]);

    { Special case for first column: pretend column -1 is same as column 0 }
    membersum := GETJSAMPLE(inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
                GETJSAMPLE(inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[1]);
    neighsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(JSAMPROW(above_ptr)^[1]) +
               GETJSAMPLE(below_ptr^) + GETJSAMPLE(JSAMPROW(below_ptr)^[1]) +
               GETJSAMPLE(inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[2]) +
               GETJSAMPLE(inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[2]);
    Inc(neighsum, neighsum);
    Inc(neighsum, GETJSAMPLE(above_ptr^) +
                  GETJSAMPLE(JSAMPROW(above_ptr)^[2]) +
                  GETJSAMPLE(below_ptr^) +
                  GETJSAMPLE(JSAMPROW(below_ptr)^[2]) );
    membersum := membersum * memberscale + neighsum * neighscale;
    outptr^ := JSAMPLE ((membersum + 32768) shr 16);
    Inc(outptr);
    prev_inptr0 := inptr0;
    prev_inptr1 := inptr1;
    Inc(prev_inptr0);
    Inc(prev_inptr1);
    Inc(inptr0, 2);
    Inc(inptr1, 2);
    prev_above_ptr := above_ptr;
    prev_below_ptr := below_ptr;
    Inc(above_ptr, 2);
    Inc(below_ptr, 2);
    Inc(prev_above_ptr, 1);
    Inc(prev_below_ptr, 1);

    for colctr := pred(output_cols - 2) downto 0 do
    begin
      { sum of pixels directly mapped to this output element }
      membersum := GETJSAMPLE(inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
                   GETJSAMPLE(inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[1]);
      { sum of edge-neighbor pixels }
      neighsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(JSAMPROW(above_ptr)^[1]) +
                  GETJSAMPLE(below_ptr^) + GETJSAMPLE(JSAMPROW(below_ptr)^[1]) +
                  GETJSAMPLE(prev_inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[2]) +
                  GETJSAMPLE(prev_inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[2]);
      { The edge-neighbors count twice as much as corner-neighbors }
      Inc(neighsum, neighsum);
      { Add in the corner-neighbors }
      Inc(neighsum, GETJSAMPLE(prev_above_ptr^) +
                    GETJSAMPLE(JSAMPROW(above_ptr)^[2]) +
                    GETJSAMPLE(prev_below_ptr^) +
                    GETJSAMPLE(JSAMPROW(below_ptr)^[2]) );
      { form final output scaled up by 2^16 }
      membersum := membersum * memberscale + neighsum * neighscale;
      { round, descale and output it }
      outptr^ := JSAMPLE ((membersum + 32768) shr 16);
      Inc(outptr);
      Inc(inptr0, 2);
      Inc(inptr1, 2);
      Inc(prev_inptr0, 2);
      Inc(prev_inptr1, 2);
      Inc(above_ptr, 2);
      Inc(below_ptr, 2);
      Inc(prev_above_ptr, 2);
      Inc(prev_below_ptr, 2);
    end;

    { Special case for last column }
    membersum := GETJSAMPLE(inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
                 GETJSAMPLE(inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[1]);
    neighsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(JSAMPROW(above_ptr)^[1]) +
                GETJSAMPLE(below_ptr^) + GETJSAMPLE(JSAMPROW(below_ptr)^[1]) +
                GETJSAMPLE(prev_inptr0^) + GETJSAMPLE(JSAMPROW(inptr0)^[1]) +
                GETJSAMPLE(prev_inptr1^) + GETJSAMPLE(JSAMPROW(inptr1)^[1]);
    Inc(neighsum, neighsum);
    Inc(neighsum, GETJSAMPLE(prev_above_ptr^) +
                  GETJSAMPLE(JSAMPROW(above_ptr)^[1]) +
                  GETJSAMPLE(prev_below_ptr^) +
                  GETJSAMPLE(JSAMPROW(below_ptr)^[1]) );
    membersum := membersum * memberscale + neighsum * neighscale;
    outptr^ := JSAMPLE ((membersum + 32768) shr 16);

    Inc(inrow, 2);
  end;
end;


{ Downsample pixel values of a single component.
  This version handles the special case of a full-size component,
  with smoothing.  One row of context is required. }

{METHODDEF}
procedure fullsize_smooth_downsample (cinfo : j_compress_ptr;
                                      compptr : jpeg_component_info_ptr;
                                      input_data : JSAMPARRAY;
                                      output_data : JSAMPARRAY); far;
var
  outrow : int;
  colctr : JDIMENSION;
  output_cols : JDIMENSION;
  {register} inptr, above_ptr, below_ptr, outptr : JSAMPLE_PTR;
  membersum, neighsum, memberscale, neighscale : INT32;
  colsum, lastcolsum, nextcolsum : int;
var
  prev_input_data : JSAMPARRAY;
begin
  output_cols := compptr^.width_in_blocks * DCTSIZE;

  { Expand input data enough to let all the output samples be generated
    by the standard loop.  Special-casing padded output would be more
    efficient. }

  prev_input_data := input_data;
  Dec(JSAMPROW_PTR(prev_input_data));
  expand_right_edge(prev_input_data, cinfo^.max_v_samp_factor + 2,
                    cinfo^.image_width, output_cols);

  { Each of the eight neighbor pixels contributes a fraction SF to the
    smoothed pixel, while the main pixel contributes (1-8*SF).  In order
    to use integer arithmetic, these factors are multiplied by 2^16 := 65536.
    Also recall that SF := smoothing_factor / 1024. }

  memberscale := long(65536) - cinfo^.smoothing_factor * long(512); { scaled 1-8*SF }
  neighscale := cinfo^.smoothing_factor * 64; { scaled SF }

  for outrow := 0 to pred(compptr^.v_samp_factor) do
  begin
    outptr := JSAMPLE_PTR(output_data^[outrow]);
    inptr := JSAMPLE_PTR(input_data^[outrow]);
    above_ptr := JSAMPLE_PTR(input_data^[outrow-1]);
    below_ptr := JSAMPLE_PTR(input_data^[outrow+1]);

    { Special case for first column }
    colsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(below_ptr^) +
             GETJSAMPLE(inptr^);
    Inc(above_ptr);
    Inc(below_ptr);
    membersum := GETJSAMPLE(inptr^);
    Inc(inptr);
    nextcolsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(below_ptr^) +
                  GETJSAMPLE(inptr^);
    neighsum := colsum + (colsum - membersum) + nextcolsum;
    membersum := membersum * memberscale + neighsum * neighscale;
    outptr^ := JSAMPLE ((membersum + 32768) shr 16);
    Inc(outptr);
    lastcolsum := colsum; colsum := nextcolsum;

    for colctr := pred(output_cols - 2) downto 0 do
    begin
      membersum := GETJSAMPLE(inptr^);
      Inc(inptr);
      Inc(above_ptr);
      Inc(below_ptr);
      nextcolsum := GETJSAMPLE(above_ptr^) + GETJSAMPLE(below_ptr^) +
                    GETJSAMPLE(inptr^);
      neighsum := lastcolsum + (colsum - membersum) + nextcolsum;
      membersum := membersum * memberscale + neighsum * neighscale;
      outptr^ := JSAMPLE ((membersum + 32768) shr 16);
      Inc(outptr);
      lastcolsum := colsum; colsum := nextcolsum;
    end;

    { Special case for last column }
    membersum := GETJSAMPLE(inptr^);
    neighsum := lastcolsum + (colsum - membersum) + colsum;
    membersum := membersum * memberscale + neighsum * neighscale;
    outptr^ := JSAMPLE ((membersum + 32768) shr 16);
  end;
end;

{$endif} { INPUT_SMOOTHING_SUPPORTED }


{ Module initialization routine for downsampling.
  Note that we must select a routine for each component. }

{GLOBAL}
procedure jinit_downsampler (cinfo : j_compress_ptr);
var
  downsample : my_downsample_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  smoothok : boolean;
begin
  smoothok := TRUE;

  downsample := my_downsample_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(my_downsampler)) );
  cinfo^.downsample := jpeg_downsampler_ptr (downsample);
  downsample^.pub.start_pass := start_pass_downsample;
  downsample^.pub.downsample := sep_downsample;
  downsample^.pub.need_context_rows := FALSE;

  if (cinfo^.CCIR601_sampling) then
    ERREXIT(j_common_ptr(cinfo), JERR_CCIR601_NOTIMPL);

  { Verify we can handle the sampling factors, and set up method pointers }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    if (compptr^.h_samp_factor = cinfo^.max_h_samp_factor) and
       (compptr^.v_samp_factor = cinfo^.max_v_samp_factor) then
    begin
{$ifdef INPUT_SMOOTHING_SUPPORTED}
      if (cinfo^.smoothing_factor <> 0) then
      begin
        downsample^.methods[ci] := fullsize_smooth_downsample;
        downsample^.pub.need_context_rows := TRUE;
      end
      else
{$endif}
        downsample^.methods[ci] := fullsize_downsample;
    end
    else
      if (compptr^.h_samp_factor * 2 = cinfo^.max_h_samp_factor) and
         (compptr^.v_samp_factor = cinfo^.max_v_samp_factor) then
      begin
        smoothok := FALSE;
        downsample^.methods[ci] := h2v1_downsample;
      end
      else
        if (compptr^.h_samp_factor * 2 = cinfo^.max_h_samp_factor) and
           (compptr^.v_samp_factor * 2 = cinfo^.max_v_samp_factor) then
        begin
  {$ifdef INPUT_SMOOTHING_SUPPORTED}
        if (cinfo^.smoothing_factor <> 0) then
        begin
          downsample^.methods[ci] := h2v2_smooth_downsample;
          downsample^.pub.need_context_rows := TRUE;
        end
        else
  {$endif}
          downsample^.methods[ci] := h2v2_downsample;
        end
        else
          if ((cinfo^.max_h_samp_factor mod compptr^.h_samp_factor) = 0) and
             ((cinfo^.max_v_samp_factor mod compptr^.v_samp_factor) = 0) then
          begin
            smoothok := FALSE;
            downsample^.methods[ci] := int_downsample;
          end
          else
            ERREXIT(j_common_ptr(cinfo), JERR_FRACT_SAMPLE_NOTIMPL);
    Inc(compptr);
  end;

{$ifdef INPUT_SMOOTHING_SUPPORTED}
  if (cinfo^.smoothing_factor <> 0) and (not smoothok) then
    TRACEMS(j_common_ptr(cinfo), 0, JTRC_SMOOTH_NOTIMPL);
{$endif}
end;

end.
