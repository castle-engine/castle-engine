unit imjcprepct;

{ Original : jcprepct.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file contains the compression preprocessing controller.
  This controller manages the color conversion, downsampling,
  and edge expansion steps.

  Most of the complexity here is associated with buffering input rows
  as required by the downsampler.  See the comments at the head of
  jcsample.c for the downsampler's needs. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjpeglib,
  imjdeferr,
  imjerror,
  imjinclude,
  imjutils;

{GLOBAL}
procedure jinit_c_prep_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);

implementation


{ At present, jcsample.c can request context rows only for smoothing.
  In the future, we might also need context rows for CCIR601 sampling
  or other more-complex downsampling procedures.  The code to support
  context rows should be compiled only if needed. }

{$ifdef INPUT_SMOOTHING_SUPPORTED}
  {$define CONTEXT_ROWS_SUPPORTED}
{$endif}


{ For the simple (no-context-row) case, we just need to buffer one
  row group's worth of pixels for the downsampling step.  At the bottom of
  the image, we pad to a full row group by replicating the last pixel row.
  The downsampler's last output row is then replicated if needed to pad
  out to a full iMCU row.

  When providing context rows, we must buffer three row groups' worth of
  pixels.  Three row groups are physically allocated, but the row pointer
  arrays are made five row groups high, with the extra pointers above and
  below "wrapping around" to point to the last and first real row groups.
  This allows the downsampler to access the proper context rows.
  At the top and bottom of the image, we create dummy context rows by
  copying the first or last real pixel row.  This copying could be avoided
  by pointer hacking as is done in jdmainct.c, but it doesn't seem worth the
  trouble on the compression side. }


{ Private buffer controller object }

type
  my_prep_ptr = ^my_prep_controller;
  my_prep_controller = record
    pub : jpeg_c_prep_controller; { public fields }

    { Downsampling input buffer.  This buffer holds color-converted data
      until we have enough to do a downsample step.  }

    color_buf : array[0..MAX_COMPONENTS-1] of JSAMPARRAY;

    rows_to_go : JDIMENSION;	{ counts rows remaining in source image }
    next_buf_row : int;		{ index of next row to store in color_buf }

  {$ifdef CONTEXT_ROWS_SUPPORTED}	{ only needed for context case }
    this_row_group : int;         { starting row index of group to process }
    next_buf_stop : int;	        { downsample when we reach this index }
  {$endif}
  end; {my_prep_controller;}


{ Initialize for a processing pass. }

{METHODDEF}
procedure start_pass_prep (cinfo : j_compress_ptr;
                           pass_mode : J_BUF_MODE );  
var
  prep : my_prep_ptr;
begin
  prep := my_prep_ptr (cinfo^.prep);

  if (pass_mode <> JBUF_PASS_THRU) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);

  { Initialize total-height counter for detecting bottom of image }
  prep^.rows_to_go := cinfo^.image_height;
  { Mark the conversion buffer empty }
  prep^.next_buf_row := 0;
{$ifdef CONTEXT_ROWS_SUPPORTED}
  { Preset additional state variables for context mode.
    These aren't used in non-context mode, so we needn't test which mode. }
  prep^.this_row_group := 0;
  { Set next_buf_stop to stop after two row groups have been read in. }
  prep^.next_buf_stop := 2 * cinfo^.max_v_samp_factor;
{$endif}
end;


{ Expand an image vertically from height input_rows to height output_rows,
  by duplicating the bottom row. }

{LOCAL}
procedure expand_bottom_edge (image_data : JSAMPARRAY;
                              num_cols : JDIMENSION;
		              input_rows : int;
                              output_rows : int);
var
  {register} row : int;
begin
  for row := input_rows to pred(output_rows) do
  begin
    jcopy_sample_rows(image_data, input_rows-1, image_data, row,
		      1, num_cols);
  end;
end;


{ Process some data in the simple no-context case.

  Preprocessor output data is counted in "row groups".  A row group
  is defined to be v_samp_factor sample rows of each component.
  Downsampling will produce this much data from each max_v_samp_factor
  input rows. }

{METHODDEF}
procedure pre_process_data (cinfo : j_compress_ptr;
		           input_buf : JSAMPARRAY;
                           var in_row_ctr : JDIMENSION;
		           in_rows_avail : JDIMENSION;
		           output_buf : JSAMPIMAGE;
                           var out_row_group_ctr : JDIMENSION;
                           out_row_groups_avail : JDIMENSION);  
var
  prep : my_prep_ptr;
  numrows, ci : int;
  inrows : JDIMENSION;
  compptr : jpeg_component_info_ptr;
var
  local_input_buf : JSAMPARRAY;
begin
  prep := my_prep_ptr (cinfo^.prep);

  while (in_row_ctr < in_rows_avail) and
	(out_row_group_ctr < out_row_groups_avail) do
  begin
    { Do color conversion to fill the conversion buffer. }
    inrows := in_rows_avail - in_row_ctr;
    numrows := cinfo^.max_v_samp_factor - prep^.next_buf_row;
    {numrows := int( MIN(JDIMENSION(numrows), inrows) );}
    if inrows < JDIMENSION(numrows) then
      numrows := int(inrows);
    local_input_buf := JSAMPARRAY(@(input_buf^[in_row_ctr]));
    cinfo^.cconvert^.color_convert (cinfo, local_input_buf,
                                    JSAMPIMAGE(@prep^.color_buf),
				    JDIMENSION(prep^.next_buf_row),
				    numrows);
    Inc(in_row_ctr, numrows);
    Inc(prep^.next_buf_row, numrows);
    Dec(prep^.rows_to_go, numrows);
    { If at bottom of image, pad to fill the conversion buffer. }
    if (prep^.rows_to_go = 0) and
       (prep^.next_buf_row < cinfo^.max_v_samp_factor) then
    begin
      for ci := 0 to pred(cinfo^.num_components) do
      begin
	expand_bottom_edge(prep^.color_buf[ci], cinfo^.image_width,
			   prep^.next_buf_row, cinfo^.max_v_samp_factor);
      end;
      prep^.next_buf_row := cinfo^.max_v_samp_factor;
    end;
    { If we've filled the conversion buffer, empty it. }
    if (prep^.next_buf_row = cinfo^.max_v_samp_factor) then
    begin
      cinfo^.downsample^.downsample (cinfo,
                                     JSAMPIMAGE(@prep^.color_buf),
                                     JDIMENSION (0),
				     output_buf,
                                     out_row_group_ctr);
      prep^.next_buf_row := 0;
      Inc(out_row_group_ctr);;
    end;
    { If at bottom of image, pad the output to a full iMCU height.
      Note we assume the caller is providing a one-iMCU-height output buffer! }
    if (prep^.rows_to_go = 0) and
       (out_row_group_ctr < out_row_groups_avail) then
    begin
      compptr := jpeg_component_info_ptr(cinfo^.comp_info);
      for ci := 0 to pred(cinfo^.num_components) do
      begin
	expand_bottom_edge(output_buf^[ci],
		   compptr^.width_in_blocks * DCTSIZE,
                   int (out_row_group_ctr) * compptr^.v_samp_factor,
		   int (out_row_groups_avail) * compptr^.v_samp_factor);
        Inc(compptr);
      end;
      out_row_group_ctr := out_row_groups_avail;
      break;			{ can exit outer loop without test }
    end;
  end;
end;


{$ifdef CONTEXT_ROWS_SUPPORTED}

{ Process some data in the context case. }

{METHODDEF}
procedure pre_process_context (cinfo : j_compress_ptr;
		              input_buf : JSAMPARRAY;
                              var in_row_ctr : JDIMENSION;
		              in_rows_avail : JDIMENSION;
		              output_buf : JSAMPIMAGE;
                              var out_row_group_ctr : JDIMENSION;
		              out_row_groups_avail : JDIMENSION);  
var
  prep : my_prep_ptr;
  numrows, ci : int;
  buf_height : int;
  inrows : JDIMENSION;
var
  row : int;

begin
  prep := my_prep_ptr (cinfo^.prep);
  buf_height := cinfo^.max_v_samp_factor * 3;

  while (out_row_group_ctr < out_row_groups_avail) do
  begin
    if (in_row_ctr < in_rows_avail) then
    begin
      { Do color conversion to fill the conversion buffer. }
      inrows := in_rows_avail - in_row_ctr;
      numrows := prep^.next_buf_stop - prep^.next_buf_row;
      {numrows := int ( MIN( JDIMENSION(numrows), inrows) );}
      if inrows < JDIMENSION(numrows) then
        numrows := int(inrows);
      cinfo^.cconvert^.color_convert (cinfo,
                                      JSAMPARRAY(@input_buf^[in_row_ctr]),
                                      JSAMPIMAGE(@prep^.color_buf),
				      JDIMENSION (prep^.next_buf_row),
				      numrows);
      { Pad at top of image, if first time through }
      if (prep^.rows_to_go = cinfo^.image_height) then
      begin
	for ci := 0 to pred(cinfo^.num_components) do
        begin
	  for row := 1 to cinfo^.max_v_samp_factor do
          begin
	    jcopy_sample_rows(prep^.color_buf[ci], 0,
			      prep^.color_buf[ci], -row,
			      1, cinfo^.image_width);
	  end;
	end;
      end;
      Inc(in_row_ctr, numrows);
      Inc(prep^.next_buf_row, numrows);
      Dec(prep^.rows_to_go, numrows);
    end
    else
    begin
      { Return for more data, unless we are at the bottom of the image. }
      if (prep^.rows_to_go <> 0) then
	break;
      { When at bottom of image, pad to fill the conversion buffer. }
      if (prep^.next_buf_row < prep^.next_buf_stop) then
      begin
	for ci := 0 to pred(cinfo^.num_components) do
        begin
	  expand_bottom_edge(prep^.color_buf[ci], cinfo^.image_width,
			     prep^.next_buf_row, prep^.next_buf_stop);
	end;
	prep^.next_buf_row := prep^.next_buf_stop;
      end;
    end;
    { If we've gotten enough data, downsample a row group. }
    if (prep^.next_buf_row = prep^.next_buf_stop) then
    begin
      cinfo^.downsample^.downsample (cinfo,
                                     JSAMPIMAGE(@prep^.color_buf),
				     JDIMENSION(prep^.this_row_group),
				     output_buf,
                                     out_row_group_ctr);
      Inc(out_row_group_ctr);
      { Advance pointers with wraparound as necessary. }
      Inc(prep^.this_row_group, cinfo^.max_v_samp_factor);
      if (prep^.this_row_group >= buf_height) then
	prep^.this_row_group := 0;
      if (prep^.next_buf_row >= buf_height) then
	prep^.next_buf_row := 0;
      prep^.next_buf_stop := prep^.next_buf_row + cinfo^.max_v_samp_factor;
    end;
  end;
end;


{ Create the wrapped-around downsampling input buffer needed for context mode. }

{LOCAL}
procedure create_context_buffer (cinfo : j_compress_ptr);
var
  prep : my_prep_ptr;
  rgroup_height : int;
  ci, i : int;
  compptr : jpeg_component_info_ptr;
  true_buffer, fake_buffer : JSAMPARRAY;
begin
  prep := my_prep_ptr (cinfo^.prep);
  rgroup_height := cinfo^.max_v_samp_factor;
  { Grab enough space for fake row pointers for all the components;
    we need five row groups' worth of pointers for each component. }

  fake_buffer := JSAMPARRAY(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				(cinfo^.num_components * 5 * rgroup_height) *
				SIZEOF(JSAMPROW)) );

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Allocate the actual buffer space (3 row groups) for this component.
      We make the buffer wide enough to allow the downsampler to edge-expand
      horizontally within the buffer, if it so chooses. }
    true_buffer := cinfo^.mem^.alloc_sarray
      (j_common_ptr(cinfo), JPOOL_IMAGE,
       JDIMENSION (( long(compptr^.width_in_blocks) * DCTSIZE *
		      cinfo^.max_h_samp_factor) div compptr^.h_samp_factor),
       JDIMENSION (3 * rgroup_height));
    { Copy true buffer row pointers into the middle of the fake row array }
    MEMCOPY(JSAMPARRAY(@ fake_buffer^[rgroup_height]), true_buffer,
	    3 * rgroup_height * SIZEOF(JSAMPROW));
    { Fill in the above and below wraparound pointers }
    for i := 0 to pred(rgroup_height) do
    begin
      fake_buffer^[i] := true_buffer^[2 * rgroup_height + i];
      fake_buffer^[4 * rgroup_height + i] := true_buffer^[i];
    end;
    prep^.color_buf[ci] := JSAMPARRAY(@ fake_buffer^[rgroup_height]);
    Inc(JSAMPROW_PTR(fake_buffer), 5 * rgroup_height); { point to space for next component }
    Inc(compptr);
  end;
end;

{$endif} { CONTEXT_ROWS_SUPPORTED }


{ Initialize preprocessing controller. }

{GLOBAL}
procedure jinit_c_prep_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);
var
  prep : my_prep_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
begin

  if (need_full_buffer)	then    { safety check }
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);

  prep := my_prep_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_prep_controller)) );
  cinfo^.prep := jpeg_c_prep_controller_ptr(prep);
  prep^.pub.start_pass := start_pass_prep;

  { Allocate the color conversion buffer.
    We make the buffer wide enough to allow the downsampler to edge-expand
    horizontally within the buffer, if it so chooses. }

  if (cinfo^.downsample^.need_context_rows) then
  begin
    { Set up to provide context rows }
{$ifdef CONTEXT_ROWS_SUPPORTED}
    prep^.pub.pre_process_data := pre_process_context;
    create_context_buffer(cinfo);
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    { No context, just make it tall enough for one row group }
    prep^.pub.pre_process_data := pre_process_data;
    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      prep^.color_buf[ci] := cinfo^.mem^.alloc_sarray
	(j_common_ptr(cinfo), JPOOL_IMAGE,
	 JDIMENSION (( long(compptr^.width_in_blocks) * DCTSIZE *
			cinfo^.max_h_samp_factor) div compptr^.h_samp_factor),
	 JDIMENSION(cinfo^.max_v_samp_factor) );
      Inc(compptr);
    end;
  end;
end;

end.
